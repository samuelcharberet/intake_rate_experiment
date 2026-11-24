## Talk figures for "How much of a meal becomes the next meal?"
## Re-plots the R2 model curves stored in the targets store (models_irn, reference families only).
## Run from the project root after tar_make(); writes PNGs to 4_outputs/talk_figures/.
## Kept outside R/ on purpose: tar_source() would otherwise run it during tar_make().
## Extra packages: ggrepel, maps, mapproj (locator map).
suppressMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(ggrepel); library(scales)
})

out <- here::here("4_outputs", "talk_figures")
dir.create(out, showWarnings = FALSE, recursive = TRUE)

m  <- targets::tar_read(models_irn)
di <- targets::tar_read(data_irn_indivuals_combined)
dg <- targets::tar_read(data_irn_groups_combined)

spec <- m$specs %>% select(id, response_label, element, term_label)
curves <- m$curves %>% filter(reference) %>% left_join(spec, by = "id")
pts <- m$points %>% left_join(spec, by = "id")

## ---- style ---------------------------------------------------------------
FONT <- "Liberation Sans"
INK <- "#1F1F1F"; CURVE <- "#2B5C8A"; RIBBON <- "#C9D6E3"; HIGHLIGHT <- "#F2B705"
greens <- c("120" = "#C7E9C0", "240" = "#A1D99B", "360" = "#74C476", "480" = "#31A354", "900" = "#006D2C")

theme_talk <- function(base = 20) {
  theme_classic(base_size = base, base_family = FONT) +
    theme(
      axis.line = element_line(colour = INK, linewidth = 0.7),
      axis.ticks = element_line(colour = INK, linewidth = 0.6),
      axis.text = element_text(colour = INK, size = base * 0.85),
      axis.title = element_text(colour = INK, size = base),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.title.x = element_text(margin = margin(t = 10)),
      legend.position = "none",
      plot.background = element_rect(fill = "transparent", colour = NA),
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.margin = margin(12, 18, 8, 8)
    )
}
save <- function(p, name, w = 8, h = 5.4) {
  ggsave(file.path(out, paste0(name, ".png")), p, width = w, height = h, dpi = 300, bg = "transparent")
}
lab_ir <- expression("Intake rate  (g"[food] %.% "g"[body]^{-1} %.% "day"^{-1}*")")
lab_gr <- expression("Growth rate  (g"[growth] %.% "g"[body]^{-1} %.% "day"^{-1}*")")

## ration of each individual (for green colouring)
ration <- di %>% transmute(x = mass_specific_ingestion_rate_dw, ration = factor(food_provided_fw)) %>% distinct()
ration_gr <- di %>% transmute(x = mean_growth_dw, ration = factor(food_provided_fw)) %>% distinct()

## ---- F1: intake gradient -------------------------------------------------
d1 <- di %>% filter(!is.na(mass_specific_ingestion_rate_dw)) %>% mutate(ration = factor(food_provided_fw))
p <- ggplot(d1, aes(ration, mass_specific_ingestion_rate_dw, fill = ration, colour = ration)) +
  geom_boxplot(width = 0.55, outlier.shape = NA, colour = INK, alpha = 0.9, linewidth = 0.6) +
  geom_jitter(width = 0.12, height = 0, size = 2.2, shape = 21, colour = INK, stroke = 0.3, alpha = 0.8) +
  scale_fill_manual(values = greens) + scale_colour_manual(values = greens) +
  scale_y_continuous(limits = c(0, 2), breaks = seq(0, 2, 0.5)) +
  labs(x = expression("Food given  (mg"[fresh] %.% "day"^{-1}*")"), y = lab_ir) +
  theme_talk()
save(p, "f01_intake_gradient", w = 7, h = 5.4)

## ---- generic curve plot --------------------------------------------------
curve_plot <- function(id_, ylab, pct = TRUE, xlab = lab_ir, show_curve = TRUE, show_pts = TRUE,
                       ylim = NULL, extra = NULL, colour_pts = ration) {
  cdat <- curves %>% filter(id == id_)
  pdat <- pts %>% filter(id == id_) %>% left_join(colour_pts, by = "x")
  k <- if (pct) 100 else 1
  g <- ggplot()
  if (show_pts) g <- g + geom_point(data = pdat, aes(x, y * k, fill = ration), shape = 21, colour = "grey30",
                                    stroke = 0.25, size = 2.4, alpha = if (show_curve) 0.55 else 0.85)
  if (show_curve) g <- g +
      geom_ribbon(data = cdat, aes(x, ymin = lower * k, ymax = upper * k), fill = RIBBON, alpha = 0.7) +
      geom_line(data = cdat, aes(x, fit * k), colour = CURVE, linewidth = 1.8)
  g <- g + scale_fill_manual(values = greens, na.value = "grey60") + labs(x = xlab, y = ylab) + theme_talk()
  if (!is.null(ylim)) g <- g + coord_cartesian(ylim = ylim)
  if (!is.null(extra)) g <- g + extra
  g
}

## ---- F2: assimilation efficiency ----------------------------------------
ae_c <- curves %>% filter(id == 1)
ae_lo <- ae_c$fit[which.min(ae_c$x)] * 100; ae_min <- min(ae_c$fit) * 100; x_min <- ae_c$x[which.min(ae_c$fit)]
yl <- c(33, 75)
save(curve_plot(1, "Assimilation efficiency  (%)", show_curve = FALSE, ylim = yl), "f02a_ae_points")
save(curve_plot(1, "Assimilation efficiency  (%)", ylim = yl, extra = list(
  annotate("point", x = min(ae_c$x), y = ae_lo, size = 5, colour = HIGHLIGHT),
  annotate("point", x = x_min, y = ae_min, size = 5, colour = HIGHLIGHT),
  annotate("text", x = min(ae_c$x) + 0.02, y = ae_lo + 5.5, label = sprintf("%.0f %%", ae_lo), family = FONT, size = 7.5, fontface = "bold", hjust = 0),
  annotate("text", x = x_min, y = ae_min - 6, label = sprintf("%.0f %%", ae_min), family = FONT, size = 7.5, fontface = "bold")
)), "f02b_ae_curve")

## ---- F3: growth efficiency vs intake ------------------------------------
ge_c <- curves %>% filter(id == 2)
pk <- ge_c[which.max(ge_c$fit), ]
yl <- c(18, 40)
save(curve_plot(2, "Growth efficiency  (%)", show_curve = FALSE, ylim = yl), "f03a_ge_points")
pk_layer <- list(
  annotate("segment", x = pk$x, xend = pk$x, y = -Inf, yend = pk$fit * 100, linetype = "22", colour = INK, linewidth = 0.6),
  annotate("point", x = pk$x, y = pk$fit * 100, size = 6, colour = HIGHLIGHT),
  annotate("text", x = pk$x + 0.05, y = pk$fit * 100 + 2.6, label = sprintf("%.0f %%", pk$fit * 100), family = FONT, size = 8, fontface = "bold", hjust = 0)
)
save(curve_plot(2, "Growth efficiency  (%)", ylim = yl, extra = pk_layer), "f03b_ge_curve")

## ---- F4: growth efficiency vs growth rate --------------------------------
gg_c <- curves %>% filter(id == 3); pk2 <- gg_c[which.max(gg_c$fit), ]
save(curve_plot(3, "Growth efficiency  (%)", xlab = lab_gr, ylim = yl, colour_pts = ration_gr, extra = list(
  annotate("segment", x = pk2$x, xend = pk2$x, y = -Inf, yend = pk2$fit * 100, linetype = "22", colour = INK, linewidth = 0.6),
  annotate("point", x = pk2$x, y = pk2$fit * 100, size = 6, colour = HIGHLIGHT),
  annotate("text", x = pk2$x + 0.012, y = pk2$fit * 100 + 2.6, label = sprintf("%.0f %%", pk2$fit * 100), family = FONT, size = 8, fontface = "bold", hjust = 0)
)), "f04_ge_growth")

## ---- element colours -----------------------------------------------------
el_cols <- c(C = "#1F1F1F", N = "#2E6FBF", P = "#E08A00", S = "#B59A00", K = "#7B4FA0", Na = "#D1495B", Mg = "#2E9E6A", Ca = "#8C6D46")

## ---- F5: elemental AE (curves + group points) --------------------------
eae <- curves %>% filter(response_label == "AE", !is.na(element))
epts <- pts %>% filter(response_label == "AE", !is.na(element))
ends <- eae %>% group_by(element) %>% slice_max(x, n = 1)
el_plot <- function(els, big = character()) {
  ggplot(filter(eae, element %in% els), aes(x, fit * 100, colour = element)) +
    geom_point(data = filter(epts, element %in% els), aes(x, y * 100, colour = element), size = 2.3, alpha = 0.55, stroke = 0) +
    geom_line(aes(linewidth = element %in% big)) +
    scale_linewidth_manual(values = c(`FALSE` = 1.5, `TRUE` = 2.6), guide = "none") +
    geom_text_repel(data = filter(ends, element %in% els), aes(label = element), family = FONT, size = 7, fontface = "bold",
                    direction = "y", nudge_x = 0.06, hjust = 0, segment.colour = "grey70", min.segment.length = 0,
                    box.padding = 0.15, seed = 1) +
    scale_colour_manual(values = el_cols) +
    scale_x_continuous(limits = c(0.45, 1.78), breaks = seq(0.6, 1.6, 0.2)) +
    scale_y_continuous(limits = c(20, 100), breaks = seq(20, 100, 20)) +
    labs(x = lab_ir, y = "Assimilation efficiency  (%)") + theme_talk()
}
save(el_plot(c("C", "N", "P")), "f05a_element_ae_CNP")
save(el_plot(names(el_cols), big = "Na"), "f05_element_ae")

## ---- F6: retention time --------------------------------------------------
rt <- curves %>% filter(response_label == "RT")
rt_ends <- rt %>% group_by(element) %>% slice_min(x, n = 1)
rt_ratio <- rt %>% group_by(element) %>% summarise(r = fit[which.min(x)] / fit[which.max(x)])
write.csv(rt_ratio, file.path(out, "rt_ratios.csv"), row.names = FALSE)
rtp <- pts %>% filter(response_label == "RT")
rt_base <- ggplot(rt, aes(x, fit, colour = element)) +
  geom_point(data = rtp, aes(x, y, colour = element), size = 2.3, alpha = 0.5, stroke = 0) +
  geom_line(aes(linewidth = element %in% c("Na", "K"))) +
  scale_linewidth_manual(values = c(`FALSE` = 1.2, `TRUE` = 2.4)) +
  geom_text_repel(data = rt_ends, aes(label = element), family = FONT, size = 6.5, fontface = "bold", direction = "y",
                  nudge_x = -0.07, hjust = 1, segment.colour = "grey70", min.segment.length = 0, box.padding = 0.12, seed = 2) +
  scale_colour_manual(values = el_cols) +
  scale_y_log10(breaks = c(1, 3, 10, 30, 100), labels = c("1", "3", "10", "30", "100"), limits = c(0.6, 200)) +
  scale_x_continuous(limits = c(0.3, 1.65), breaks = seq(0.6, 1.6, 0.2)) +
  labs(x = lab_ir, y = "Retention time  (days, log scale)") + theme_talk()
save(rt_base, "f06a_retention")
save(rt_base + annotate("rect", xmin = -Inf, xmax = Inf, ymin = 40, ymax = 60, fill = HIGHLIGHT, alpha = 0.25) +
       annotate("text", x = 1.62, y = 75, label = "life span: about 50 days", family = FONT, size = 6.5, hjust = 1, colour = INK),
     "f06b_retention_lifespan")

## ---- F7/F8: stoichiometric ratios (group level, linear fits as in Fig. 4) -----
st <- dg %>% filter(variable %in% c("larvae", "frass"), element %in% c("C", "N", "P")) %>%
  select(group_ID, variable, element, elemental_value, x = mean_mass_specific_intake_rate_dw, treatment_ID) %>%
  pivot_wider(names_from = element, values_from = elemental_value) %>%
  mutate(CN = C / N, CP = C / (P / 1e4))
ratio_panel <- function(v, ratio, ylab, col) {
  d <- st %>% filter(variable == v) %>% mutate(y = .data[[ratio]]) %>% filter(!is.na(y))
  ggplot(d, aes(x, y)) +
    geom_smooth(method = "lm", formula = y ~ x, colour = col, fill = col, alpha = 0.18, linewidth = 1.8) +
    geom_point(shape = 21, fill = col, colour = "grey20", size = 2.8, alpha = 0.75, stroke = 0.3) +
    scale_x_continuous(breaks = seq(0.6, 1.6, 0.4)) +
    labs(x = lab_ir, y = ylab) + theme_talk(19)
}
FRASS <- "#8B5A2B"; BODY <- "#3E7D3E"
save(ratio_panel("frass", "CN", "Frass C:N", FRASS), "f07a_frass_CN", w = 5.6, h = 5)
save(ratio_panel("frass", "CP", "Frass C:P", FRASS), "f07b_frass_CP", w = 5.6, h = 5)
save(ratio_panel("larvae", "CN", "Body C:N", BODY), "f08a_body_CN", w = 5.6, h = 5)
save(ratio_panel("larvae", "CP", "Body C:P", BODY), "f08b_body_CP", w = 5.6, h = 5)
fr <- st %>% group_by(variable) %>% summarise(
  cn_lo = predict(lm(CN ~ x), newdata = data.frame(x = min(x))), cn_hi = predict(lm(CN ~ x), newdata = data.frame(x = max(x))),
  cp_lo = predict(lm(CP ~ x), newdata = data.frame(x = min(x))), cp_hi = predict(lm(CP ~ x), newdata = data.frame(x = max(x))))
write.csv(fr, file.path(out, "ratio_endpoints.csv"), row.names = FALSE)

## ---- theory schematics -----------------------------------------------------
th <- tibble(I = seq(0, 2.6, length.out = 600)) %>% mutate(GE = 0.55 * (1 - exp(-3 * I)) * exp(-0.35 * I) - 0.12)
tpk <- th[which.max(th$GE), ]
th_base <- ggplot(th, aes(I, GE)) +
  geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.6, linetype = "22") +
  scale_x_continuous(limits = c(0, 2.65), expand = c(0, 0)) +
  coord_cartesian(ylim = c(-0.15, 0.32)) +
  labs(x = "Intake rate", y = "Growth efficiency") +
  theme_talk(22) + theme(axis.text = element_blank(), axis.ticks = element_blank(),
                         axis.line = element_line(arrow = arrow(length = unit(0.25, "cm"), type = "closed"), linewidth = 0.8))
TH <- "#C0392B"
save(th_base + geom_line(data = filter(th, I < 0.3), colour = TH, linewidth = 2.2), "t01a_theory_low")
save(th_base + geom_line(data = filter(th, I < 0.3), colour = TH, linewidth = 2.2) +
       geom_line(data = filter(th, I > 1.5), colour = TH, linewidth = 2.2), "t01b_theory_high")
save(th_base + geom_line(colour = TH, linewidth = 2.2) +
       annotate("point", x = tpk$I, y = tpk$GE, size = 6, colour = HIGHLIGHT), "t01c_theory_hump")
save(th_base +
       annotate("rect", xmin = 0.08, xmax = 0.5, ymin = -Inf, ymax = Inf, fill = "#5B8DB8", alpha = 0.18) +
       annotate("rect", xmin = 1.05, xmax = 2.5, ymin = -Inf, ymax = Inf, fill = "#5B8DB8", alpha = 0.18) +
       geom_line(colour = "grey75", linewidth = 2.2) +
       geom_line(data = filter(th, I >= 0.08, I <= 0.5), colour = TH, linewidth = 2.6) +
       geom_line(data = filter(th, I >= 1.05, I <= 2.5), colour = TH, linewidth = 2.6), "t02_theory_windows")

## ---- F9: back to Lindeman: data curve vs a constant ----------------------------
mean_ge <- mean(pts$y[pts$id == 2]) * 100
save(curve_plot(2, "Growth efficiency  (%)", show_pts = FALSE, ylim = c(18, 40), extra = list(
  annotate("segment", x = min(ge_c$x), xend = max(ge_c$x), y = mean_ge, yend = mean_ge, linetype = "42", colour = "grey35", linewidth = 1.1),
  annotate("text", x = max(ge_c$x), y = mean_ge + 1.3, label = "a constant efficiency", family = FONT, size = 6.5, hjust = 1, colour = "grey30"),
  annotate("point", x = pk$x, y = pk$fit * 100, size = 6, colour = HIGHLIGHT)
)), "f09_ge_vs_constant")

## ---- Backup: family sensitivity for GE ~ MSIR and AE ~ MSIR ---------------------
fam <- m$curves %>% filter(id %in% c(1, 2)) %>% left_join(spec, by = "id") %>%
  mutate(panel = ifelse(id == 1, "Assimilation efficiency", "Growth efficiency"))
p <- ggplot(fam, aes(x, fit * 100, colour = family, linetype = reference)) + geom_line(linewidth = 1.3) +
  facet_wrap(~panel, scales = "free_y") +
  scale_linetype_manual(values = c(`TRUE` = "solid", `FALSE` = "22"), guide = "none") +
  scale_colour_manual(values = c("#2B5C8A", "#D1495B", "#2E9E6A", "#E08A00", "#7B4FA0", "#8C6D46", "grey40")) +
  labs(x = lab_ir, y = "Efficiency  (%)", colour = NULL) + theme_talk(18) +
  theme(legend.position = "bottom", strip.background = element_blank(), strip.text = element_text(size = 18, family = FONT))
save(p, "b01_family_sensitivity", w = 11, h = 5.4)

## key numbers for the notes
cat(sprintf("AE low-intake %.1f%%, AE min %.1f%% at %.2f\nGE peak %.1f%% at MSIR %.2f; GE ends %.1f / %.1f\nGE~GR peak %.1f%% at %.3f\nmean GE %.1f\n",
            ae_lo, ae_min, x_min, pk$fit * 100, pk$x, ge_c$fit[1] * 100, tail(ge_c$fit, 1) * 100, pk2$fit * 100, pk2$x, mean_ge))
print(rt_ratio); print(fr)

## ---- poll icons -----------------------------------------------------------------
icon <- function(f, name, col = "#1F1F1F") {
  d <- tibble(I = seq(0.02, 1, length.out = 200)) %>% mutate(G = f(I))
  p <- ggplot(d, aes(I, G)) + geom_line(colour = col, linewidth = 3) +
    labs(x = "Intake", y = "Growth eff.") + coord_cartesian(ylim = c(0, 1)) +
    theme_talk(26) + theme(axis.text = element_blank(), axis.ticks = element_blank(),
                           axis.line = element_line(arrow = arrow(length = unit(0.25, "cm"), type = "closed"), linewidth = 1))
  save(p, name, w = 3.6, h = 3.2)
}
icon(function(I) 0.15 + 0.7 * I, "p01_increase")
icon(function(I) 0.85 - 0.7 * I, "p02_decrease")
icon(function(I) 0.15 + 0.75 * sin(pi * I) * exp(-0.4 * I), "p03_hump")

## ---- backup: food vs larvae stoichiometry -----------------------------------------
fc <- targets::tar_read(data_irn_food_controls)
food <- fc %>% summarise(C = mean(food_C, na.rm = TRUE), N = mean(food_N, na.rm = TRUE), P = mean(food_P, na.rm = TRUE) / 1e4, K = mean(food_K, na.rm = TRUE) / 1e4)
lar <- dg %>% filter(variable == "larvae", element %in% c("C", "N", "P", "K")) %>% group_by(element) %>%
  summarise(v = mean(elemental_value, na.rm = TRUE)) %>% pivot_wider(names_from = element, values_from = v) %>% mutate(P = P / 1e4, K = K / 1e4)
rat <- bind_rows(food %>% mutate(what = "Food"), lar %>% mutate(what = "Larvae")) %>%
  transmute(what, `C:N` = C / N, `C:P` = C / P, `N:P` = N / P, `C:K` = C / K) %>% pivot_longer(-what)
print(rat)
p <- ggplot(rat, aes(what, value, fill = what)) + geom_col(width = 0.6, colour = INK, linewidth = 0.4) +
  facet_wrap(~name, scales = "free_y", nrow = 1) + scale_fill_manual(values = c(Food = "#F3D27A", Larvae = "#74C476")) +
  labs(x = NULL, y = "Mass ratio") + theme_talk(18) + theme(strip.background = element_blank(), strip.text = element_text(size = 20, family = FONT))
save(p, "b03_food_larvae_ratios", w = 11, h = 4.6)

## (element AE build C, N, P: see F5)

## ---- locator map: Cedar Bog Lake ---------------------------------------------------
if (requireNamespace("maps", quietly = TRUE)) {
  us <- map_data("state")
  p <- ggplot(us, aes(long, lat, group = group)) +
    geom_polygon(aes(fill = region == "minnesota"), colour = "white", linewidth = 0.25) +
    scale_fill_manual(values = c(`FALSE` = "#D9D9D9", `TRUE` = "#8FB8A0")) +
    annotate("point", x = -93.1991, y = 45.4105, size = 7, colour = "#C0392B") +
    coord_map("albers", lat0 = 30, lat1 = 45) + theme_void() +
    theme(legend.position = "none", plot.background = element_rect(fill = "transparent", colour = NA))
  ggsave(file.path(out, "m01_locator.png"), p, width = 7, height = 4.3, dpi = 300, bg = "transparent")
}

## ---- F10: growth rate vs intake (talk v3) -----------------------------------
save(curve_plot(4, lab_gr, pct = FALSE, show_curve = FALSE), "f10a_gr_points")
save(curve_plot(4, lab_gr, pct = FALSE), "f10b_gr_curve")

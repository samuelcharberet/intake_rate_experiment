as.data.frame.pwpm <- function(pwpm) {
  pwpm_m <- matrix(pwpm, nrow = sqrt(length(pwpm)))
  pwpm_m_df <- as.data.frame(pwpm_m)
  rownames(pwpm_m_df) <- attr(pwpm, "dimnames")[[1]]
  colnames(pwpm_m_df) <- attr(pwpm, "dimnames")[[2]]
  pwpm_m_df <- as.data.frame(lapply(df_ae_emm, function(x) ifelse(grepl("<", x) | grepl("e", x), x, signif(as.numeric(x), digits = 2))))
  pwpm_m_df
}

#' model_irn
#'
#' @return model results from the intake rate and nutrient experiment
#' @export
#'
#' @examples
model_irn <- function(data_i, data_g) {
  

  # GAM
  # for all relationships
  # Extract the p-value and the edf. When edf is close to 1, the relationship is close to linear.
  # gam.check function to check whether the model converges or not.
  # Non convergence can be due to too high number of parameters compared to the number of data
  # We also check that smooths have enough basis functions 
  # by looking at the significance result in the diagnostic test.
  
  # Using the broom package, and the augment, tidy, and glance functions, we can 
  # insect, evaluate, and predict
}
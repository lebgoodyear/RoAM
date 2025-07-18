#' Calculate metric value
#' 
#' @description Calculates a number between 0 and 1, using given inputs, corresponding to the metric value
#' @param root_vars Vector of numbers between 0 and 1.
#' @param additional_vars Vector of numbers between 0 and 1.
#' @param betas Vector of numbers between 0 and 1 that sum to 1.
#' @return A number between 0 and 1.
#' @export
#' 
#' @examples
#' calc_metric(c(0.2, 0.3), c(1, 0), c(0.5, 0.3, 0.2))
calc_metric <- function(root_vars = c(), 
                        additional_vars = c(), 
                        betas = c()) {

  # 1. Checks

  # a. Check for variables
  if (length(root_vars) == 0 && length(additional_vars) == 0) {
  stop("At least one of 'root_vars' or 'additional_vars' must be provided.")
  }

  # b. The number of betas provided should be one more than the number of avars
  if (length(betas) != length(additional_vars) + 1) {
      stop("Incorrect number of betas. There must be one beta for every 
      additional variable, plus another beta for the baseline value, i.e.
      number of betas = number of additional variables + 1.")
  }

  # c. Betas must sum to 1
  if (sum(betas) != 1) {
      stop("Betas must sum to 1.")
  }

  # d. All variables must be between 0 and 1.
  if (max(root_vars) > 1 || max(additional_vars) > 1) {
    stop("All variables must be scaled between 0 and 1.")
  }  

  # 2. Preparation

  # Multiply all root variables together  
  fvars_prod <- prod(root_vars)

  # Multiply all additional variables by their corresponding weights, adding
  # an extra variable at the end for the baseline beta
  avars_weighted <- c(additional_vars, 1) * betas

  # Sum all weighted additional variables
  avars_weighted_sum <- sum(avars_weighted)

  # 3. Calculate metric values

  # Formula to calculate metric values from root and weighted additional variables  
  metric_value <- fvars_prod * avars_weighted_sum

  return(metric_value)
  
}

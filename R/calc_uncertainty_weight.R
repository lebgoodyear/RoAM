#' Calculate weight as estimate of uncertainty
#' 
#' @description Combines grade weight with scaled sample size to generate overall weight.
#' @param scaled_sample_size Single numeric or vector of numeric input value/s between 0 and 1.
#' @param grade_weight Single numeric or vector of numeric input value/s between 0 and 1.
#' @return Number or vector of numbers between 0 and 1 corresponding calculated weights.
#' @export
#' 
#' @examples
#' calc_uncertainty_weight(scaled_sample_size = 0.7, grade_weight = 0.8)
#' calc_uncertainty_weight(
#'    scaled_sample_size = c(0.7, 0.5, 0.3, 0.8), 
#'    grade_weight = c(0.8, 0.2, 0.4, 0.4)
#' )
calc_uncertainty_weight <- function(scaled_sample_size, grade_weight) {

    uncertainty_weight <- scaled_sample_size * grade_weight

    return(uncertainty_weight)
}
#' Estimate standard error for metric value
#' 
#' @description Estimates standard error for metric value given only aggregated data. 
#' Calculates an effective sample size by weighting sample size by usability grade weights, 
#' then uses this and the metric value to estimate the alpha and beta parameters of a beta distribution.
#' This beta distribution is referred to as an Artificially Constructucted from Aggregate (ACA) 
#' beta distribution to differentiate from the beta distribution of metric values generated for further analysis.
#' The standard deviation for the ACA beta distribution is then calculated to produce
#' an estimate of the standard error around the metric value for the aggregated data.
#' @param metric_value Single numeric or vector of numeric input value/s between 0 and 1.
#' @param sample_size Any natural number or vector of natural numbers.
#' @param grade_weight Single numeric or vector of numeric input value/s between 0 and 1.
#' @return Number or vector of numbers corresponding to standard error/s.
#' @export
#' 
#' @examples
#' calc_std_error(metric_value = 0.7, sample_size = 100, grade_weight = 0.8)
#' calc_std_error(
#'    metric_value = c(0.7, 0.5, 0.3, 0.8), 
#'    sample_size = c(100, 80, 50, 115), 
#'    grade_weight = c(0.8, 0.5, 0.4, 0.4)
#' )
calc_std_error <- function(metric_value, sample_size, grade_weight) {
    
    # metric value is set as mean value for ACA beta distribution
    mu <- metric_value
    # calculate effective sample size by weighting sample size by usability grade weights
    nu <- sample_size * grade_weight

    # replace 0 and 1 with values close to 0 and 1 to prevent zero standard errors
    mu <- replace(mu, mu==0, 1e-5)
    mu <- replace(mu, mu==1, 1-1e-5)

    # estimate ACA beta distribution parameter alpha
    a <- mu * nu

    # estimate ACA beta distribution parameter beta
    b <- (1 - mu) * nu

    # estimate standard error using formula for standard deviation for ACA beta distribution
    standard_error <- sqrt((a * b) / ((a + b)^2 * (a + b + 1)))

    return(standard_error)
}
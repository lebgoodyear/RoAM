#' Estimate confidence interval for metric value
#' 
#' @description Estimates confidence interval for metric value given only aggregated data. 
#' Uses metric value and standard error to estimate a confidence interval (truncated at 0 and 1)
#' according to the confidence level provided. This confidence interval is based on the
#' standard error and mean of an Artificially Constructucted from Aggregate (ACA) 
#' beta distribution.
#' @param metric_value Number between 0 and 1.
#' @param std_error Number between 0 and 1.
#' @param conf_level Number between 0 and 1, corresponding to confidence level.
#' @return Vector where first element is lower bound and second element is upper bound.
#' 
#' @importFrom stats qnorm
#' @export
#' 
#' @examples
#' calc_ci(metric_value = 0.7, std_error = 0.2)
#' df <- data.frame(
#'    MetricValue = c(0.7, 0.5, 0.3, 0.8), 
#'    StdError = c(0.5, 0.3, 0.2, 0.4)
#' )
#' # Lower bound
#' sapply(1:nrow(df), function(i) {
#'    calc_ci(df$MetricValue[i], df$StdError[i])[1]
#' })
#' # Upper bound
#' sapply(1:nrow(df), function(i) {
#'    calc_ci(df$MetricValue[i], df$StdError[i])[2]
#' })
calc_ci <- function(metric_value, std_error, conf_level = 0.95) {

    # 1. Calculate z-score for given confidence interval
    z_score <- qnorm(1 - (1 - conf_level) / 2)

    # 2. Calculate confidence interval lower bound 
    lower_bound <- max(0, metric_value - z_score * std_error)

    # 3. Calculate confidence interval upper bound 
    upper_bound <- min(1, metric_value + z_score * std_error)

    # output is a vector where first element is lower bound and second element is upper bound
    return(c(lower_bound, upper_bound))
}
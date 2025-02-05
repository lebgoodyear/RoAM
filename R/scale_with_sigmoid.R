#' Create sigmoid relationship
#' 
#' @description Scales continuous input to between 0 and 1 according to a sigmoidal relationship.
#' @param x Numeric or vector of numeric input values.
#' @param k Numeric. Shape parameter dictating 'steepness' of curve (higher k => steeper).
#' @param x0 Numeric. Location of midpoint of curve.
#' @return Vector of numbers between 0 and 1 corresponding to sigmoid curve.
#' @export
#' 
#' @examples
#' x <- seq(0, 100, length=100)
#' scale_with_sigmoid(log10(x), 10^(-(log10(log10(max(x)))-1)), log10(max(x))/2)
scale_with_sigmoid <- function(x, k, x0) {

    y <- 1 / (1 + exp(-k*(x - x0)))
    
    return(y)
}
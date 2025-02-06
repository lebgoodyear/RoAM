#' Scale sample size for use as weights
#' 
#' @description Scales continuous input to between 0 and 1 for use as weights.
#' @param x Vector of numeric input values.
#' @param method Character string either "sigmoid" (default) or "log-linear".
#' @return Vector of numbers between 0 and 1 corresponding calculated weights.
#' @export
#' 
#' @examples
#' scale_sample_size(c(100, 50, 150), method="log-linear")
#' df <- data.frame(SampleSize=c(10, 15, 100, 30, 50, 90, 140))
#' scale_sample_size(df$SampleSize)
scale_sample_size <- function(x, method="sigmoid") {

    # first log10 the input
    xlog <- log10(x)

    if (!(method %in% c("sigmoid", "log-linear"))) {
        stop("Method must be either log-linear or sigmoid.")
    }

    # store maximum sample size
    xmax_raw <- max(x)
    # store log10 of maximum sample size
    xmax <- log10(xmax_raw)

    if (method == "sigmoid") {
        # call function to generate sigmoidal relationship
        # default k is generated to produce symmetrical relationship between 0 and 1
        # default x0 is generated to put midpoint in the centre of the curve between 0 and 1
        y <- dare::scale_with_sigmoid(xlog, k = 10^(-(log10(xmax)-1)), x0 = log10(xmax_raw)/2)
    }

    if (method == "log-linear") {
        # scale log values linearly to between 0 and 1
        y <- xlog / xmax
    }

    return(y)
}
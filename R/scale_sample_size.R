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
scale_sample_size1 <- function(x, method="sigmoid") {

    # first log10 the input
    xlog <- log10(x)

    if (!(method %in% c("sigmoid", "log-linear"))) {
        stop("Method must be either log-linear or sigmoid.")
    }

    # store maximum log10 sample size
    xmax <- max(xlog)

    if (method == "sigmoid") {
        # call function to generate sigmoidal relationship
        # default k is generated to produce symmetrical relationship between 0 and 1
        # default x0 is generated to put midpoint in the centre of the curve between 0 and 1
        y <- roam::scale_with_sigmoid(xlog, k = 10/xmax, x0 = xmax/2)
    }

    if (method == "log-linear") {
        # add small value to any sample size equal to 1 so that no datapoint is given 0 weight
        # log10(2) = 0.3
        # log10(3) = 0.48
        # log10(4) = 0.6
        # sample sizes are natural numbers and so 0.1 is appropriate choice
        xlog[xlog == 0] <- 0.1
        # scale log values linearly to between 0 and 1
        y <- xlog / xmax
    }

    return(y)
}
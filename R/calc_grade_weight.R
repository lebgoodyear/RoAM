#' Calculate weight for usabilty grade
#' 
#' @description Scales usability grade of any format to between 0 and 1 for use as weight.
#' @param grade Input value to be scaled.
#' @param max_grade Number corresponding to best possible grade.
#' @param min_grade Number corresponding to worst possible grade.
#' @param min_weight Number between 0 and 1 corresponding to weight chosen for worst possible grade.
#' @return Number between 0 and 1 corresponding calculated weight.
#' @export
#' 
#' @examples
#' calc_grade_weight(grade=2, max_grade=0, min_grade=5, min_weight=0.5)
#' df <- data.frame(Grade=c(1, 0, 2, 1, 1, 2, 2, 2, 0))
#' df$GradeWeights <- sapply(
#'  1:nrow(df), 
#'  function(i) {
#'    calc_grade_weight(
#'      grade=df$Grade[i], 
#'      max_grade=2,
#'      min_grade=0,
#'      min_weight=0.6
#'    )
#'  }
#')
calc_grade_weight <- function(grade, max_grade, min_grade, min_weight) {

    # 1. Checks and necessary transformations

    # use if statement to account for lowest number or highest number being the best grade
    if (max_grade < min_grade) {
        # check grade is not out of bounds
        if (grade < max_grade || grade > min_grade) {
            stop("Grade provided is not within minimum/maximum bounds.")
        }
        adjusted_grade <- 1 - (grade - max_grade)/abs(max_grade - min_grade)
    } else {
        # check grade is not out of bounds
        if (grade < min_grade || grade > max_grade) {
            stop("Grade provided is not within minimum/maximum bounds.")
        }
        adjusted_grade <- (grade - min_grade)/abs(max_grade - min_grade)
    }

    # 2. Calculate proprtion available for weight

    free_weights <- 1 - min_weight

    # 3. Calculate grade weight

    weight <- min_weight + free_weights * adjusted_grade

    return(weight)   
}
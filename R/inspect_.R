
#' @title Validate vectors of probabilities
#'
#' @description `inspect_prob` checks if an object is a numeric vector of valid probability values. This can be useful to validate inputs, intermediate calculations or outputs in user-defined functions.
#'
#' @param x An arbitrary object.
#' @param allow_nas Logical value. If `TRUE` then `NA` and `NaN` values in `x` are allowed. If `FALSE`, execution is stopped and an error message is thrown in case there are `NA` or `NaN` values in `x`.
#' @param warning_nas Logical value. If `TRUE` then the presence of `NA` or `NaN` values in `x` generates a warning message. `NA` and `NaN` values pass silently otherwise (if `allow_nas` is set to `TRUE`).
#'
#' @details `inspect_prob` conducts a series of tests to check if `x` is a numeric vector of valid probability values. Namely, `inspect_prob` checks if:
#' * `x` is `NULL` or empty.
#' * `x` is an atomic vector.
#' * `x` is numeric.
#' * `x` has `NA` or `NaN` values.
#' *  The values of `x` are in the \[0, 1\] interval.
#'
#' @return `inspect_prob` does not return any output. There are three possible outcomes:
#' * The call is silent if:
#'   * `x` is a numeric vector of valid probability values and there are no `NA` or `NaN` values in `x`.
#'   * `x` is a numeric vector of valid probability values, there are some `NA` or `NaN` values in `x`, `allow_nas` is set to `TRUE` and `warning_nas` is set to `FALSE`.
#' * An informative warning message is thrown if `x` is a numeric vector of valid probability values, there are some `NA` or `NaN` values in `x` and both `allow_nas` and `warning_nas` are set to `TRUE`.
#' * An informative error message is thrown and the execution is stopped if:
#'   * `x` is not a numeric vector of valid probability values.
#'   * `x` is a numeric vector of valid probability values, there are some `NA` or `NaN` values in `x` and `allow_nas` is set to `FALSE`.
#'
#' @seealso
#' * \code{\link[inspector]{inspect_par_bernoulli}} to check if an object is a valid Bernoulli/Binomial proportion.
#' * \code{\link[inspector]{inspect_par_multinomial}} to check if an object is a numeric vector of valid Multinomial proportions.
#'
#' @examples
#' # Calls that pass silently:
#' x1 <- c(0.1, 0.2, 0.3, 0.4, 0.5)
#' x2 <- c(0.1, 0.2, 0.3, 0.4, 0.5, NA)
#' inspect_prob(x1)
#' inspect_prob(x2, warning_nas = FALSE)
#' inspect_prob(x2, allow_nas = TRUE, warning_nas = FALSE)
#'
#' # Calls that throw an informative warning message:
#' y <- c(0.1, 0.2, NA, 0.4, 0.5)
#' try(inspect_prob(y))
#' try(inspect_prob(y, allow_nas = TRUE))
#' try(inspect_prob(y, allow_nas = TRUE, warning_nas = TRUE))
#'
#' # Calls that throw an informative error message:
#' z1 <- c(-0.9, 0, 0.1, 0.2, 0.3, 0.4, 0.5)
#' try(inspect_prob(z1))
#' z2 <- c(NA, 0, 0.1, 0.2, 0.3, 0.4, 0.5)
#' try(inspect_prob(z2, allow_nas = FALSE))
#' mylist <- list(NULL, TRUE, factor(.5), matrix(0.5),
#'          "0.5", list(0.5), NA, NaN, numeric(0), 1.1, -0.5)
#' try(inspect_prob(mylist[[1]]))
#' try(inspect_prob(mylist[[2]]))
#' try(inspect_prob(mylist[[3]]))
#' try(inspect_prob(mylist[[4]]))
#' try(inspect_prob(mylist[[5]]))
#' try(inspect_prob(mylist[[6]]))
#' try(inspect_prob(mylist[[7]]))
#' try(inspect_prob(mylist[[8]]))
#' try(inspect_prob(mylist[[9]]))
#' try(inspect_prob(mylist[[10]]))
#' try(inspect_prob(mylist[[11]]))
#'
#' @export

inspect_prob <- function(x, allow_nas = TRUE, warning_nas = TRUE){

  inspect_true_or_false(allow_nas)
  inspect_true_or_false(warning_nas)

  output_name <- deparse(substitute(x))

  x_filtered <- x[!is.na(x)]

  if(is.null(x)){
    stop(paste("Invalid argument:", output_name, "is NULL."))
  }
  if(any(isFALSE(is.atomic(x)), isFALSE(is.vector(x)))){
    stop(paste("Invalid argument:", output_name, "must be an atomic vector."))
  }
  if(length(x) == 0){
    stop(paste("Invalid argument:", output_name, "is empty."))
  }
  if(all(is.na(x))){
    stop(paste("Invalid argument: all elements of", output_name, "are NA or NaN."))
  }
  if(any(is.na(x))){
    if(isFALSE(allow_nas)) {
      stop(paste("Invalid argument: There are NA or NaN values in ", paste0(output_name, ".")))
    } else {
      if(isTRUE(warning_nas)){
        warning(paste("There are NA or NaN values in", paste0(output_name, ".")))
      }
    }
  }
  if(isFALSE(is.numeric(x))){
    stop(paste("Invalid argument: the type of", output_name, "must be numeric."))
  }
  if(any(x_filtered < 0, x_filtered > 1)){
    stop(paste("Invalid argument: all elements of",  output_name, "must be in the [0, 1] interval."))
  }
}

#' @title Validate vectors of  Bayes factors
#'
#' @description `inspect_bfactor` checks if an object is a numeric vector of valid Bayes factor values. This can be useful to validate inputs, intermediate calculations or outputs in user-defined functions.
#'
#' @param x An arbitrary object.
#' @param allow_nas Logical value. If `TRUE` then `NA` and `NaN` values in `x` are allowed. If `FALSE`, execution is stopped and an error message is thrown in case there are `NA` or `NaN` values in `x`.
#' @param warning_nas Logical value. If `TRUE` then the presence of `NA` or `NaN` values in `x` generates a warning message. `NA` and `NaN` values pass silently otherwise (if `allow_nas` is `TRUE`).
#'
#' @details `inspect_bfactor` conducts a series of tests to check if `x` is a numeric vector of valid Bayes factor values. Namely, `inspect_bfactor` checks if:
#' * `x` is `NULL` or empty.
#' * `x` is an atomic vector.
#' * `x` is numeric.
#' * `x` has `NA` or `NaN` values.
#' *  The values of `x` are non-negative.
#'
#' @return `inspect_bfactor` does not return any output. There are three possible outcomes:
#' * The call is silent if:
#'   * `x` is a numeric vector of valid Bayes factor values and there are no `NA` or `NaN` values in `x`.
#'   * `x` is a numeric vector of valid Bayes factor values, there are some `NA` or `NaN` values in `x`, `allow_nas` is set to `TRUE` and `warning_nas` is set to `FALSE`.
#' * An informative warning message is given if `x` is a numeric vector of valid Bayes factor values, there are some `NA` or `NaN` values in `x` and both  `allow_nas` and `warning_nas` are set to `TRUE`.
#' * An informative error message is thrown and the execution is stopped if:
#'   * `x` is not a numeric vector of valid Bayes factor values.
#'   * `x` is a numeric vector of valid Bayes factor values, there are some in `NA` or `NaN` values in `x` and `allow_nas` is set to `FALSE`.
#'
#' @seealso
#' * \code{\link[inspector]{inspect_bfactor_log}} to check if an object is a numeric vector of valid logarithmic Bayes factor values.
#' * \code{\link[pcal]{bfactor_interpret}} for the interpretation of Bayes factors.
#' * \code{\link[inspector]{inspect_bfactor_scale}} to check if an object is a valid Bayes factor interpretation scale.
#'
#' @examples
#' # Calls that pass silently:
#' x1 <- c(0, 0.5, 1, 10, 50, 100)
#' x2 <- c(NA, 0.5, 1, 10, 50, 100)
#' inspect_bfactor(x1)
#' inspect_bfactor(x2, warning_nas = FALSE)
#' inspect_bfactor(x2, allow_nas = TRUE, warning_nas = FALSE)
#'
#' # Call that throws an informative warning message:
#' y <- c(0.1, 0.2, NA, 0.4, 0.5)
#' try(inspect_bfactor(y))
#' try(inspect_bfactor(y, warning_nas = TRUE))
#' try(inspect_bfactor(y, allow_nas = TRUE, warning_nas = TRUE))
#'
#' # Calls that throw informative error messages:
#' z <- c(-0.9, 0, 0.1, 0.2, 0.3, 0.4, 0.5)
#' try(inspect_bfactor(z))
#' mylist <- list(NULL, TRUE, factor(.5), matrix(0.5),
#'          "0.5", list(0.5), NA, NaN, numeric(0), -0.5, -5)
#' try(inspect_bfactor(mylist[[1]]))
#' try(inspect_bfactor(mylist[[2]]))
#' try(inspect_bfactor(mylist[[3]]))
#' try(inspect_bfactor(mylist[[4]]))
#' try(inspect_bfactor(mylist[[5]]))
#' try(inspect_bfactor(mylist[[6]]))
#' try(inspect_bfactor(mylist[[7]]))
#' try(inspect_bfactor(mylist[[8]]))
#' try(inspect_bfactor(mylist[[9]]))
#' try(inspect_bfactor(mylist[[10]]))
#' try(inspect_bfactor(mylist[[11]]))
#'
#' @export

inspect_bfactor <- function(x, allow_nas = TRUE, warning_nas = TRUE){

  inspect_true_or_false(allow_nas)
  inspect_true_or_false(warning_nas)

  output_name <- deparse(substitute(x))

  if(is.null(x)){
    stop(paste("Invalid argument:", output_name, "is NULL."))
  }
  if(any(isFALSE(is.atomic(x)), isFALSE(is.vector(x)))){
    stop(paste("Invalid argument:", output_name, "must be an atomic vector."))
  }
  if(length(x) == 0){
    stop(paste("Invalid argument:", output_name, "is empty."))
  }
  if(all(is.na(x))){
    stop(paste("Invalid argument: all elements of ", output_name, "are NA or NaN."))
  }
  if(isFALSE(is.numeric(x))){
    stop(paste("Invalid argument: the type of", output_name, "must be numeric."))
  }
  if(any(x[!is.na(x)] < 0)){
    stop(paste("Invalid argument: all elements of", output_name, "must be non-negative."))
  }
  if(any(is.na(x))){
    if(isFALSE(allow_nas)) {
      stop(paste("Invalid argument: There are NA or NaN values in ", paste0(output_name, ".")))
    } else {
      if(isTRUE(warning_nas)){
        warning(paste("There are NA or NaN values in", paste0(output_name, ".")))
      }
    }
  }
}

#' @title Validate vectors of logarithmic Bayes factors
#'
#' @description `inspect_bfactor_log` checks if an object is a numeric vector of valid logarithmic Bayes factor values.  This can be useful to validate inputs, intermediate calculations or outputs in user-defined functions.
#'
#' @param x An arbitrary object.
#' @param allow_nas Logical value. If `TRUE` then `NA` and `NaN` values in `x` are allowed. If `FALSE`, execution is stopped and an error message is thrown in case there are `NA` or `NaN` values in `x`.
#' @param warning_nas Logical value. If `TRUE` then the presence of `NA` or `NaN` values in `x` generates a warning message. `NA` and `NaN` values pass silently otherwise (if `allow_nas` is `TRUE`).
#'
#' @details `inspect_bfactor_log` conducts a series of tests to check if `x` is a numeric vector of valid logarithmic Bayes factor values. Namely, `inspect_bfactor_log` checks if:
#' * `x` is `NULL` or empty.
#' * `x` is an atomic vector.
#' * `x` is numeric.
#' * `x` has `NA` or `NaN` values.
#'
#' @return `inspect_bfactor_log` does not return any output. There are three possible outcomes:
#' * The call is silent if:
#'   * `x` is a numeric vector of valid logarithmic Bayes factor values and there are no `NA` or `NaN` values in `x`.
#'   * `x` is a numeric vector of valid logarithmic Bayes factor values, there are some `NA` or `NaN` values in `x`, `allow_nas` is set to `TRUE` and `warning_nas` is set to `FALSE`.
#' * An informative warning message is given if `x` is a numeric vector of valid logarithmic Bayes factor values, there are some `NA` or `NaN` values in `x` and both  `allow_nas` and `warning_nas` are set to `TRUE`.
#' * An informative error message is thrown and the execution is stopped if:
#'   * `x` is not a numeric vector of valid logarithmic Bayes factor values.
#'   * `x` is a numeric vector of valid logarithmic Bayes factor values, there are some `NA` or `NaN` values in `x` and `allow_nas` is set to `FALSE`.
#'
#' @seealso
#' * \code{\link[inspector]{inspect_bfactor}} to check if an object is a numeric vector of valid Bayes factor values.
#' * \code{\link[pcal]{bfactor_log_interpret}} for the interpretation of the logarithms of Bayes factors.
#' * \code{\link[inspector]{inspect_bfactor_scale}} to check if an object is a Bayes factor interpretation scale.
#' * \code{\link[inspector]{inspect_log_base}} to check if an object is an eligible logarithmic base.
#'
#' @examples
#' # Calls that pass silently:
#' x1 <- c(0, 0.5, 1, 10, 50, 100)
#' x2 <- c(NA, 0.5, 1, 10, 50, 100)
#' inspect_bfactor_log(x1)
#' inspect_bfactor_log(x2, warning_nas = FALSE)
#' inspect_bfactor_log(x2, allow_nas = TRUE, warning_nas = FALSE)
#'
#' # Call that throws an informative warning message:
#' y <- c(0.1, 0.2, NA, 0.4, 0.5)
#' try(inspect_bfactor_log(y))
#' try(inspect_bfactor_log(y, warning_nas = TRUE))
#' try(inspect_bfactor_log(y, allow_nas = TRUE, warning_nas = TRUE))
#'
#' # Calls that throw informative error messages:
#' mylist <- list(NULL, TRUE, factor(.5), matrix(0.5),
#'          "0.5", list(0.5), numeric(0), NA, NaN)
#' try(inspect_bfactor_log(mylist[[1]]))
#' try(inspect_bfactor_log(mylist[[2]]))
#' try(inspect_bfactor_log(mylist[[3]]))
#' try(inspect_bfactor_log(mylist[[4]]))
#' try(inspect_bfactor_log(mylist[[5]]))
#' try(inspect_bfactor_log(mylist[[6]]))
#' try(inspect_bfactor_log(mylist[[7]]))
#' try(inspect_bfactor_log(mylist[[8]]))
#' try(inspect_bfactor_log(mylist[[9]]))
#'
#' @export

inspect_bfactor_log <- function(x, allow_nas = TRUE, warning_nas = TRUE){

  inspect_true_or_false(allow_nas)
  inspect_true_or_false(warning_nas)

  output_name <- deparse(substitute(x))

  if(is.null(x)){
    stop(paste("Invalid argument:", output_name, "is NULL."))
  }
  if(any(isFALSE(is.atomic(x)), isFALSE(is.vector(x)))){
    stop(paste("Invalid argument:", output_name, "must be an atomic vector."))
  }
  if(length(x) == 0){
    stop(paste("Invalid argument:", output_name, "is empty."))
  }
  if(all(is.na(x))){
    stop(paste("Invalid argument: all elements of ", output_name, "are NA or NaN."))
  }
  if(isFALSE(is.numeric(x))){
    stop(paste("Invalid argument: the type of", output_name, "must be numeric."))
  }
  if(any(is.na(x))){
    if(isFALSE(allow_nas)) {
      stop(paste("Invalid argument: There are NA or NaN values in ", paste0(output_name, ".")))
    } else {
      if(isTRUE(warning_nas)){
        warning(paste("There are NA or NaN values in", paste0(output_name, ".")))
      }
    }
  }
}

#' @title Validate logarithmic bases
#'
#' @description `inspect_log_base` checks if an object is a valid a logarithmic base. This can be useful to validate inputs in user-defined functions.
#'
#' @param x An arbitrary object.
#'
#' @details `inspect_log_base` conducts a series of tests to check if `x` is a valid logarithmic base. Namely, `inspect_log_base` checks if:
#' * `x` is `NULL` or empty.
#' * `x` is an atomic vector of \code{\link[base]{length}} 1.
#' * `x` is numeric.
#' * `x` is `NA` or `NaN`.
#' * `x` is positive.
#'
#' @return `inspect_log_base` does not return any output. There are two possible outcomes:
#' * The call is silent if `x` is a numeric vector of \code{\link[base]{length}} 1 that is a valid logarithmic base.
#' * An informative error message is thrown otherwise.
#'
#' @seealso
#' * \code{\link[pcal]{bfactor_log_interpret}} for the interpretation of the logarithms of Bayes factors.
#' * \code{\link[inspector]{inspect_bfactor_log}} to check if an object is a numeric vector of valid logarithmic Bayes factor values.
#'
#' @examples
#' # Calls that pass silently:
#' x1 <- 10
#' x2 <- exp(1)
#' x3 <- 0.5
#' inspect_log_base(x1)
#' inspect_log_base(x2)
#' inspect_log_base(x3)
#'
#' # Calls that throw informative error messages:
#' mylist <- list(NULL, numeric(0), TRUE, factor(10),
#'         list(10), matrix(10), NaN, NA, -1, 0)
#' try(inspect_log_base(mylist[[1]]))
#' try(inspect_log_base(mylist[[2]]))
#' try(inspect_log_base(mylist[[3]]))
#' try(inspect_log_base(mylist[[4]]))
#' try(inspect_log_base(mylist[[5]]))
#' try(inspect_log_base(mylist[[6]]))
#' try(inspect_log_base(mylist[[7]]))
#' try(inspect_log_base(mylist[[8]]))
#' try(inspect_log_base(mylist[[9]]))
#' try(inspect_log_base(mylist[[10]]))
#'
#' @export

inspect_log_base <- function(x){

  output_name <- deparse(substitute(x))

  if(is.null(x)){
    stop(paste("Invalid argument:", output_name, "is NULL."))
  }

  if(any(isFALSE(is.vector(x)), isFALSE(is.atomic(x)), isFALSE(length(x) == 1))
    ){
    stop(paste("Invalid argument:", output_name, "must be an atomic vector of length 1."))
  }
  if(is.na(x)){
    stop(paste("Invalid argument:", output_name, "is NA or NaN."))
  }
  if(isFALSE(is.numeric(x))){
    stop(paste("Invalid argument: the type of", output_name, "must be numeric."))
  }
  if(isTRUE(x <= 0)){
    stop(paste("Invalid argument:", output_name, "must be positive."))
  }
}

#' @title Validate Bayes factor interpretation scales
#'
#' @description `inspect_bfactor_scale` checks if an object is a character vector of \code{\link[base]{length}} 1 that is eligible to represent one of the Bayes factor interpretation scales available in the `pcal` package. This can be useful to validate inputs in user-defined functions.
#'
#' @param x An arbitrary object.
#'
#' @details `inspect_bfactor_scale` conducts a series of tests to check if `x` is a character vector of \code{\link[base]{length}} 1 that is eligible to represent one of the Bayes factor interpretation scales available in the `pcal` package. Namely, `inspect_bfactor_scale` checks if:
#' * `x` is `NULL` or empty.
#' * `x` is `NA` or `NaN`.
#' * `x` is an atomic vector of \code{\link[base]{length}} 1
#' * The \code{\link[base]{typeof}} `x` is character
#' * The value of `x` is either "Jeffreys" or "Kass-Raftery" (not case sensitive).
#'
#' @return `inspect_bfactor_scale` does not return any output. There are two possible scenarios:
#' * The call is silent if `x` is a character vector of \code{\link[base]{length}} 1 that is eligible to represent one of the Bayes factor interpretation scales available in the `pcal` package.
#' * An informative error message is thrown otherwise.
#'
#' @seealso
#' * \code{\link[pcal]{bfactor_interpret}} for the interpretation of Bayes factors.
#' * \code{\link[pcal]{bfactor_log_interpret}} for the interpretation of the logarithms of Bayes factors.
#' * \code{\link[inspector]{inspect_bfactor}} to check if an object is a numeric vector of valid Bayes factor values.
#' * \code{\link[inspector]{inspect_bfactor_log}} to check if an object is a numeric vector of valid logarithmic Bayes factor values.
#'
#' @examples
#' # Calls that pass silently:
#' x1 <- "Jeffreys"
#' x2 <- "jeffreys"
#' x3 <- "kass-raftery"
#' x4 <- "Kass-Raftery"
#' inspect_bfactor_scale(x1)
#' inspect_bfactor_scale(x2)
#' inspect_bfactor_scale(x3)
#' inspect_bfactor_scale(x4)
#'
#' # Calls that throw informative error messages:
#' mylist <- list(NULL, NA, NaN, 10, "Bayes", "Jeff",
#'           "kassraftery", c("jeffreys", "kass-raftery"))
#' try(inspect_bfactor_scale(mylist[[1]]))
#' try(inspect_bfactor_scale(mylist[[2]]))
#' try(inspect_bfactor_scale(mylist[[3]]))
#' try(inspect_bfactor_scale(mylist[[4]]))
#' try(inspect_bfactor_scale(mylist[[5]]))
#' try(inspect_bfactor_scale(mylist[[6]]))
#' try(inspect_bfactor_scale(mylist[[7]]))
#' try(inspect_bfactor_scale(mylist[[8]]))
#'
#' @export

inspect_bfactor_scale <- function(x){

  output_name <- deparse(substitute(x))

  if(is.null(x)){
    stop(paste("Invalid argument:", output_name, "is NULL."))
  }
  if(any(isFALSE(is.vector(x)), isFALSE(is.atomic(x)), isFALSE(length(x) == 1))){
    stop(paste("Invalid argument:", output_name, "must be an atomic vector of length 1."))
  }
  if(is.na(x)){
    stop(paste("Invalid argument:", output_name, "is NA or NaN."))
  }
  if(!is.character(x)){
    stop(paste("Invalid argument: the type of", output_name, "must be character."))
  }
  if(isFALSE(tolower(x) %in% c("jeffreys", "kass-raftery"))){
    stop(paste("Invalid argument:", output_name, "must be either 'jeffreys' or 'kass-raftery'."))
  }
}

#' @title Validate non-missing logical values
#'
#' @description `inspect_true_or_false` checks if an object is a non-missing logical vector of \code{\link[base]{length}} 1. This can be useful to validate inputs in user-defined functions.
#'
#' @param x An arbitrary object.
#'
#' @details `inspect_true_or_false` conducts a series of tests to check if `x` is a non-missing logical vector of \code{\link[base]{length}} 1. Namely, `inspect_true_or_false` checks if:
#' * `x` is `NULL` or empty.
#' * `x` is an atomic vector of \code{\link[base]{length}} 1.
#' * The \code{\link[base]{typeof}} `x` is logical.
#' * `x` is `NA` or `NaN`.
#'
#' @return `inspect_true_or_false` does not return any output. There are two possible scenarios:
#' * The call is silent if `x` is a non-missing logical vector of \code{\link[base]{length}} 1.
#' * An informative error message is thrown otherwise.
#'
#' @seealso
#' * \code{\link[inspector]{inspect_character}} to validate character vectors.
#' * \code{\link[inspector]{inspect_character_match}} to validate character vectors with predefined allowed values.
#'
#' @examples
#' # Calls that pass silently:
#' x <- TRUE
#' y <- FALSE
#' inspect_true_or_false(x)
#' inspect_true_or_false(y)
#'
#' # Calls that throw informative error messages:
#' mylist <- list(NULL, NA, NaN, 1, 0, "TRUE")
#' try(inspect_true_or_false(mylist[[1]]))
#' try(inspect_true_or_false(mylist[[2]]))
#' try(inspect_true_or_false(mylist[[3]]))
#' try(inspect_true_or_false(mylist[[4]]))
#' try(inspect_true_or_false(mylist[[5]]))
#' try(inspect_true_or_false(mylist[[6]]))
#'
#' @export

inspect_true_or_false <- function(x){

  output_name <- deparse(substitute(x))

  if(is.null(x)){
    stop(paste("Invalid argument:", output_name, "is NULL."))
  }
  if(any(isFALSE(is.vector(x)), isFALSE(is.atomic(x)), isFALSE(length(x) == 1))){
    stop(paste("Invalid argument:", output_name, "must be an atomic vector of length 1."))
  }
  if(is.na(x)){
    stop(paste("Invalid argument:", output_name, "is NA or NaN."))
  }
  if(isFALSE(is.logical(x))){
    stop(paste("Invalid argument: the type of", output_name, "must be logical."))
  }
}

#' @title Validate factor levels
#'
#' @description `inspect_categories` checks if an object is eligible to be used as the levels of a factor. This can be useful to validate inputs in user-defined functions.
#'
#' @param x An arbitrary object.
#'
#' @details `inspect_categories` conducts a series of tests to check if `x` is eligible to be used as the levels of a factor. Namely, `inspect_categories` checks if:
#' * `x` is `NULL` or empty.
#' * `x` is atomic.
#' * `x` has an eligible data type (logical, integer, double, character).
#' * There are `NA` or `NaN` values in `x`.
#' * There are repeated values in `x`.
#'
#' @return `inspect_categories` does not return any output. There are two possible outcomes:
#' * The call is silent if `x` is eligible to be used as the levels of a factor.
#' * An informative error message is thrown otherwise.
#'
#' @seealso
#' * \code{\link[inspector]{inspect_data_dichotomous}} to validate dichotomous data.
#' * \code{\link[inspector]{inspect_data_categorical}} and \code{\link[inspector]{inspect_data_cat_as_dichotomous}} to validate categorical data.
#' * \code{\link[inspector]{inspect_par_bernoulli}} to validate Bernoulli/Binomial proportions.
#' * \code{\link[inspector]{inspect_par_multinomial}} to validate vectors of Multinomial proportions.
#' * \code{\link[inspector]{inspect_character}} to validate character vectors.
#' * \code{\link[inspector]{inspect_character_match}} to validate character vectors with predefined allowed values.
#'
#' @examples
#' # Calls that pass silently:
#' x1 <- 1:5
#' x2 <- c("yes", "no")
#' x3 <- c(TRUE, FALSE)
#' x4 <- factor(c("smoker", "non-smoker"))
#' x5 <- factor(c("yes", "no", "yes"))
#' inspect_categories(x1)
#' inspect_categories(x2)
#' inspect_categories(x3)
#' inspect_categories(x4)
#' inspect_categories(levels(x5))
#'
#' # Calls that throw informative error messages:
#' y1 <- c(1, 1:5)
#' y2 <- c("yes", "no", "yes")
#' y3 <- factor(c("yes", "no", "yes"))
#' try(inspect_categories(y1))
#' try(inspect_categories(y2))
#' try(inspect_categories(y3))
#' try(mylist <- list(NULL, numeric(0),
#'  complex(1), list(10), NaN, NA))
#' try(inspect_categories(mylist[[1]]))
#' try(inspect_categories(mylist[[2]]))
#' try(inspect_categories(mylist[[3]]))
#' try(inspect_categories(mylist[[4]]))
#' try(inspect_categories(mylist[[5]]))
#' try(inspect_categories(mylist[[6]]))
#'
#' @export

inspect_categories <- function(x){

  output_name <- deparse(substitute(x))

  if(is.null(x)){
    stop(paste("Invalid argument:", output_name, "is NULL."))
  }
  if(isFALSE(is.atomic(x))){
    stop(paste("Invalid argument:", output_name, "must be of an atomic type."))
  }
  if(isTRUE(length(x) == 0)){
    stop(paste("Invalid argument:", output_name, "is empty."))
  }
  if(isFALSE(typeof(x) %in% c("logical", "integer", "double", "character"))){
    stop(paste("Invalid argument: the type of", output_name, "must be 'logical', 'integer', 'double' or 'character'."))
  }
  if(any(is.na(x))){
    stop(paste("Invalid argument: there are NA or NaN values in",  paste0(output_name, ".")))
  }
  if(isFALSE(length(unique(x)) == length(x))){
    stop(paste("Invalid argument: all element of", output_name, "must be unique."))
  }
}

#' @title Validate character values
#'
#' @description `inspect_character_match` checks if an object is a character vector of \code{\link[base]{length}} 1 that belongs to a set of allowed values. This can be useful to validate inputs in user-defined functions.
#'
#' @param x An arbitrary object.
#' @param allowed A character vector.
#' @param case_sensitive A non-missing logical value.
#'
#' @details `inspect_character_match` conducts a series of tests to check if `x` is a character vector of \code{\link[base]{length}} 1 whose value belongs to the set of allowed values. Namely, `inspect_character_match` checks if:
#' * `x` is `NULL` or empty.
#' * `x` is an atomic vector of \code{\link[base]{length}} 1.
#' * The \code{\link[base]{typeof}} `x` is character.
#' * `x` is `NA` or `NaN`.
#' * `x` is one of the allowed values (as specified in the `allowed` argument).
#'
#' By default, the comparison of `x` with `allowed` is not case sensitive. If you only want case sensitive matches of `x` to `allowed` set `case_sensitive` to `TRUE`.
#'
#' @return `inspect_character_match` does not return any output. There are two possible outcomes:
#' * The call is silent if `x` is a character vector of \code{\link[base]{length}} 1 whose value belongs to the set of allowed values.
#' * An informative error message is thrown otherwise.
#'
#' @seealso
#' * \code{\link[inspector]{inspect_character}} to validate character vectors with arbitrary allowed values.
#' * \code{\link[inspector]{inspect_true_or_false}} to check if an object is a non-missing logical value.
#'
#' @examples
#' # Calls that pass silently:
#' x1 <- "Kass"
#' x2 <- "kass"
#' inspect_character_match(x1, allowed = c("Kass", "Raftery"))
#' inspect_character_match(x2, allowed = c("Kass", "Raftery"))
#'
#' # Calls that throw informative error messages:
#' y1 <- "kasss"
#' y2 <- "kass"
#' try(inspect_character_match(y1, allowed = c("Kass", "Raftery")))
#' try(inspect_character_match(y2, allowed = c("Kass", "Raftery"),
#'  case_sensitive = TRUE))
#' mylist <- list(NULL, character(0), c("abc", "abcd"),
#'  c("abc", "abc"), "ab", list("abc"), factor("abc"), NaN, NA)
#' try(inspect_character_match(mylist[[1]], "abc"))
#' try(inspect_character_match(mylist[[2]], "abc"))
#' try(inspect_character_match(mylist[[3]], "abc"))
#' try(inspect_character_match(mylist[[4]], "abc"))
#' try(inspect_character_match(mylist[[5]], "abc"))
#' try(inspect_character_match(mylist[[6]], "abc"))
#' try(inspect_character_match(mylist[[7]], "abc"))
#' try(inspect_character_match(mylist[[8]], "abc"))
#' try(inspect_character_match(mylist[[9]], "abc"))
#'
#' @export

inspect_character_match <- function(x, allowed, case_sensitive = FALSE){

  inspect_character(allowed)
  inspect_true_or_false(case_sensitive)

  x_output_name <- deparse(substitute(x))


  if(is.null(x)){
    stop(paste("Invalid argument:", x_output_name, "is NULL."))
  }
  if(any(isFALSE(is.atomic(x)), isFALSE(is.vector(x)), isFALSE(length(x) == 1))){
    stop(paste("Invalid argument:", x_output_name, "must be an atomic vector of length 1."))
  }
  if(is.na(x)){
    stop(paste("Invalid argument:", x_output_name, "is NA or NaN."))
  }
  if(isFALSE(is.character(x))){
    stop(paste("Invalid argument: the type of", x_output_name, "must be character."))
  }
  if(isFALSE(case_sensitive)){
    if(isFALSE(tolower(x) %in% tolower(allowed))){
      stop(paste("Invalid argument:", x_output_name, "=", paste0("'", x, "'"), "is not allowed."))
    }
  } else {
    if(isFALSE(x %in% allowed)){
      stop(paste("Invalid argument:", x_output_name, "=", paste0("'", x, "'"), "is not allowed."))
    }
  }
}

#' @title Validate character vectors
#'
#' @description `inspect_character` checks if an object is a character vector. This can be useful to validate inputs in user-defined functions.
#'
#' @param x An arbitrary object.
#' @param allow_nas Logical value. If `TRUE` then `NA` and `NaN` values in `x` are allowed. If `FALSE`, execution is stopped and an error message is thrown in case there are `NA` or `NaN` values in `x`.
#' @param warning_nas Logical value. If `TRUE` then the presence of `NA` or `NaN` values in `x` generates a warning message. `NA` and `NaN` values pass silently otherwise (if `allow_nas` is set to `TRUE`).
#'
#' @details `inspect_character` conducts a series of tests to check if `x` is a character vector. Namely, `inspect_character` checks if:
#' * `x` is `NULL` or empty.
#' * `x` is an atomic vector.
#' * The \code{\link[base]{typeof}} `x` is character.
#' * There are `NA` or `NaN` values in `x`.
#'
#' @return `inspect_character` does not return any output. There are three possible outcomes:
#' * The call is silent if:
#'   * `x` is a character vector and there are no `NA` or `NaN` values in `x`.
#'   * `x` is a character vector, there are some `NA` or `NaN` values in `x`, `allow_nas` is set to `TRUE` and `warning_nas` is set to `FALSE`.
#' * An informative warning message is thrown if `x` is a character vector, there are some `NA` or `NaN` values in `x` and both `allow_nas` and `warning_nas` are set to `TRUE`.
#' * An informative error message is thrown if:
#'   * `x` is not a character vector.
#'   * `x` is a character vector, there are some `NA` or `NaN` values in `x` and `allow_nas` is set to `FALSE`.
#'
#' @seealso
#' * \code{\link[inspector]{inspect_character_match}} to validate character vectors with predefined allowed values.
#' * \code{\link[inspector]{inspect_true_or_false}} to check if an object is a non-missing logical value.
#'
#' @examples
#' # Calls that pass silently:
#' x1 <- "Kass"
#' x2 <- c("Kass", "Raftery")
#' x3 <- c("Kass", "Raftery", NA)
#' x4 <- letters
#' inspect_character(x1)
#' inspect_character(x2)
#' inspect_character(x3)
#' inspect_character(x4)
#'
#' # Call that throws an informative warning message
#' y <- c("Kass", "Raftery", NA)
#' try(inspect_character(y, warning_nas = TRUE))
#'
#' # Calls that throw informative error messages
#' try(inspect_character(y, allow_nas = FALSE))
#' mylist <- list(NULL, character(0), 1,
#'  c(1, 2), factor(c(1,2)), list(c(1,2)), NaN, NA)
#' try(inspect_character(mylist[[1]]))
#' try(inspect_character(mylist[[2]]))
#' try(inspect_character(mylist[[3]]))
#' try(inspect_character(mylist[[4]]))
#' try(inspect_character(mylist[[5]]))
#' try(inspect_character(mylist[[6]]))
#' try(inspect_character(mylist[[7]]))
#' try(inspect_character(mylist[[8]]))
#'
#' @export

inspect_character <- function(x, allow_nas = TRUE, warning_nas = FALSE){

  inspect_true_or_false(allow_nas)
  inspect_true_or_false(warning_nas)

  output_name <- deparse(substitute(x))

  if(is.null(x)){
    stop(paste("Invalid argument:", output_name, "is NULL."))
  }
  if(any(isFALSE(is.atomic(x)), isFALSE(is.vector(x)))){
    stop(paste("Invalid argument:", output_name, "must be an atomic vector."))
  }
  if(length(x) == 0){
    stop(paste("Invalid argument:", output_name, "is empty."))
  }
  if(all(is.na(x))){
    stop(paste("Invalid argument: all elements of", output_name, "are NA or NaN."))
  }
  if(any(is.na(x))){
    if(isFALSE(allow_nas)) {
      stop(paste("Invalid argument: There are NA or NaN values in", paste0(output_name, ".")))
    } else {
      if(isTRUE(warning_nas)){
        warning(paste("There are NA or NaN values in", paste0(output_name, ".")))
      }
    }
  }
  if(isFALSE(is.character(x))){
    stop(paste("Invalid argument: the type of", output_name, "must be character."))
  }
}

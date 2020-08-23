
#' @title Check if an object is a vector of valid probability values
#'
#' @description `inspect_prob` checks if an object is a numeric vector of valid probability values. This can be useful to validate inputs, intermediate calculations or outputs in user-defined functions.
#'
#' @param x An arbitrary object.
#' @param allow_nas Logical value. If `TRUE` then `NA` and `NaN` values in `x` are allowed. If `FALSE`, execution is stopped and an error message is thrown in case there are `NA` or `NaN` values in `x`.
#' @param warning_nas Logical value. If `TRUE` then the presence of `NA` or `NaN` values in `x` generates a warning message. `NA` and `NaN` pass silently otherwise (if `allow_nas` is `TRUE`).
#'
#' @details `inspect_prob` conducts a series of tests to check if `x` is a numeric vector of valid probability values. Namely, `inspect_prob` checks if:
#' * `x` is `NULL` or empty.
#' * `x` is a numeric, atomic vector.
#' * `x` has `NA` or `NaN` values.
#' *  The values of `x` are in the \[0, 1\] interval.
#'
#' @return `inspect_prob` does not return any output. There are three possible outcomes:
#' * The call is silent if:
#'   * `x` is a numeric vector of valid probability values without `NA` or `NaN` values.
#'   * `x` is a numeric vector of valid probability values with some `NA` or `NaN` values, `allow_nas` is set to `TRUE` and `warning_nas` is set to `FALSE`.
#' * An informative warning message is thrown if `x` is a numeric vector of valid probability values, there are `NA` or `NaN` values in `x` and both `allow_nas` and `warning_nas` are set to `TRUE`.
#' * An informative error message is thrown and the execution is stopped if:
#'   * `x` is not a numeric vector of valid probability values.
#'   * `x` is a numeric vector of valid probability values with some `NA` or `NaN` values and `allow_nas` is set to `FALSE`.
#'
#' @seealso
#' * \code{\link[inspector]{inspect_bf}} to check if an object is a numeric vector of valid Bayes factor values.
#' * \code{\link[inspector]{inspect_log_bf}} to check if an object is a numeric vector of valid logarithmic Bayes factor values.
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
#' \dontrun{y <- c(0.1, 0.2, NA, 0.4, 0.5)}
#' \dontrun{inspect_prob(y)}
#' \dontrun{inspect_prob(y, allow_nas = TRUE)}
#' \dontrun{inspect_prob(y, allow_nas = TRUE, warning_nas = TRUE)}
#'
#' # Calls that throw an informative error message:
#' \dontrun{z1 <- c(-0.9, 0, 0.1, 0.2, 0.3, 0.4, 0.5)}
#' \dontrun{inspect_prob(z1)}
#' \dontrun{z2 <- c(NA, 0, 0.1, 0.2, 0.3, 0.4, 0.5)}
#' \dontrun{inspect_prob(z2, allow_nas = FALSE)}
#' \dontrun{mylist <- list(NULL, TRUE, factor(.5), matrix(0.5),
#'          "0.5", list(0.5), NA, NaN, numeric(0), 1.1, -0.5)}
#' \dontrun{inspect_prob(mylist[[1]])}
#' \dontrun{inspect_prob(mylist[[2]])}
#' \dontrun{inspect_prob(mylist[[3]])}
#' \dontrun{inspect_prob(mylist[[4]])}
#' \dontrun{inspect_prob(mylist[[5]])}
#' \dontrun{inspect_prob(mylist[[6]])}
#' \dontrun{inspect_prob(mylist[[7]])}
#' \dontrun{inspect_prob(mylist[[8]])}
#' \dontrun{inspect_prob(mylist[[9]])}
#' \dontrun{inspect_prob(mylist[[10]])}
#' \dontrun{inspect_prob(mylist[[11]])}
#'
#' @export

inspect_prob <- function(x, allow_nas = TRUE, warning_nas = TRUE){

  inspect_true_or_false(allow_nas)
  inspect_true_or_false(warning_nas)

  output_name <- paste0("'", deparse(substitute(x)), "'")

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

#' @title Check if an object is a numeric vector of valid Bayes factor values
#'
#' @description `inspect_bf` checks if an object is a numeric vector of valid Bayes factor values. This can be useful to validate inputs, intermediate calculations or outputs in user-defined functions.
#'
#' @param x An arbitrary object.
#' @param allow_nas Logical value. If `TRUE` then `NA` and `NaN` values in `x` are allowed. If `FALSE`, execution is stopped and an error message is thrown in case there are `NA` or `NaN` values in `x`.
#' @param warning_nas Logical value. If `TRUE` then the presence of `NA` or `NaN` values in `x` generates a warning message. `NA` and `NaN` pass silently otherwise (if `allow_nas` is `TRUE`).
#'
#' @details `inspect_bf` conducts a series of tests to check if `x` is a numeric vector of valid Bayes factor values. Namely, `inspect_bf` checks if:
#' * `x` is `NULL` or empty.
#' * `x` is a numeric, atomic vector.
#' * `x` has `NA` or `NaN` values.
#' *  The values of `x` are non-negative.
#'
#' @return `inspect_bf` does not return any output. There are three possible outcomes:
#' * The call is silent if:
#'   * `x` is a numeric vector of valid Bayes factor values without `NA` or `NaN` values.
#'   * `x` is a numeric vector of valid Bayes factor values with some `NA` or `NaN` values, `allow_nas` is set to `TRUE` and `warning_nas` is set to `FALSE`.
#' * An informative warning message is given if `x` is a numeric vector of valid Bayes factor values, there are `NA` or `NaN` values in `x` and both  `allow_nas` and `warning_nas` are set to `TRUE`.
#' * An informative error message is thrown and the execution is stopped if:
#'   * `x` is not a numeric vector of valid Bayes factor values.
#'   * `x` is a numeric vector of valid Bayes factor values with some `NA` or `NaN` values and `allow_nas` is set to `FALSE`.
#'
#' @seealso
#' * \code{\link[inspector]{inspect_log_bf}} to check if an object is a numeric vector of valid logarithmic Bayes factor values.
#' * \code{\link[inspector]{inspect_prob}} to check if an object is a numeric vector of valid probability values.
#'
#' @examples
#' # Calls that pass silently:
#' x1 <- c(0, 0.5, 1, 10, 50, 100)
#' x2 <- c(NA, 0.5, 1, 10, 50, 100)
#' inspect_bf(x1)
#' inspect_bf(x2, warning_nas = FALSE)
#' inspect_bf(x2, allow_nas = TRUE, warning_nas = FALSE)
#'
#' # Call that throws an informative warning message:
#' \dontrun{y <- c(0.1, 0.2, NA, 0.4, 0.5)}
#' \dontrun{inspect_bf(y)}
#' \dontrun{inspect_bf(y, warning_nas = TRUE)}
#' \dontrun{inspect_bf(y, allow_nas = TRUE, warning_nas = TRUE)}
#'
#' # Calls that throw informative error messages:
#' \dontrun{z <- c(-0.9, 0, 0.1, 0.2, 0.3, 0.4, 0.5)}
#' \dontrun{inspect_bf(z)}
#' \dontrun{mylist <- list(NULL, TRUE, factor(.5), matrix(0.5),
#'          "0.5", list(0.5), NA, NaN, numeric(0), -0.5, -5)}
#' \dontrun{inspect_bf(mylist[[1]])}
#' \dontrun{inspect_bf(mylist[[2]])}
#' \dontrun{inspect_bf(mylist[[3]])}
#' \dontrun{inspect_bf(mylist[[4]])}
#' \dontrun{inspect_bf(mylist[[5]])}
#' \dontrun{inspect_bf(mylist[[6]])}
#' \dontrun{inspect_bf(mylist[[7]])}
#' \dontrun{inspect_bf(mylist[[8]])}
#' \dontrun{inspect_bf(mylist[[9]])}
#' \dontrun{inspect_bf(mylist[[10]])}
#' \dontrun{inspect_bf(mylist[[11]])}
#'
#' @export

inspect_bf <- function(x, allow_nas = TRUE, warning_nas = TRUE){

  inspect_true_or_false(allow_nas)
  inspect_true_or_false(warning_nas)

  output_name <- paste0("'", deparse(substitute(x)), "'")

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

#' @title Check if an object is a numeric vector of valid logarithmic Bayes factor values
#'
#' @description `inspect_log_bf` checks if an object is a numeric vector of valid logarithmic Bayes factor values.  This can be useful to validate inputs, intermediate calculations or outputs in user-defined functions.
#'
#' @param x An arbitrary object.
#' @param allow_nas Logical value. If `TRUE` then `NA` and `NaN` values in `x` are allowed. If `FALSE`, execution is stopped and an error message is thrown in case there are `NA` or `NaN` values in `x`.
#' @param warning_nas Logical value. If `TRUE` then the presence of `NA` or `NaN` values in `x` generates a warning message. `NA` and `NaN` pass silently otherwise (if `allow_nas` is `TRUE`).
#'
#' @details `inspect_log_bf` conducts a series of tests to check if `x` is a numeric vector of valid logarithmic Bayes factor values. Namely, `inspect_log_bf` checks if:
#' * `x` is `NULL` or empty.
#' * `x` is a numeric, atomic vector.
#' * `x` has `NA` or `NaN` values.
#'
#' @return `inspect_log_bf` does not return any output. There are three possible outcomes:
#' * The call is silent if:
#'   * `x` is a numeric vector of valid logarithmic Bayes factor values without `NA` or `NaN` values.
#'   * `x` is a numeric vector of valid logarithmic Bayes factor values with some `NA` or `NaN` values, `allow_nas` is set to `TRUE` and `warning_nas` is set to `FALSE`.
#' * An informative warning message is given if `x` is a numeric vector of valid logarithmic Bayes factor values, there are `NA` or `NaN` values in `x` and both  `allow_nas` and `warning_nas` are set to `TRUE`.
#' * An informative error message is thrown and the execution is stopped if:
#'   * `x` is not a numeric vector of valid logarithmic Bayes factor values.
#'   * `x` is a numeric vector of valid logarithmic Bayes factor values with some `NA` or `NaN` values and `allow_nas` is set to `FALSE`.
#'
#' @seealso
#' * \code{\link[inspector]{inspect_bf}} to check if an object is a numeric vector of valid Bayes factor values.
#' * \code{\link[inspector]{inspect_prob}} to check if an object is a numeric vector of valid probability values.
#'
#' @examples
#' # Calls that pass silently:
#' x1 <- c(0, 0.5, 1, 10, 50, 100)
#' x2 <- c(NA, 0.5, 1, 10, 50, 100)
#' inspect_log_bf(x1)
#' inspect_log_bf(x2, warning_nas = FALSE)
#' inspect_log_bf(x2, allow_nas = TRUE, warning_nas = FALSE)
#'
#' # Call that throws an informative warning message:
#' \dontrun{y <- c(0.1, 0.2, NA, 0.4, 0.5)}
#' \dontrun{inspect_log_bf(y)}
#' \dontrun{inspect_log_bf(y, warning_nas = TRUE)}
#' \dontrun{inspect_log_bf(y, allow_nas = TRUE, warning_nas = TRUE)}
#'
#' # Calls that throw informative error messages:
#' \dontrun{mylist <- list(NULL, TRUE, factor(.5), matrix(0.5),
#'          "0.5", list(0.5), numeric(0), NA, NaN)}
#' \dontrun{inspect_log_bf(mylist[[1]])}
#' \dontrun{inspect_log_bf(mylist[[2]])}
#' \dontrun{inspect_log_bf(mylist[[3]])}
#' \dontrun{inspect_log_bf(mylist[[4]])}
#' \dontrun{inspect_log_bf(mylist[[5]])}
#' \dontrun{inspect_log_bf(mylist[[6]])}
#' \dontrun{inspect_log_bf(mylist[[7]])}
#' \dontrun{inspect_log_bf(mylist[[8]])}
#' \dontrun{inspect_log_bf(mylist[[9]])}
#'
#' @export

inspect_log_bf <- function(x, allow_nas = TRUE, warning_nas = TRUE){

  inspect_true_or_false(allow_nas)
  inspect_true_or_false(warning_nas)

  output_name <- paste0("'", deparse(substitute(x)), "'")

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

#' @title Check if an object is a valid logarithmic base
#'
#' @description `inspect_log_base` checks if an object is a numeric vector of \code{\link[base]{length}} 1 that is eligible to be used as a logarithmic base. This can be useful to validate inputs in user-defined functions.
#'
#' @param x An arbitrary object.
#'
#' @details `inspect_log_base` conducts a series of tests to check if `x` is a valid logarithmic base. Namely, `inspect_log_base` checks if:
#' * `x` is `NULL` or empty.
#' * `x` is a numeric, atomic vector of \code{\link[base]{length}} 1.
#' * `x` is `NA` or `NaN`.
#' * `x` is positive.
#'
#' @return `inspect_log_base` does not return any output. There are two possible outcomes:
#' * The call is silent if `x` is a numeric vector of \code{\link[base]{length}} 1 that is a valid logarithmic base.
#' * An informative error message is thrown otherwise.
#'
#' @seealso
#' * \code{\link[inspector]{inspect_prob}} to check if an object is a numeric vector of valid probability values.
#' * \code{\link[inspector]{inspect_bf}} to check if an object is a numeric vector of valid Bayes factor values.
#' * \code{\link[inspector]{inspect_log_bf}} to check if an object is a numeric vector of valid logarithmic Bayes factor values.
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
#' \dontrun{mylist <- list(NULL, numeric(0), TRUE, factor(10),
#'         list(10), matrix(10), NaN, NA, -1, 0)}
#' \dontrun{inspect_log_base(mylist[[1]])}
#' \dontrun{inspect_log_base(mylist[[2]])}
#' \dontrun{inspect_log_base(mylist[[3]])}
#' \dontrun{inspect_log_base(mylist[[4]])}
#' \dontrun{inspect_log_base(mylist[[5]])}
#' \dontrun{inspect_log_base(mylist[[6]])}
#' \dontrun{inspect_log_base(mylist[[7]])}
#' \dontrun{inspect_log_base(mylist[[8]])}
#' \dontrun{inspect_log_base(mylist[[9]])}
#' \dontrun{inspect_log_base(mylist[[10]])}
#'
#' @export

inspect_log_base <- function(x){

  output_name <- paste0("'", deparse(substitute(x)), "'")

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

#' @title Check if an object is a valid Bayes factor interpretation scale
#'
#' @description `inspect_scale` checks if an object is a character vector of \code{\link[base]{length}} 1 that is eligible to represent one of the Bayes factor interpretation scales available in the `pcal` package. This can be useful to validate inputs in user-defined functions.
#'
#' @param x An arbitrary object.
#'
#' @details `inspect_scale` conducts a series of tests to check if `x` is a character vector of \code{\link[base]{length}} 1 that is eligible to represent one of the Bayes factor interpretation scales available in the `pcal` package. Namely, `inspect_scale` checks if:
#' * `x` is `NULL` or empty.
#' * `x` is `NA` or `NaN`.
#' * `x` is an atomic character vector of \code{\link[base]{length}} 1 specifying either "Jeffreys" or "Kass-Raftery" (not case sensitive).
#'
#' @return `inspect_scale` does not return any output. There are two possible scenarios:
#' * The call is silent if `x` is a character vector of \code{\link[base]{length}} 1 that is eligible to represent one of the Bayes factor interpretation scales available in the `pcal` package.
#' * An informative error message is thrown otherwise.
#'
#' @seealso
#' * \code{\link[pcal]{bfactor_interpret}} for the interpretation of Bayes factors.
#' * \code{\link[pcal]{bfactor_log_interpret}} for the interpretation of the logarithms of Bayes factors.
#' * \code{\link[inspector]{inspect_bf}} to check if an object is a numeric vector of valid Bayes factor values.
#' * \code{\link[inspector]{inspect_log_bf}} to check if an object is a numeric vector of valid logarithmic Bayes factor values.
#' * \code{\link[inspector]{inspect_log_base}} to check if an object is a numeric vector of \code{\link[base]{length}} 1 representing a valid logarithmic base.
#'
#' @examples
#' # Calls that pass silently:
#' x1 <- "Jeffreys"
#' x2 <- "jeffreys"
#' x3 <- "kass-raftery"
#' x4 <- "Kass-Raftery"
#' inspect_scale(x1)
#' inspect_scale(x2)
#' inspect_scale(x3)
#' inspect_scale(x4)
#'
#' # Calls that throw informative error messages:
#' \dontrun{mylist <- list(NULL, NA, NaN, 10, "Bayes", "Jeff",
#'           "kassraftery", c("jeffreys", "kass-raftery"))}
#' \dontrun{inspect_scale(mylist[[1]])}
#' \dontrun{inspect_scale(mylist[[2]])}
#' \dontrun{inspect_scale(mylist[[3]])}
#' \dontrun{inspect_scale(mylist[[4]])}
#' \dontrun{inspect_scale(mylist[[5]])}
#' \dontrun{inspect_scale(mylist[[6]])}
#' \dontrun{inspect_scale(mylist[[7]])}
#' \dontrun{inspect_scale(mylist[[8]])}
#'
#' @export

inspect_scale <- function(x){

  output_name <- paste0("'", deparse(substitute(x)), "'")

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

#' @title Check if an object is a non-missing logical value
#'
#' @description `inspect_true_or_false` checks if an object is a logical vector of \code{\link[base]{length}} 1 with value equal to `TRUE` or `FALSE`. This can be useful to validate inputs in user-defined functions.
#'
#' @param x An arbitrary object.
#'
#' @details `inspect_true_or_false` conducts a series of tests to check if `x` is a logical vector of \code{\link[base]{length}} 1 with value equal to `TRUE` or `FALSE`. Namely, `inspect_true_or_false` checks if:
#' * `x` is `NULL` or empty.
#' * `x` is an atomic logical vector of \code{\link[base]{length}} 1.
#' * `x` is `NA` or `NaN`.
#'
#' @return `inspect_true_or_false` does not return any output. There are two possible scenarios:
#' * The call is silent if `x` is  a logical vector of \code{\link[base]{length}} 1 with value equal to `TRUE` or `FALSE`.
#' * An informative error message is thrown otherwise.
#'
#' @seealso
#' * \code{\link[inspector]{inspect_prob}} to check if an object is a numeric vector of valid probability values.
#' * \code{\link[inspector]{inspect_bf}} to check if an object is a numeric vector of valid Bayes factor values.
#'
#' @examples
#' # Calls that pass silently:
#' x <- TRUE
#' y <- FALSE
#' inspect_true_or_false(x)
#' inspect_true_or_false(y)
#'
#' # Calls that throw informative error messages:
#' \dontrun{mylist <- list(NULL, NA, NaN, 1, 0, "TRUE")}
#' \dontrun{inspect_true_or_false(mylist[[1]])}
#' \dontrun{inspect_true_or_false(mylist[[2]])}
#' \dontrun{inspect_true_or_false(mylist[[3]])}
#' \dontrun{inspect_true_or_false(mylist[[4]])}
#' \dontrun{inspect_true_or_false(mylist[[5]])}
#' \dontrun{inspect_true_or_false(mylist[[6]])}
#'
#' @export

inspect_true_or_false <- function(x){

  output_name <- paste0("'", deparse(substitute(x)), "'")

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

#' @title Check if an object is eligible to be used as the levels of a factor
#'
#' @description `inspect_categories` checks if an object is eligible to be used as the levels of a factor. This can be useful to validate inputs in user-defined functions.
#'
#' @param x An arbitrary object.
#'
#' @details `inspect_categories` conducts a series of tests to check if `x` is eligible to be used as the levels of a factor. Namely, `inspect_categories` checks if:
#' * `x` is `NULL` or empty.
#' * `x` is atomic.
#' * The \code{\link[base]{typeof}} `x` is logical, integer, double or character.
#' * There are `NA` or `NaN` values in `x`.
#' * There are repeated values in `x`.
#'
#' @return `inspect_categories` does not return any output. There are two possible outcomes:
#' * The call is silent if `x` is eligible to be used as the levels of a factor.
#' * An informative error message is thrown otherwise.
#'
#' @seealso
#' * \code{\link[inspector]{inspect_data_binomial}} to validate dichotomous data.
#' * \code{\link[inspector]{inspect_data_multinomial}} and \code{\link[inspector]{inspect_data_multinom_as_bern}} to validate categorical data.
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
#' \dontrun{y1 <- c(1, 1:5)}
#' \dontrun{y2 <- c("yes", "no", "yes")}
#' \dontrun{y3 <- factor(c("yes", "no", "yes"))}
#' \dontrun{inspect_categories(y1)}
#' \dontrun{inspect_categories(y2)}
#' \dontrun{inspect_categories(y3)}
#' \dontrun{mylist <- list(NULL, numeric(0),
#'  complex(1), list(10), NaN, NA)}
#' \dontrun{inspect_categories(mylist[[1]])}
#' \dontrun{inspect_categories(mylist[[2]])}
#' \dontrun{inspect_categories(mylist[[3]])}
#' \dontrun{inspect_categories(mylist[[4]])}
#' \dontrun{inspect_categories(mylist[[5]])}
#' \dontrun{inspect_categories(mylist[[6]])}
#'
#' @export

inspect_categories <- function(x){

  output_name <- paste0("'", deparse(substitute(x)), "'")

  if(is.null(x)){
    stop(paste("Invalid argument:", output_name, "is NULL."))
  }
  if(isFALSE(is.atomic(x))){
    stop(paste("Invalid argument:", output_name, "must be of an atomic type."))
  }
  if(isTRUE(length(x) == 0)){
    stop(paste("Invalid argument:", output_name, " is empty."))
  }
  if(isFALSE(typeof(x) %in% c("logical", "integer", "double", "character"))){
    stop(paste("Invalid argument: the type of", output_name, "must be 'logical', 'integer', 'double' or 'character'."))
  }
  if(any(is.na(x))){
    stop(paste("Invalid argument: there are NA or NaN values in",  paste0(output_name, ".")))
  }
  if(isFALSE(length(unique(x)) == length(x))){
    stop(paste("Invalid argument: all element of", output_name, " must be unique."))
  }
}

#' @title Check if an object is a character value that belongs to a set of allowed values
#'
#' @description `inspect_character_match` checks if an object is a character value that belongs to a set of allowed values. This can be useful to validate inputs in user-defined functions.
#'
#' @param x An arbitrary object.
#' @param allowed A character vector.
#' @param case_sensitive A non-missing logical value.
#'
#' @details `inspect_character_match` conducts a series of tests to check if `x` is a character value that belongs to a set of allowed values. Namely, `inspect_character_match` checks if:
#' * `x` is `NULL` or empty.
#' * `x` is an atomic character vector of \code{\link[base]{length}} 1.
#' * `x` is `NA` or `NaN`.
#' * `x` is one of the allowed values (as specified in the `allowed` argument).
#'
#' By default, the comparison of `x` with `allowed` is not case sensitive. If you only want case sensitive matches of `x` to `allowed` set `case_sensitive` to `TRUE`.
#'
#' @return `inspect_character_match` does not return any output. There are two possible outcomes:
#' * The call is silent if `x` is a character value that belongs to the set of allowed values.
#' * An informative error message is thrown otherwise.
#'
#' @seealso
#' * \code{\link[inspector]{inspect_true_or_false}} to check if an object is a non-missing logical value.
#' * \code{\link[inspector]{inspect_categories}} to check if an object is eligible to be used as the levels of a factor.
#'
#' @examples
#' # Calls that pass silently:
#' x1 <- "Kass"
#' x2 <- "kass"
#' inspect_character_match(x1, allowed = c("Kass", "Raftery"))
#' inspect_character_match(x2, allowed = c("Kass", "Raftery"))
#'
#' # Calls that throw informative error messages:
#' \dontrun{y1 <- "kasss"}
#' \dontrun{y2 <- "kass"}
#' \dontrun{inspect_character_match(y1, allowed = c("Kass", "Raftery"))}
#' \dontrun{inspect_character_match(y2, allowed = c("Kass", "Raftery"), case_sensitive = TRUE)}
#' \dontrun{mylist <- list(NULL, character(0), c("Kass", "Raftery"),
#'  1, "1", list(1), NaN, NA)}
#' \dontrun{inspect_character_match(mylist[[1]])}
#' \dontrun{inspect_character_match(mylist[[2]])}
#' \dontrun{inspect_character_match(mylist[[3]])}
#' \dontrun{inspect_character_match(mylist[[4]])}
#' \dontrun{inspect_character_match(mylist[[5]])}
#' \dontrun{inspect_character_match(mylist[[6]])}
#' \dontrun{inspect_character_match(mylist[[7]])}
#'
#' @export

inspect_character_match <- function(x, allowed, case_sensitive = FALSE){

  inspect_character(allowed)
  inspect_true_or_false(case_sensitive)

  x_output_name <- paste0("'", deparse(substitute(x)), "'")


  if(is.null(x)){
    stop(paste("Invalid argument:", x_output_name, "is NULL."))
  }
  if(any(isFALSE(is.atomic(x)), isFALSE(is.vector(x)), isFALSE(is.character(x)), isFALSE(length(x) == 1))){
    stop(paste("Invalid argument:", x_output_name, "must be an atomic vector of type character and length 1."))
  }
  if(any(is.na(x))){
    stop(paste("Invalid argument:", x_output_name, "is NA or NaN"))
  }
  if(isFALSE(case_sensitive)){
    if(isFALSE(tolower(x) %in% tolower(allowed))){
      stop(paste("Invalid argument:", deparse(substitute(x)), "=", x, "is not allowed."))
    }
  } else {
    if(isFALSE(x %in% allowed)){
      stop(paste("Invalid argument:", deparse(substitute(x)), "=", x, "is not allowed."))
    }
  }

}

#' @title Check if an object is a character value
#'
#' @description `inspect_character` checks if an object is a character value. This can be useful to validate inputs in user-defined functions.
#'
#' @param x An arbitrary object.
#' @param allowed A character vector.
#' @param case_sensitive A non-missing logical value.
#'
#' @details `inspect_character` conducts a series of tests to check if `x` is a character value that belongs to a set of allowed values. Namely, `inspect_character` checks if:
#' * `x` is `NULL` or empty.
#' * `x` is an atomic character vector of \code{\link[base]{length}} 1.
#' * `x` is `NA` or `NaN`.
#' * `x` is one of the allowed values (as specified in the `allowed` argument).
#'
#' By default, the comparison of `x` with `allowed` is not case sensitive. If you only want case sensitive matches of `x` to `allowed` set `case_sensitive` to `TRUE`.
#'
#' @return `inspect_character` does not return any output. There are two possible outcomes:
#' * The call is silent if `x` is a character value that belongs to the set of allowed values.
#' * An informative error message is thrown otherwise.
#'
#' @seealso
#' * \code{\link[inspector]{inspect_true_or_false}} to check if an object is a non-missing logical value.
#' * \code{\link[inspector]{inspect_categories}} to check if an object is eligible to be used as the levels of a factor.
#'
#' @examples
#' # Calls that pass silently:
#' x1 <- "Kass"
#' x2 <- "kass"
#' inspect_character(x1, allowed = c("Kass", "Raftery"))
#' inspect_character(x2, allowed = c("Kass", "Raftery"))
#'
#' # Calls that throw informative error messages:
#' \dontrun{y1 <- "kasss"}
#' \dontrun{y2 <- "kass"}
#' \dontrun{inspect_character(y1, allowed = c("Kass", "Raftery"))}
#' \dontrun{inspect_character(y2, allowed = c("Kass", "Raftery"), case_sensitive = TRUE)}
#' \dontrun{mylist <- list(NULL, character(0), c("Kass", "Raftery"),
#'  1, "1", list(1), NaN, NA)}
#' \dontrun{inspect_character(mylist[[1]])}
#' \dontrun{inspect_character(mylist[[2]])}
#' \dontrun{inspect_character(mylist[[3]])}
#' \dontrun{inspect_character(mylist[[4]])}
#' \dontrun{inspect_character(mylist[[5]])}
#' \dontrun{inspect_character(mylist[[6]])}
#' \dontrun{inspect_character(mylist[[7]])}
#'
#' @export

inspect_character <- function(x, warning_nas = FALSE){

  output_name <- paste0("'", deparse(substitute(x)), "'")

  if(is.null(x)){
    stop(paste("Invalid argument:", output_name, "is NULL."))
  }
  if(any(isFALSE(is.atomic(x)), isFALSE(is.vector(x)), isFALSE(is.character(x)))){
    stop(paste("Invalid argument:", output_name, "must be a character vector."))
  }
  if(all(is.na(x))){
    stop(paste("Invalid argument: all elements of", output_name, "are NA or NaN."))
  }
  if(isTRUE(warning_nas)){
    if(any(is.na(x))){
      warning(paste("There are NA or NaN values in", paste0(output_name, ".")))
    }
  }
}













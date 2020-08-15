
#' @title Check if an object is a vector of valid probability values
#'
#' @description `inspect_prob` checks if an object is a numeric vector of valid probability values. This can be useful to validate inputs, intermediate calculations or outputs in user-defined functions.
#'
#' @param x An arbitrary object.
#' @param allow_nas Logical value indicating either `TRUE` or `FALSE`. If `FALSE` then the execution is stopped (via \code{\link[base]{stop}}) in case there are `NA` or `NaN` values in `x`.
#'
#' @details `inspect_prob` conducts a series of tests to check if `x` is a numeric vector of valid probability values. Namely, `inspect_prob` checks if:
#' * `x` is `NULL` or empty.
#' * `x` is a numeric, atomic vector.
#' * `x` has `NA` or `NaN` values.
#' *  The values of `x` are in the \[0, 1\] interval.
#'
#' @return `inspect_prob` does not return any output. There are three possible outcomes:
#' * The call is silent if `x` is a numeric vector of valid probability values without `NA` or `NaN` values.
#' * An informative warning message is thrown if `x` is a numeric vector of valid probability values with some `NA` or `NaN` values and `allow_nas` is set to `TRUE`.
#' * An informative error message is thrown and the execution is stopped if:
#'   * `x` is not a numeric vector of valid probability values.
#'   * `x` is a numeric vector of valid probability values with some `NA` or `NaN` values and `allow_nas` is set to `FALSE`.
#'
#' @seealso
#' * \code{\link[inspect]{inspect_bf}} to check if an object is a numeric vector of valid Bayes factor values.
#' * \code{\link[inspect]{inspect_log_bf}} to check if an object is a numeric vector of valid logarithmic Bayes factor values.
#' * \code{\link[inspect]{inspect_log_base}} to check if an object is a numeric vector of \code{\link[base]{length}} 1 representing a valid logarithmic base.
#' * \code{\link[inspect]{inspect_scale}} to check if an object is a string of characters representing one of the Bayes factor interpretation scales available in the \code{pcal} package.
#'
#' @examples
#' # Calls that pass silently:
#' x <- c(0.1, 0.2, 0.3, 0.4, 0.5)
#' inspect_prob(x)
#' inspect_prob(seq(0, 1, 0.1))
#'
#' # Calls that throw an informative warning message:
#' y <- c(0.1, 0.2, NA, 0.4, 0.5)
#' \dontrun{inspect_prob(y)}
#' \dontrun{inspect_prob(y, allow_nas = TRUE)}
#'
#' # Calls that throw an informative error message:
#' z1 <- c(-0.9, 0, 0.1, 0.2, 0.3, 0.4, 0.5)
#' \dontrun{inspect_prob(z1)}
#' z2 <- c(-0.9, 0, 0.1, 0.2, 0.3, 0.4, 0.5)
#' \dontrun{inspect_prob(z2, allow_nas = FALSE)}
#' mylist <- list(NULL, TRUE, factor(.5), matrix(0.5),
#'          "0.5", list(0.5), NA, NaN, 1.1, .5)
#' \dontrun{inspect_prob(mylist)}
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
#'
#' @export

inspect_prob <- function(x, allow_nas = TRUE){

  output_name <- paste0("'", deparse(substitute(x)), "'")

  x_filtered <- x[!is.na(x)]

  if(is.null(x)){
    stop(paste("Invalid argument:", output_name, "is NULL."))
  }
  if(any(!is.numeric(x), !is.vector(x))){
    stop(paste("Invalid argument:", output_name, "must be a numeric vector."))
  }
  if(length(x) == 0){
    stop(paste("Invalid argument:", output_name, "is empty."))
  }
  if(all(is.na(x))){
    stop(paste("Invalid argument: all elements of", output_name,  "are NA are NaN."))
  }
  if(any(is.na(x))){
    if(isTRUE(allow_nas)){
      warning(paste("There are NA or NaN values in", paste0(output_name, ".")))
    } else {
      stop(paste("Invalid argument: There are NA or NaN values in ", paste0(output_name, ".")))
    }
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
#'
#' @details `inspect_bf` conducts a series of tests to check if `x` is a numeric vector of valid Bayes factor values. Namely, `inspect_bf` checks if:
#' * `x` is `NULL` or empty.
#' * `x` is a numeric, atomic vector.
#' * `x` has `NA` or `NaN` values.
#' *  The values of `x` are non-negative.
#'
#' @return `inspect_bf` does not return any output. There are three possible outcomes:
#' * The call is silent if `x` is a numeric vector of valid Bayes factor values and there are no `NA` or `NaN` values.
#' * An informative warning message is given if `p` is a numeric vector of valid Bayes factor values and there are `NA` or `NaN` values.
#' * An informative error message is thrown if `p` is not a numeric vector of valid Bayes factor values. This will \code{\link[base]{stop}} the execution.
#'
#' @seealso
#' * \code{\link[inspect]{inspect_log_bf}} to check if an object is a numeric vector of valid logarithmic Bayes factor values.
#' * \code{\link[inspect]{inspect_prob}} to check if an object is a numeric vector of valid probability values.
#' * \code{\link[inspect]{inspect_log_base}} to check if an object is a numeric vector of \code{\link[base]{length}} 1 representing a valid logarithmic base.
#' * \code{\link[inspect]{inspect_scale}} to check if an object is a string of characters representing one of the Bayes factor interpretation scales available in the `pcal` package.
#'
#' @examples
#' # Calls that pass silently:
#' x <- c(0, 0.5, 1, 10, 50, 100)
#' inspect_bf(x)
#' inspect_bf(seq(10, 100, 10))
#'
#' # Call that throws an informative warning message:
#' y <- c(0.1, 0.2, NA, 0.4, 0.5)
#' \dontrun{inspect_bf(y)}
#'
#' # Calls that throw informative error messages:
#' z <- c(-0.9, 0, 0.1, 0.2, 0.3, 0.4, 0.5)
#' \dontrun{inspect_bf(z)}
#' mylist <- list(NULL, TRUE, factor(.5), matrix(0.5),
#'          "0.5", list(0.5), NA, NaN, -0.5, -5)
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
#'
#' @export

inspect_bf <- function(x){

  output_name <- paste0("'", deparse(substitute(x)), "'")

  if(is.null(x)){
    stop(paste("Invalid argument:", output_name, "is NULL."))
  }
  if(any(!is.numeric(x), !is.vector(x))){
    stop(paste("Invalid argument:", output_name, "must be a numeric vector."))
  }
  if(length(x) == 0){
    stop(paste("Invalid argument:", output_name, "is empty."))
  }
  if(all(is.na(x))){
    stop(paste("Invalid argument: all elements of", output_name, "are NA or NaN."))
  }
  if(any(x[!is.na(x)] < 0)){
    stop(paste("Invalid argument: all elements of", output_name, "must be non-negative."))
  }
  if(any(is.na(x))){
    warning(paste("There are NA or NaN values in", paste0(output_name, ".")))
  }
}

#' @title Check if an object is a numeric vector of valid logarithmic Bayes factor values
#'
#' @description `inspect_log_bf` checks if an object is a numeric vector of valid logarithmic Bayes factor values.  This can be useful to validate inputs, intermediate calculations or outputs in user-defined functions.
#'
#' @param x An arbitrary object.
#'
#' @details `inspect_log_bf` conducts a series of tests to check if `x` is a numeric vector of valid logarithmic Bayes factor values. Namely, `inspect_log_bf` checks if:
#' * `x` is `NULL` or empty.
#' * `x` is a numeric, atomic vector.
#' * `x` has `NA` or `NaN` values.
#'
#' @return `inspect_log_bf` does not return any output. There are three possible outcomes:
#' * The call is silent if `x` is a numeric vector of valid logarithmic Bayes factor values and there are no `NA` or `NaN` values.
#' * An informative warning message is given if `x` is a numeric vector of valid logarithmic Bayes factor values and there are `NA` or `NaN` values.
#' * An informative error message is thrown if `x` is not a numeric vector of valid logarithmic Bayes factor values.
#'
#' @seealso
#' * \code{\link[inspect]{inspect_bf}} to check if an object is a numeric vector of valid Bayes factor values.
#' * \code{\link[inspect]{inspect_prob}} to check if an object is a numeric vector of valid probability values.
#' * \code{\link[inspect]{inspect_log_base}} to check if an object is a numeric vector of \code{\link[base]{length}} 1 representing a valid logarithmic base.
#' * \code{\link[inspect]{inspect_scale}} to check if an object is a string of characters representing one of the Bayes factor interpretation scales available in the `pcal` package.
#'
#' @examples
#' # Calls that pass silently:
#' x <- c(-0.9, 0, 0.1, 0.2, 0.3, 0.4, 0.5)
#' y <- seq(10, 100, 10)
#' inspect_log_bf(x)
#' inspect_log_bf(y)
#'
#' # Call that throws an informative warning message:
#' z <- c(0.1, 2, NA, 40, 0.5)
#' \dontrun{inspect_log_bf(z)}
#'
#' # Calls that throw informative error messages:
#' z <- c(-0.9, 0, 0.1, 0.2, 0.3, 0.4, 0.5)
#' \dontrun{inspect_bf(z)}
#' mylist <- list(NULL, TRUE, factor(.5), matrix(0.5),
#'          "0.5", list(0.5), NA, NaN)
#' \dontrun{inspect_log_bf(mylist[[1]])}
#' \dontrun{inspect_log_bf(mylist[[2]])}
#' \dontrun{inspect_log_bf(mylist[[3]])}
#' \dontrun{inspect_log_bf(mylist[[4]])}
#' \dontrun{inspect_log_bf(mylist[[5]])}
#' \dontrun{inspect_log_bf(mylist[[6]])}
#' \dontrun{inspect_log_bf(mylist[[7]])}
#' \dontrun{inspect_log_bf(mylist[[8]])}
#'
#' @export

inspect_log_bf <- function(x){

  output_name <- paste0("'", deparse(substitute(x)), "'")

  if(is.null(x)){
    stop(paste("Invalid argument:", output_name, "is NULL."))
  }
  if(any(!is.numeric(x), !is.vector(x))){
    stop(paste("Invalid argument:", output_name, "must be a numeric vector."))
  }
  if(length(x) == 0){
    stop(paste("Invalid argument:", output_name, "is empty."))
  }
  if(all(is.na(x))){
    stop(paste("Invalid argument: all elements of ", output_name, "are NA or NaN."))
  }
  if(any(is.na(x))){
    warning(paste("There are NA or NaN values in", paste0(output_name, ".")))
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
#'
#' @return `inspect_log_base` does not return any output. There are two possible outcomes:
#' * The call is silent if `x` is a numeric vector of \code{\link[base]{length}} 1 that is a valid logarithmic base.
#' * An informative error message is thrown otherwise.
#'
#' @seealso
#' * \code{\link[inspect]{inspect_prob}} to check if an object is a numeric vector of valid probability values.
#' * \code{\link[inspect]{inspect_bf}} to check if an object is a numeric vector of valid Bayes factor values.
#' * \code{\link[inspect]{inspect_log_bf}} to check if an object is a numeric vector of valid logarithmic Bayes factor values.
#' * \code{\link[inspect]{inspect_scale}} to check if an object is a string of characters representing one of the Bayes factor interpretation scales available in the `pcal` package.
#'
#' @examples
#' # Calls that pass silently:
#' inspect_log_base(10)
#' inspect_log_base(2)
#' inspect_log_base(exp(1))
#' inspect_log_base(0.5)
#'
#' # Calls that throw informative error messages:
#' mylist <- list(NULL, numeric(0), -1, 0, TRUE, 10,
#'                factor(10), list(10), matrix(10), NaN, NA)
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
#' \dontrun{inspect_log_base(mylist[[11]])}
#'
#' @export

inspect_log_base <- function(x){

  output_name <- paste0("'", deparse(substitute(x)), "'")

  if(any(
    is.null(x),
    isFALSE(is.vector(x)),
    isFALSE(is.atomic(x)),
    isFALSE(is.numeric(x)),
    isFALSE(length(x) == 1),
    is.na(x))
    ){
    stop(paste("Invalid argument:", output_name, "must be a numeric vector of length 1."))
  }
  if(isTRUE(x <= 0)){
    stop(paste("Invalid argument:", output_name, "must be positive."))
  }
}

#' @title Check if an object is a string of characters representing an eligible Bayes factor interpretation scale
#'
#' @description `inspect_scale` checks if an object is a string of characters representing one of the Bayes factor interpretation scales available in the `pcal` package. This can be useful to validate inputs in user-defined functions.
#'
#' @param scale An arbitrary object.
#'
#' @details `inspect_scale` conducts a series of tests to check if `scale` is a string of characters representing one of the Bayes factor interpretation scales available in the `pcal` package. Namely, `inspect_scale` checks if:
#' * `scale` is `NULL` or empty.
#' * `scale` is a character, atomic vector of \code{\link[base]{length}} 1 specifying either "Jeffreys" or "Kass-Raftery" (not case sensitive).
#' * `scale` is `NA` or `NaN`.
#'
#' @return `inspect_scale` does not return any output. There are two possible scenarios:
#' * The call is silent if `scale` is a string of characters representing one of the Bayes factor interpretation scales available in the `pcal` package.
#' * An informative error message is thrown otherwise.
#'
#' @seealso
#' * \code{\link[inspect]{inspect_prob}} to check if an object is a numeric vector of valid probability values.
#' * \code{\link[inspect]{inspect_bf}} to check if an object is a numeric vector of valid Bayes factor values.
#' * \code{\link[inspect]{inspect_log_bf}} to check if an object is a numeric vector of valid logarithmic Bayes factor values.
#' * \code{\link[inspect]{inspect_log_base}} to check if an object is a numeric vector of \code{\link[base]{length}} 1 representing a valid logarithmic base.
#'
#' @examples
#' # Calls that pass silently:
#' inspect_scale("Jeffreys")
#' inspect_scale("jeffreys")
#' inspect_scale("kass-raftery")
#' inspect_scale("Kass-Raftery")
#'
#' # Calls that throw informative error messages:
#' \dontrun{inspect_scale(NULL)}
#' \dontrun{inspect_scale(10)}
#' \dontrun{inspect_scale("Bayes")}
#' \dontrun{inspect_scale("jeff")}
#' \dontrun{inspect_scale("kassraftery")}
#' \dontrun{inspect_scale(c("jeffreys", "kass-raftery"))}
#'
#' @export

inspect_scale <- function(scale){

  output_name <- paste0("'", deparse(substitute(scale)), "'")

  if(is.null(scale)){
    stop(paste("Invalid argument:", output_name, "is NULL."))
  }
  if(any(!is.vector(scale), !is.atomic(scale), isFALSE(length(scale) == 1))){
    stop(paste("Invalid argument:", output_name, "must be an atomic vector of length 1."))
  }
  if(is.na(scale)){
    stop(paste("Invalid argument:", output_name, "is NA or NaN."))
  }
  if(!is.character(scale)){
    stop(paste("Invalid argument: the type of", output_name, "must be character."))
  }
  if(isFALSE(tolower(scale) %in% c("jeffreys", "kass-raftery"))){
    stop(paste("Invalid argument:", output_name, "must be either 'jeffreys' or 'kass-raftery'."))
  }
}

inspect_true_or_false <- function(x){

  output_name <- paste0("'", deparse(substitute(x)), "'")

}





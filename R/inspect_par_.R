
#' @title Validate Bernoulli/Binomial proportions
#'
#' @description `inspect_par_bernoulli` checks if an object is an eligible Bernoulli/Binomial proportion. This can be useful to validate inputs, intermediate calculations or outputs in user-defined functions.
#'
#' @param x An arbitrary object.
#'
#' @details `inspect_par_bernoulli` conducts a series of tests to check if `x` is an eligible Bernoulli/Binomial proportion. Namely, `inspect_par_bernoulli` checks if:
#' * `x` is `NULL` or empty.
#' * `x` is an atomic vector
#' * `x` is numeric
#' * `x` has \code{\link[base]{length}} 1
#' * `x` is `NA` or `NaN`.
#' * `x` is in the (0, 1) interval.
#'
#' @return `inspect_par_bernoulli` does not return any output. There are two possible outcomes:
#' * The call is silent if `x` is an eligible Bernoulli/Binomial proportion.
#' * An informative error message is thrown otherwise.
#'
#' @seealso
#' * \code{\link[inspector]{inspect_data_dichotomous}} to validate dichotomous data.
#' * \code{\link[inspector]{inspect_data_categorical}} and \code{\link[inspector]{inspect_data_multinom_as_bern}} to validate categorical data.
#' * \code{\link[inspector]{inspect_data_dichotomous}} to validate dichotomous data.
#' * \code{\link[inspector]{inspect_data_categorical}} and \code{\link[inspector]{inspect_data_multinom_as_bern}} to validate categorical data.
#'
#' @examples
#' # Calls that pass silently:
#' x <- 0.5
#' inspect_par_bernoulli(x)
#' inspect_par_bernoulli(0.1)
#'
#' # Calls that throw an informative error message:
#' \dontrun{mylist <- list(NULL, TRUE, factor(.5), matrix(0.5), "0.5",
#'  list(0.5), NA, NaN, numeric(0), c(0.1, 0.5), -0.5, 1.1)}
#' \dontrun{inspect_par_bernoulli(mylist[[1]])}
#' \dontrun{inspect_par_bernoulli(mylist[[2]])}
#' \dontrun{inspect_par_bernoulli(mylist[[3]])}
#' \dontrun{inspect_par_bernoulli(mylist[[4]])}
#' \dontrun{inspect_par_bernoulli(mylist[[5]])}
#' \dontrun{inspect_par_bernoulli(mylist[[6]])}
#' \dontrun{inspect_par_bernoulli(mylist[[7]])}
#' \dontrun{inspect_par_bernoulli(mylist[[8]])}
#' \dontrun{inspect_par_bernoulli(mylist[[9]])}
#' \dontrun{inspect_par_bernoulli(mylist[[10]])}
#' \dontrun{inspect_par_bernoulli(mylist[[11]])}
#' \dontrun{inspect_par_bernoulli(mylist[[11]])}
#'
#' @export

inspect_par_bernoulli <- function(x){

  output_name <- paste0("'", deparse(substitute(x)), "'")

  if(is.null(x)){
    stop(paste("Invalid argument:", output_name, "is NULL."))
  }
  if(any(isFALSE(is.atomic(x)), isFALSE(is.vector(x)))){
    stop(paste("Invalid argument:", output_name, "must an atomic vector."))
  }
  if(isFALSE(length(x) == 1)){
    stop(paste("Invalid argument:", output_name, "must be of length 1"))
  }
  if(is.na(x)){
    stop(paste("Invalid argument:", output_name,  "is NA or NaN."))
  }
  if(isFALSE(is.numeric(x))){
    stop(paste("Invalid argument:", output_name, "must be numeric"))
  }
  if(any(x >= 1, x <= 0)) {
    stop(paste("Invalid argument:", output_name, "must be in the (0, 1) interval."))
  }
}

#' @title Validate Multinomial proportions
#'
#' @description `inspect_par_multinomial` checks if an object is an eligible vector of Multinomial proportions. This can be useful to validate inputs, intermediate calculations or outputs in user-defined functions.
#'
#' @param x An arbitrary object.
#'
#' @details `inspect_par_multinomial` conducts a series of tests to check if `x` is an eligible vector of Multinomial proportions. Namely, `inspect_par_multinomial` checks if:
#' * `x` is `NULL` or empty.
#' * `x` is an atomic vector
#' * `x` is numeric
#' * `x` has `NA` or `NaN` values.
#' * All elements of `x` are in the (0, 1) interval.
#' * `x` sums to 1.
#'
#' @return `inspect_par_multinomial` does not return any output. There are two possible outcomes:
#' * The call is silent if `x` is an eligible vector of Multinomial proportions.
#' * An informative error message is thrown otherwise.
#'
#' @seealso
#' * \code{\link[inspector]{inspect_data_dichotomous}} to validate dichotomous data.
#' * \code{\link[inspector]{inspect_data_categorical}} and \code{\link[inspector]{inspect_data_multinom_as_bern}} to validate categorical data.
#' * \code{\link[inspector]{inspect_data_dichotomous}} to validate dichotomous data.
#' * \code{\link[inspector]{inspect_data_categorical}} and \code{\link[inspector]{inspect_data_multinom_as_bern}} to validate categorical data.
#'
#' @examples
#' # Calls that pass silently:
#' x1 <- c(0.5, 0.5)
#' x2 <- rep(1/5, 5)
#' inspect_par_multinomial(x1)
#' inspect_par_multinomial(x2)
#'
#' # Calls that throw an informative error message:
#' \dontrun{mylist <- list(NULL, TRUE, factor(0.5, 0.5),
#'  matrix(c(0.5, 0.5)), c("0.5", "0.5"), list(0.5, 0.5),
#'  c(0.9, NA), c(0.9, NaN), numeric(0), NA, c(0.9, 0.6), c(-0.1, 0.9))}
#' \dontrun{inspect_par_multinomial(mylist[[1]])}
#' \dontrun{inspect_par_multinomial(mylist[[2]])}
#' \dontrun{inspect_par_multinomial(mylist[[3]])}
#' \dontrun{inspect_par_multinomial(mylist[[4]])}
#' \dontrun{inspect_par_multinomial(mylist[[5]])}
#' \dontrun{inspect_par_multinomial(mylist[[6]])}
#' \dontrun{inspect_par_multinomial(mylist[[7]])}
#' \dontrun{inspect_par_multinomial(mylist[[8]])}
#' \dontrun{inspect_par_multinomial(mylist[[9]])}
#' \dontrun{inspect_par_multinomial(mylist[[10]])}
#' \dontrun{inspect_par_multinomial(mylist[[11]])}
#' \dontrun{inspect_par_multinomial(mylist[[12]])}
#'
#' @export

inspect_par_multinomial <- function(x){

  output_name <- paste0("'", deparse(substitute(x)), "'")

  if(is.null(x)){
    stop(paste("Invalid argument:", output_name, "is NULL."))
  }
  if(any(isFALSE(is.atomic(x)), isFALSE(is.vector(x)))){
    stop(paste("Invalid argument:", output_name, "must an atomic vector."))
  }
  if(isTRUE(length(x) == 0)){
    stop(paste("Invalid argument:", output_name, "is empty."))
  }
  if(isFALSE(is.numeric(x))){
    stop(paste("Invalid argument:", output_name, "must be numeric"))
  }
  if(any(is.na(x))){
    stop(paste("Invalid argument: there are NA or NaN values in",  paste0(output_name, ".")))
  }
  if(any(x >= 1, x <= 0)) {
    stop(paste("Invalid argument: all elements of", output_name, "values must be in the (0, 1) interval."))
  }
  if(isFALSE(dplyr::near(sum(x), 1))) {
    stop(paste("Invalid argument:", output_name, "must sum to 1."))
  }
}

#' @title Validate parameters for the Beta distribution
#'
#' @description `inspect_par_beta` checks if an object is an eligible vector of parameters for a Beta distribution. This can be useful to validate inputs, intermediate calculations or outputs in user-defined functions.
#'
#' @param x An arbitrary object.
#'
#' @details `inspect_par_beta` conducts a series of tests to check if `x` is an eligible vector of parameters for a Beta distribution. Namely, `inspect_par_beta` checks if:
#' * `x` is `NULL` or empty.
#' * `x` is an atomic vector
#' * `x` is numeric
#' * `x` is of \code{\link[base]{length}}  2
#' * `x` has `NA` or `NaN` values.
#' * All elements of `x` are positive.
#'
#' @return `inspect_par_beta` does not return any output. There are two possible outcomes:
#' * The call is silent if `x` is an eligible vector of parameters for a Beta distribution.
#' * An informative error message is thrown otherwise.
#'
#' @seealso
#' * \code{\link[inspector]{inspect_data_dichotomous}} to validate dichotomous data.
#' * \code{\link[inspector]{inspect_data_categorical}} and \code{\link[inspector]{inspect_data_multinom_as_bern}} to validate categorical data.
#' * \code{\link[inspector]{inspect_data_dichotomous}} to validate dichotomous data.
#' * \code{\link[inspector]{inspect_data_categorical}} and \code{\link[inspector]{inspect_data_multinom_as_bern}} to validate categorical data.
#'
#' @examples
#' # Calls that pass silently:
#' x1 <- c(1, 1)
#' x2 <- c(2, 5)
#' inspect_par_beta(x1)
#' inspect_par_beta(x2)
#'
#' # Calls that throw an informative error message:
#' \dontrun{mylist <- list(NULL, 1, factor(1, 1),
#'  matrix(c(1, 1)), c("1", "1"), list(1, 1), c(1, NA),
#'   c(1, NaN), c(TRUE, FALSE), numeric(0), c(-1, 1))}
#' \dontrun{inspect_par_beta(mylist[[1]])}
#' \dontrun{inspect_par_beta(mylist[[2]])}
#' \dontrun{inspect_par_beta(mylist[[3]])}
#' \dontrun{inspect_par_beta(mylist[[4]])}
#' \dontrun{inspect_par_beta(mylist[[5]])}
#' \dontrun{inspect_par_beta(mylist[[6]])}
#' \dontrun{inspect_par_beta(mylist[[7]])}
#' \dontrun{inspect_par_beta(mylist[[8]])}
#' \dontrun{inspect_par_beta(mylist[[9]])}
#' \dontrun{inspect_par_beta(mylist[[10]])}
#' \dontrun{inspect_par_beta(mylist[[11]])}
#'
#' @export

inspect_par_beta <- function(x){

  output_name <- paste0("'", deparse(substitute(x)), "'")

  if(is.null(x)){
    stop(paste("Invalid argument:", output_name, "is NULL."))
  }
  if(any(isFALSE(is.atomic(x)), isFALSE(is.vector(x)))){
    stop(paste("Invalid argument:", output_name, "must an atomic vector."))
  }
  if(isFALSE(length(x) == 2)){
    stop(paste("Invalid argument:", output_name, " must be of length 2."))
  }
  if(isFALSE(is.numeric(x))){
    stop(paste("Invalid argument:", output_name, "must be numeric"))
  }
  if(any(is.na(x))){
    stop(paste("Invalid argument: there are NA or NaN values in",  paste0(output_name, ".")))
  }
  if(any(x <= 0)){
    stop(paste("Invalid argument: elements of",  output_name, "must be greather than 0."))
  }
}

#' @title Validate parameters for the Dirichlet distribution
#'
#' @description `inspect_par_dirichlet` checks if an object is an eligible vector of parameters for a Dirichlet distribution. This can be useful to validate inputs, intermediate calculations or outputs in user-defined functions.
#'
#' @param x An arbitrary object.
#'
#' @details `inspect_par_dirichlet` conducts a series of tests to check if `x` is an eligible vector of parameters for a Dirichlet distribution. Namely, `inspect_par_dirichlet` checks if:
#' * `x` is `NULL` or empty.
#' * `x` is an atomic vector
#' * `x` is numeric
#' * `x` has `NA` or `NaN` values.
#' * All elements of `x` are positive.
#'
#' @return `inspect_par_dirichlet` does not return any output. There are two possible outcomes:
#' * The call is silent if `x` is an eligible vector of parameters for a Dirichlet distribution.
#' * An informative error message is thrown otherwise.
#'
#' @seealso
#' * \code{\link[inspector]{inspect_data_dichotomous}} to validate dichotomous data.
#' * \code{\link[inspector]{inspect_data_categorical}} and \code{\link[inspector]{inspect_data_multinom_as_bern}} to validate categorical data.
#' * \code{\link[inspector]{inspect_data_dichotomous}} to validate dichotomous data.
#' * \code{\link[inspector]{inspect_data_categorical}} and \code{\link[inspector]{inspect_data_multinom_as_bern}} to validate categorical data.
#'
#' @examples
#' # Calls that pass silently:
#' x1 <- c(1, 1, 1)
#' x2 <- c(2, 5)
#' inspect_par_dirichlet(x1)
#' inspect_par_dirichlet(x2)
#'
#' # Calls that throw an informative error message:
#' \dontrun{mylist <- list(NULL, factor(1, 1, 1),
#'  matrix(c(1, 1, 1)), c("1", "1", "1"), list(1, 1, 1), c(1, NA),
#'   c(1, NaN, 1), c(TRUE, FALSE), numeric(0), c(-1, 1, 1))}
#' \dontrun{inspect_par_dirichlet(mylist[[1]])}
#' \dontrun{inspect_par_dirichlet(mylist[[2]])}
#' \dontrun{inspect_par_dirichlet(mylist[[3]])}
#' \dontrun{inspect_par_dirichlet(mylist[[4]])}
#' \dontrun{inspect_par_dirichlet(mylist[[5]])}
#' \dontrun{inspect_par_dirichlet(mylist[[6]])}
#' \dontrun{inspect_par_dirichlet(mylist[[7]])}
#' \dontrun{inspect_par_dirichlet(mylist[[8]])}
#' \dontrun{inspect_par_dirichlet(mylist[[9]])}
#' \dontrun{inspect_par_dirichlet(mylist[[10]])}
#'
#' @export

inspect_par_dirichlet <- function(x){

  output_name <- paste0("'", deparse(substitute(x)), "'")

  if(is.null(x)){
    stop(paste("Invalid argument:", output_name, "is NULL."))
  }
  if(any(isFALSE(is.atomic(x)), isFALSE(is.vector(x)))){
    stop(paste("Invalid argument:", output_name, "must an atomic vector."))
  }
  if(isTRUE(length(x) == 0)){
    stop(paste("Invalid argument:", output_name, " is empty."))
  }
  if(isFALSE(is.numeric(x))){
    stop(paste("Invalid argument:", output_name, "must be numeric"))
  }
  if(any(is.na(x))){
    stop(paste("Invalid argument: there are NA or NaN values in",  paste0(output_name, ".")))
  }
  if(any(x <= 0)){
    stop(paste("Invalid argument: elements of",  output_name, "must be greather than 0."))
  }
}

#' @title Validate parameters for the Haldane distribution
#'
#' @description `inspect_par_haldane` checks if an object is an eligible vector of parameters for a Haldane distribution. This can be useful to validate inputs, intermediate calculations or outputs in user-defined functions.
#'
#' @param x An arbitrary object.
#'
#' @details `inspect_par_haldane` conducts a series of tests to check if `x` is an eligible vector of parameters for a Haldane distribution. Namely, `inspect_par_haldane` checks if:
#' * `x` is `NULL` or empty.
#' * `x` is an atomic vector
#' * `x` is numeric
#' * `x` has `NA` or `NaN` values.
#' * All elements of `x` equal to 0.
#'
#' @return `inspect_par_haldane` does not return any output. There are two possible outcomes:
#' * The call is silent if `x` is an eligible vector of parameters for a Haldane distribution.
#' * An informative error message is thrown otherwise.
#'
#' @seealso
#' * \code{\link[inspector]{inspect_data_dichotomous}} to validate dichotomous data.
#' * \code{\link[inspector]{inspect_data_categorical}} and \code{\link[inspector]{inspect_data_multinom_as_bern}} to validate categorical data.
#' * \code{\link[inspector]{inspect_data_dichotomous}} to validate dichotomous data.
#' * \code{\link[inspector]{inspect_data_categorical}} and \code{\link[inspector]{inspect_data_multinom_as_bern}} to validate categorical data.
#'
#' @examples
#' # Calls that pass silently:
#' x1 <- c(0, 0, 0)
#' x2 <- c(0, 0)
#' inspect_par_haldane(x1)
#' inspect_par_haldane(x2)
#'
#' # Calls that throw an informative error message:
#' \dontrun{mylist <- list(NULL, factor(0, 0, 0),
#'  matrix(c(0, 0, 0)), c("0", "0", "0"), list(0, 0, 0), c(0, NA),
#'   c(0, NaN, 0), c(TRUE, FALSE), numeric(0), c(1, 0, 0))}
#' \dontrun{inspect_par_haldane(mylist[[1]])}
#' \dontrun{inspect_par_haldane(mylist[[2]])}
#' \dontrun{inspect_par_haldane(mylist[[3]])}
#' \dontrun{inspect_par_haldane(mylist[[4]])}
#' \dontrun{inspect_par_haldane(mylist[[5]])}
#' \dontrun{inspect_par_haldane(mylist[[6]])}
#' \dontrun{inspect_par_haldane(mylist[[7]])}
#' \dontrun{inspect_par_haldane(mylist[[8]])}
#' \dontrun{inspect_par_haldane(mylist[[9]])}
#' \dontrun{inspect_par_haldane(mylist[[10]])}
#'
#' @export
#'
inspect_par_haldane <- function(x){

  output_name <- paste0("'", deparse(substitute(x)), "'")

  if(is.null(x)){
    stop(paste("Invalid argument:", output_name, "is NULL."))
  }
  if(any(isFALSE(is.atomic(x)), isFALSE(is.vector(x)))){
    stop(paste("Invalid argument:", output_name, "must an atomic vector."))
  }
  if(isTRUE(length(x) == 0)){
    stop(paste("Invalid argument:", output_name, " is empty."))
  }
  if(isFALSE(is.numeric(x))){
    stop(paste("Invalid argument:", output_name, "must be numeric"))
  }
  if(any(is.na(x))){
    stop(paste("Invalid argument: there are NA or NaN values in",  paste0(output_name, ".")))
  }
  if(any(x != 0)){
    stop(paste("Invalid argument: all elements of",  output_name, "must be 0."))
  }
}

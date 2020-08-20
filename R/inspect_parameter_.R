
#' @title lorem ipsum
#'
#' @description lorem ipsum
#'
#' @param x  lorem ipsumlorem ipsum
#'
#' @details lorem ipsum
#'
#' @return lorem ipsum
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
    stop(paste("Invalid argument:", output_name, " is empty."))
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
  if(isFALSE(all.equal(sum(x), 1))) {
    stop(paste("Invalid argument:", output_name, "must sum to 1."))
  }
}

#' @title lorem ipsum
#'
#' @description lorem ipsum
#'
#' @param x  lorem ipsumlorem ipsum
#'
#' @details lorem ipsum
#'
#' @return lorem ipsum
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

#' @title lorem ipsum
#'
#' @description lorem ipsum
#'
#' @param x  lorem ipsumlorem ipsum
#'
#' @details lorem ipsum
#'
#' @return lorem ipsum
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

#' @title lorem ipsum
#'
#' @description lorem ipsum
#'
#' @param x  lorem ipsumlorem ipsum
#'
#' @details lorem ipsum
#'
#' @return lorem ipsum
#' @export

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
    stop(paste("Invalid argument: elements of",  output_name, "must be 0."))
  }
}

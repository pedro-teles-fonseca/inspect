
inspect_data_binomial <- function(x, s, warning_nas = FALSE){

  x_output_name <- deparse(substitute(x))
  s_output_name <- deparse(substitute(s))

  if(is.null(x)){
    stop(paste("Invalid argument:", x_output_name, "is NULL."))
  }
  if(is.null(s)){
    stop(paste("Invalid argument:", s_output_name, "is NULL."))
  }
  if(any(isFALSE(is.atomic(x)), isFALSE(is.vector(x)))){
    stop(paste("Invalid argument:", x_output_name, "must be an atomic vector."))
  }
  if(length(x) == 0){
    stop(paste("Invalid argument:", x_output_name, "is empty."))
  }
  if(any(isFALSE(is.atomic(s)), isFALSE(is.vector(s)), isFALSE(length(s) == 1))){
    stop(paste("Invalid argument:", s_output_name, "must be an atomic vector of length 1."))
  }
  if(isFALSE(typeof(x) %in% c("integer", "double", "character"))){
    stop(paste("Invalid argument: the type of", x_output_name, "must be 'logical', 'integer', 'double' or 'character'."))
  }
  if(isFALSE(typeof(s) %in% c("integer", "double", "character"))){
    stop(paste("Invalid argument: the type of", s_output_name, "must be 'logical', 'integer', 'double' or 'character'."))
  }
  if(all(is.na(x))){
    stop(paste("Invalid argument: all elements of",  x_output_name, " are NA or NaN."))
  }
  if(isTRUE(warning_nas)){
    if(any(is.na(x))){
      warning(paste("There are NA or NaN values in", paste0(x_output_name, ".")))
    }
  }
  if(is.na(s)){
    stop(paste("Invalid argument:", s_output_name, "is NA or NaN"))
  }
  if(isFALSE(s %in% unique(x))){
    warning(paste(s_output_name, "not observed in the data", paste0(x_output_name, ".")))
  }
}

inspect_data_multinomial <- function(x, warning_nas = FALSE){

  x_output_name <- deparse(substitute(x))

  if(is.null(x)){
    stop(paste("Invalid argument:", x_output_name, "is NULL."))
  }
  if(any(isFALSE(is.atomic(x)), isFALSE(is.vector(x)))){
    stop(paste("Invalid argument:", x_output_name, "must be an atomic vector."))
  }
  if(isFALSE(typeof(x) %in% c("double", "integer", "character"))){
    stop("Invalid argument: typeof(x) must be 'integer', 'double' or 'character'.")
  }
  if(length(x) == 0){
    stop(paste("Invalid argument:", x_output_name, "is empty."))
  }
  if(isFALSE(typeof(x) %in% c("integer", "double", "character"))){
    stop(paste("Invalid argument: the type of", x_output_name, "must be 'logical', 'integer', 'double' or 'character'."))
  }
  if(all(is.na(x))){
    stop(paste("Invalid argument: all elements of",  x_output_name, " are NA or NaN."))
  }
  if(isTRUE(warning_nas)){
    if(any(is.na(x))){
      warning(paste("There are NA or NaN values in", paste0(x_output_name, ".")))
    }
  }

}

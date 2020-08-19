
check_beta_hyper_par <- function(hyper_par){

  if(any(hyper_par <= 0, is.na(hyper_par), is.null(hyper_par))){
    stop("Invalid argument: elements of 'hyper_par' must be greather than 0.")
  }
  if(any(!is.atomic(hyper_par), !is.vector(hyper_par))){
    stop("Invalid argument: 'hyper_par' must be an atomic vector.")
  }
  if(any(isFALSE(length(hyper_par) %in% c(1, 2)))){
    stop("Invalid argument: 'hyper_par' must be of length 1 or 2.")
  }
}

check_dirichlet_hyper_par <- function(hyper_par, prior){

  if(is.null(hyper_par)){
    stop("Invalid argument: 'hyper_par' is NULL.")
  }
  if(any(is.na(hyper_par), is.nan(hyper_par))){
    stop("Invalid argument: 'hyper_par' is NA or NaN.")
  }
  if(any(!is.atomic(hyper_par), !is.vector(hyper_par))){
    stop("Invalid argument: 'hyper_par' must be an atomic vector.")
  }
  if(any(!is.numeric(hyper_par))){
    stop("Invalid argument: 'hyper_par' must be a numeric vector.")
  }
  if(any(hyper_par < 0)){
    stop("Invalid argument: dirichlet 'hyper_par' must be positive.")
  }
  if(tolower(prior) %in% c("dirichlet") && all(hyper_par == 0)){
    stop("Invalid argument: dirichlet 'hyper_par' must be positive. Use 'prior' = 'haldane'")
  }
}


#' @importFrom Rdpack reprompt

# nocov start

release_questions <- function(){

  c(
    "Have you updated the version number in inst/CITATION (two fields)?",
    "Have you run all the tests listed in cran-comments.md?",
    "Have you evaluated the impact in downstream dependencies?"
  )
}

# nocov end

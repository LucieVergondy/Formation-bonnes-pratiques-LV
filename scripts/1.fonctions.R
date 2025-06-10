decennie_a_partir_annee <- function(ANNEE) {
  return(ANNEE - ANNEE %%
           10)
}

# fonction de stat agregee
#' Title
#'
#' @param a 
#' @param b 
#' @param ... 
#'
#' @returns
#' @export
#'
#' @examples
#' calculer_stats(rnorm(10))
#' calculer_stats(rnorm(10), "ecart-type")
#' calculer_stats(rnorm(10), "variance")

calculer_stats <- function(a, b = "moyenne", ...) {
  if (b == "moyenne") {
    x <- mean(a, na.rm = T, ...)
  } else if (b == "ecart-type" || b == "sd") {
    x <- sd(a, na.rm = T, ...)
  } else if (b == "variance") {
    x <- var(a, na.rm = T, ...)
  }
  return(x)
}




#' L2 norm
#'
#' @param x vector
#'
#' @return scalar
#' @export
l2norm <- function(x){
  sqrt(sum(x^2))
}

#' C++ version
#'
#' @param vec1 vector 1
#' @param vec2 vector 2
#'
#' @return matrix
#' @export
function_cpp <- function(vec1, vec2){
  calcPWDcpp(matrix(vec1, length(vec1), 1), matrix(vec2, length(vec2), 1))
}

#' R version
#'
#' @param vec1 vector 1
#' @param vec2 vector 2
#'
#' @return matrix
#' @export
function_r <- function(vec1, vec2){
  n1 <- length(vec1); n2 <- length(vec2)
  mat <- matrix(NA, n1, n2)
  for(i in 1:n1){
    for(j in 1:n2){
      mat[i,j] <- abs(vec1[i] - vec2[j])
    }
  }
  
  mat
}
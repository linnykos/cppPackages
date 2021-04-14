context("Test function")

test_that("C++ works and is same as R", {
  trials <- 50
  
  bool_vec <- sapply(1:trials, function(x){
    set.seed(x)
    vec1 <- sample(1:100, 10)
    vec2 <- sample(1:100, 10)
    
    mat1 <- function_r(vec1, vec2)
    mat2 <- function_cpp(vec1, vec2)
    
    sum(abs(mat1 - mat2)) <= 1e-6 && all(dim(mat1) == dim(mat2)) && is.matrix(mat1) && is.matrix(mat2)
  })
  
  expect_true(all(bool_vec))
})

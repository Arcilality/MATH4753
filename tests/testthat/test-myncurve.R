library(testthat)
library(MATH4753li0197)

#Mean calculations
test_that("myncurve calculates mean correctly", {
  result <- myncurve(3, 7, 2)
  expect_equal(result$mu, 3)
})

test_that("myncurve calculates mean correctly", {
  result <- myncurve(10, 4, 1)
  expect_equal(result$mu, 10)
})

test_that("myncurve calculates mean correctly", {
  result <- myncurve(5, 2, 4)
  expect_equal(result$mu, 5)
})



#Sigma calculations
test_that("myncurve calculates sigma correctly", {
 result <- myncurve(3, 7, 2)
 expect_equal(result$sigma, 7)
})

test_that("myncurve calculates sigma correctly", {
  result <- myncurve(10, 4, 1)
  expect_equal(result$sigma, 4)
})

test_that("myncurve calculates sigma correctly", {
  result <- myncurve(5, 2, 4)
  expect_equal(result$sigma, 2)
})


#Area calculations
test_that("myncurve calculates area correctly", {
  result <- myncurve(3, 7, 2)
  expect_true(result$area > 0, "Area is greater than zero")

})

test_that("myncurve calculates area correctly", {
  result <- myncurve(10, 4, 1)
  expect_true(result$area > 0, "Area is greater than zero")

})

test_that("myncurve calculates area correctly", {
  result <- myncurve(5, 2, 4)
  expect_true(result$area > 0, "Area is greater than zero")

})

#' Normal Distribution curve
#'
#' @param mu mu is the mean
#' @param sigma sigma is the standard deviation
#' @param a a is the area
#'
#' @return Normal density curve
#' @export
#'
#' @examples
#' myncurve(3, 7, 2)
#'


myncurve <- function(mu, sigma, a) {

  curve(dnorm(x, mean = mu, sd = sigma), xlim = c(mu - 3 * sigma, mu + 3 * sigma),
        ylab = "Normal Density", main = "Normal Density Curve")

  area <- integrate(dnorm, -Inf, a, mean = mu, sd = sigma)$value

  x <- seq(mu - 3 * sigma, a, length.out = 100)

  y <- dnorm(x, mean = mu, sd = sigma)

  polygon(c(x, rev(x)), c(rep(0, length(x)), rev(y)), col = "skyblue")

  list(mu = mu, sigma = sigma, area = area)

}



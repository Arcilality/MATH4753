#' ntickets function
#'
#' @param N is the number of flight seats
#' @param gamma probability plane will be overbooked
#' @param p probability passenger will show
#'
#' @return two plots of continuous and discrete gamma functions
#' @export
#'
#' @examples
#' ntickets(N = 200, gamma = 0.02, p = 0.95)
#'
ntickets <- function(N, gamma, p) {
  # Calculate number of tickets using the appropriate discrete distribution
  nd <- qbinom(1 - gamma, N, p)

  # Calculate number of tickets using normal approximation
  mu <- N * p
  sigma <- sqrt(N * p * (1 - p))
  nc <- mu + qnorm(gamma) * sigma

  # Print named list
  print(list(nd = nd, nc = nc, N = N, p = p, gamma = gamma))

  # Define objective function for discrete case
  obj_function_discrete <- function(n) 1 - gamma - pbinom(n, N, p)

  # Define objective function for continuous case
  obj_function_continuous <- function(n) 1 - gamma - pnorm(n, mean = mu, sd = sigma)

  # Create plot of objective function vs n for discrete case
  n_values_discrete <- seq(0, 2 * N, by = 1)
  plot(n_values_discrete, obj_function_discrete(n_values_discrete), type = "l", col = "blue",
       xlab = "Number of tickets sold (n)", ylab = "Objective function",
       main = "Objective function Vs n (Discrete)")

  # Create plot of objective function vs n for continuous case
  n_values_continuous <- seq(mu - 3 * sigma, mu + 3 * sigma, length.out = 100)
  plot(n_values_continuous, obj_function_continuous(n_values_continuous), type = "l", col = "red",
       xlab = "Number of tickets sold (n)", ylab = "Objective function",
       main = "Objective function Vs n (Continuous)")

}

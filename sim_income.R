sim_income <- function(gini = 0.6, ave_income = 10000, pop = 10000) {
  fgamma <- function(phi) {
    gini - (1 / (phi * 4^phi)) * 1 / beta(phi, phi + 1)
  }

  # income distribution function
  phi <- uniroot(fgamma, lower = 0.000001, upper = 100)$root
  beta <- (1 / phi) * (ave_income)
  set.seed(123)
  output <- rgamma(pop, shape = phi, scale = beta)

  return(output)
}


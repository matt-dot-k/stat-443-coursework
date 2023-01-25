# Question 3

#' @param sd Standard deviation of the white noise process
#' @param n Number of observations in the white noise process
#' @param lag The lag to calculate the autocorrelation coefficient for
#' @param max_iter The number of iterations to run the simulation for


acf_simulation <- function(n = 1000, sd = 1, lag = 1, max_iter) {
    stopifnot(is.numeric(max_iter))

    # Pre-allocate space
    acf <- double(max_iter)

    # Run simulation
    set.seed(123)
    for (i in 1:max_iter) {
        z_t <- rnorm(n = n, mean = 0, sd = sd)
        acf[i] <- acf(z_t, plot = FALSE)$acf[lag + 1]
    }
    results <- tibble(iteration = 1:5000, acf)
    return(results)
}

acf_simulation(n = 1000, sd = 1, lag = 1, max_iter = 5000)

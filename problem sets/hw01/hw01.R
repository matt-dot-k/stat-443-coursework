library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

# Question 1

co2_mm_gl <- read_csv("./data/co2_mm_gl.csv", col_names = TRUE)

co2_chart <- ggplot(
    data = co2_mm_gl,
    aes(x = year, y = average)
) +
    geom_line(
        linewidth = 1.2,
        color = "#0F5499"
) +
    labs(
        x = "Year",
        y = "CO2 Concentration (ppm)",
        title = "Surface CO2 Concentration Since 1979"
)

co2_train <- co2_mm_gl

# Question 2

canada_gdp <- read_csv("./data/canadaGDP.csv", col_names = TRUE) %>%
    rename(Date = `...1`) %>%
    mutate(Date = ymd(Date))

gdp_chart <- ggplot(
    data = canada_gdp,
    aes(x = Date, y = GDP)
) +
    geom_line(
        linewidth = 1.2,
        color = "#0F5499"
) +
    labs(
        x = "Date",
        y = "GDP",
        title = "Canadian GDP Since 1997"
) +
    scale_x_date(
        date_breaks = "3 years"
)

sequential <- tibble(
    Date = canada_gdp$Date[2:276],
    Growth = diff(log(canada_gdp$GDP), lag = 1)
)

annual <- tibble(
    Date = canada_gdp$Date[13:276],
    Growth = diff(log(canada_gdp$GDP), lag = 12)
)

sequential_chart <- ggplot(
    data = sequential,
    aes(x = Date, y = Growth)
) +
    geom_line(
        linewidth = 1.2,
        color = "#990F3D"
) +
    labs(
        x = "Date",
        y = "GDP Growth Rate",
        title = "Canadian GDP Growth Rate (Monthly)"
)

annual_chart <- ggplot(
    data = annual,
    aes(x = Date, y = Growth)
) +
    geom_line(
        linewidth = 1.2,
        color = "#990F3D"
) +
    labs(
        x = "Date",
        y = "GDP Growth Rate",
        title = "Canadian GDP Growth Rate (Annual)"
)

acf_sequential <- acf(
    x = ts(sequential$Growth),
    main = "ACF of Sequential Series",
    lwd = 2.0)

acf_annual <- acf(
    x = ts(annual$Growth),
    main = "ACF of Annual Series",
    lwd = 2.0)

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
    results <- tibble(iteration = 1:max_iter, acf)
    return(results)
}

simul <- acf_simulation(n = 1000, sd = 1, lag = 1, max_iter = 5000)

simul_chart <- ggplot(
    data = simul,
    aes(x = acf)
) +
    geom_histogram(
        aes(y = ..density..),
        color = "#000000",
        fill = "#FF7FAA"
) +
    stat_function(
        fun = dnorm,
        args = list(
            mean = -0.001, sd = sqrt(0.001)),
        linewidth = 1.2,
        color = "#000000"
) +
    labs(
        x = "Autocorrelation Coefficient (ACF)",
        y = "Density",
        title = "Distribution of ACF for 5000 WN Processes"
)

simul_chart
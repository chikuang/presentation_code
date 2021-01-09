library(astsa)
library(fdaACF)
library(dplyr)

par(mfrow=c(1,2))
tsplot(sp500w, main = "Weekly growth rate of S&P 500 index",
       xlab = "week", ylab =  "Return growth rate")
acf(sp500w, lag.max = 30, xlim = c(2, 30), ylim = c(-0.1, 0.2),
    ylab = "Autocorrelation")
title(main = "ACF")
legend(1, 0.21, legend = c("i.i.d. bound (95 % conf.)"),
       col = "blue", lty = 2, cex=0.8,
       text.font= 4, bg="white")



# From fdaACF -------------------------------------------------------------

# Define the discretization axis
v <- seq(from = 0, to = 1)
nlags <- 30
ci <- 0.95

# Simulate white noise series
N <- 400
v <- seq(from = 0, to = 1, length.out = 24)
sig <- 1
set.seed(3) # For replication
Y <- simulate_iid_brownian_bridge(N, v, sig)


matplot(x = v, y = t(Y), type = "l",
        lty = 1, xlab = "t", ylab = "Value", main = "Functional Brownian Bridge")

FACF_iid <- obtain_FACF(Y = Y, 
                        v = v,
                        nlags = 30,
                        ci = ci,
                        figure = TRUE,
                        main = "Functional ACF")

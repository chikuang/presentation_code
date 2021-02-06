
# Presentation code -------------------------------------------------------
library(fdaACF)
library(ftsa)
library(fda)
library(rlist)
library(dplyr)
library(ggplot2)
# Define the discretization axis
v <- seq(from = 1, to = 24)
nlags <- 30
ci <- 0.95



data(elec_prices)
# Continuous plot ---------------------------------------------------------
par(mfrow=c(1,2))
yyy <- unlist(elec_prices[22:26, ], use.name = FALSE)
xxx <- seq_along(yyy)
plot(x = xxx, y = yyy, type = "l", xlab = "Hours", ylab = "Price (€/MWh)", lwd = 2)
abline(v = (1:5*24)+1, col = "red", lwd = 2)

# raw data and functionalized curve ---------------------------------------



# yy <- unlist(elec_prices[15,], use.names=FALSE)
# xx <- seq_along(yy)
# plot(x = xx, y = yy, xlab = "Hours", ylab = "Price (€/MWh)")
matplot(t(elec_prices[22:26,]), type = "l",lty = 1,xlab = "Hours", ylab = "Price (€/MWh)")

# Autocorrelation function for functional data
# par(mfrow=c(1,1))
par(mfrow=c(1,1))
FACF <- obtain_FACF(Y = as.matrix(elec_prices), 
                    v = v,
                    nlags = nlags,
                    ci = ci,
                    figure = TRUE)

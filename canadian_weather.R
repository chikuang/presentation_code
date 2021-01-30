# Canadian weather --------------------------------------------------------
#' This example is using the code from Ramsay and Silverman (2009), page 51
#' 

library(fda)
library(dplyr)
par(mfrow=c(1,2))
daytime = (1:365)-0.5
JJindex = c(182:365, 1:181)
tempmat = daily$tempav[JJindex,]

dt <- tempmat[,1]
plot(1:length(dt), dt, type = "p", lwd = 1, col = "red",
     xlab = "Day (July 2 to Jun 30)",
     ylab = "Mean temperature (deg. C)")
# lines(1:length(dt), dt, type = "p")


tempbasis = create.fourier.basis(c(0,365),65)
tempfd = smooth.basis(daytime, tempmat, tempbasis)$fd
tempfd$fdnames = list("Day (July 2 to June 30)",
                      "Weather Station",
                      "Mean temperature (deg. C)")
plot(tempfd, col=1, lty=1)


library(fdaACF)
source("./../dynAutocorr/utilities.R")

par(mfrow=c(1,1))
FACF_iid <- obtain_FACF(Y = t(tempmat), 
                        v = seq(0,1, length.out = 365),
                        nlags = 10,
                        ci = 0.95,
                        figure = TRUE,
                        main = "Functional ACF")
par(mfrow=c(1,1))
N <- ncol(tempmat)
# nsamp <- 24
times <- seq(0, 1, length.out = 365)
# 
obs <- tempmat
colnames(obs) <- 1:ncol(tempmat)
tfd <- fts(x = times, y = obs)
calc_distrn(tfd, max = 10) %>% plot_dynauto() + ylim(-0.3, 1.0)


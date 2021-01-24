# Canadian weather --------------------------------------------------------
#' This example is using the code from Ramsay and Silverman (2009), page 51
#' 

library(fda)
daytime = (1:365)-0.5
JJindex = c(182:365, 1:181)
tempmat = daily$tempav[JJindex,]
tempbasis = create.fourier.basis(c(0,365),65)
tempfd = smooth.basis(daytime, tempmat, tempbasis)$fd
tempfd$fdnames = list("Day (July 2 to June 30)",
                      "Weather Station",
                      "Mean temperature (deg. C)")
plot(tempfd, col=1, lty=1)

dt <- tempmat[,1]
plot(1:length(dt), dt, type = "l", lwd = 2, col = "red",
     xlab = "Day (July 2 to Jun 30)",
     ylab = "Mean temperature (deg. C)")
lines(1:length(dt), dt, type = "p")

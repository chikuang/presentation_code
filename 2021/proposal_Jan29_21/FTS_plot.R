library(ftsa)
library(fda)
library(rlist)
library(dplyr)
library(ggplot2)
library(fdaACF)


par(mfrow=c(1,2))

# PM_10 -------------------------------------------------------------------


data("pm_10_GR")
s <- list()
pig < -lapply(1:5, function(i){
  pm_10_GR$y[,i]
})

a <- pig %>% unlist()

plot(seq_along(a)*0.5, a, type = "l", 
     xlab = "Hour", ylab = "PM 10", lwd = 2)
abline(v = (1:10*24)+1, col = "red", lwd = 2)

yyy <- pm_10_GR$y[,1:5]
matplot(x = seq(0,1, length.out = 48), yyy, type = "l", lwd = 2,
        xlab = "Hour", ylab = "PM 10")

par(mfrow=c(1, 1))

obtain_FACF(Y = t(pm_10_GR$y), 
                    v = seq(0, 1, length.out = 48),
                    nlags = 60,
                    ci = 0.95,
                    figure = TRUE,
                    main = "Functional ACF")

source("./../dynAutocorr/utilities.R")
# 
N <- ncol(pm_10_GR$y)
# nsamp <- 24
times <- seq(0, 1, length.out = 48)
# 
obs <- pm_10_GR$y
calc_distrn(obs, H = 60) %>% plot_dynauto()


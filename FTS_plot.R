library(ftsa)
library(fda)
library(rlist)
library(dplyr)
library(ggplot2)

data("pm_10_GR")
plot(pm_10_GR)

pm_10_GR$x

s <- list()
pig <-lapply(1:10, function(i){
  pm_10_GR$y[,i]
})

a <- pig %>% unlist()

plot(seq_along(a)*0.5, a, type = "l", 
     xlab = "Hour", ylab = "PM 10")
abline(v = (1:10*24)+1, col = "red")

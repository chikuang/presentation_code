#' Code for SAS presentation day @ The university of Waterloo
#' Date: Feb 5th, 2021
#' Location: Virtual
#' 

library(tibble)
library(ggplot2)
library(dplyr)
library(cowplot)
library(tidyr)
N <- 1000
set.seed(520)
yy <- rbinom(N, size = 1, prob = 0.8)
x_uni <-  runif(N, 0, 1)
x_beta <- rbeta(N, shape1 = 0.5, shape2 = 10)
x_beta2 <- rbeta(N, shape = 0.5, shape2 = 0.5)
df1 <- tibble(Y = yy, pred_val = x_uni, ind = "Low sharpness")
df2 <- tibble(Y = yy, pred_val = x_beta, ind = "High sharpness")

df_naive <- rbind(df1, df2)

                    
ggplot(df_naive, aes(x = pred_val)) + 
  geom_histogram(aes(y = stat(density)/10),
                 colour = "black", fill = "red",
                 binwidth = 0.1, boundary = 0) + 
  xlab("") + xlim(-.1, 1.1) +
  xlab("Predictive home team winning probability") +  ylab("Percentage") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.5)),
        strip.text = element_text(size = rel(1.5))) +
  facet_wrap(~ ind, nrow = 1) +
  scale_x_continuous(breaks = seq(from = 0, to = 1, by = 0.25))

ggplot(df1, aes(x = yy)) + 
  geom_histogram(aes( y = ..count../sum(..count..)), col = "black",
                 fill = "red",
                 binwidth = 0.1, boundary = 0) +
  xlab("Actual home team winning result") +  ylab("Percentage") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.5)),
        strip.text = element_text(size = rel(1.5))) +
  ggtitle("Actual Result")



# Calibration -------------------------------------------------------------
N <- 1000
set.seed(520)
yy <- rbinom(N, size = 1, prob = 0.8)
x_uni <-  runif(N, 0, 1)
x_beta <- rbeta(N, shape1 = 0.5, shape2 = 10)
x_beta2 <- rbeta(N, shape1 = 12, shape2 = 1)
df1 <- tibble(pred_val = yy, ind = "Actual outcome")
df2 <- tibble(pred_val = x_beta, ind = "Low calibration")
df3 <- tibble(pred_val = x_beta2, ind = "Good calibration")
df_calibration <- rbind(df1, df2, df3)

ggplot(df_calibration, aes(x = pred_val)) + 
  geom_histogram(aes( y = stat(density)/10), col = "black",
                 fill = "red",
                 binwidth = 0.1, boundary = 0) +
  xlab("Home team winning probability") +  ylab("Percentage") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.25)),
        strip.text = element_text(size = rel(1.5))) +
  facet_wrap(~ ind)


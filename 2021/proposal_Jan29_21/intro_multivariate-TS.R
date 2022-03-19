library(astsa)
library(fdaACF)
library(dplyr)
library(ggplot2)
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))
par(mfrow = c(1, 2))
tsplot(sp500w[1:200, ], main = "Weekly growth rate of S&P 500 index",
       xlab = "week", ylab =  "Return growth rate", type = "p",
       lwd = 2)
lines(x = 1:200, sp500w[1:200, ], col = "red", type = "l")
acf(sp500w, lag.max = 30, xlim = c(2, 30), ylim = c(-0.1, 0.2),
    ylab = "Autocorrelation")
title(main = "ACF")
legend(1, 0.21, legend = c("i.i.d. bound (95 % conf.)"),
       col = "blue", lty = 2, cex=0.8,
       text.font= 4, bg="white")

tsplot(sp500w[1:200,], main = "Weekly growth rate of S&P 500 index",
       xlab = "week", ylab =  "Return growth rate", type = "p",
       lwd = 2)


df <- tibble(time = 1:200, val = as.numeric(sp500w[1:200, ]))
ggplot(df, aes(time, val)) + 
    geom_point() + geom_line( col = "red") + 
    labs(x = "Week", y = "Return growth rate")  + 
    ggtitle("Weekly return growth rate of S&P 500")
sp500_acf <- acf(as.numeric(sp500w), lag.max = 30)$acf[-1]
df_acf <- tibble(h = 1:30, rho = sp500_acf,
                 band = 1.96/sqrt(length(sp500w)))
ggplot(data = df_acf, mapping = aes(x = h, y = rho)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = h, yend = 0), size = 1, 
                 col = "black") + 
    geom_ribbon(aes(ymin = -band, ymax = +band), 
                linetype = "longdash", fill = NA, color = "blue") + 
    labs(x = "Lag (week)", y = " ") +
    ggtitle("Autocorrelation plot of S&P 500") + 
    scale_x_continuous(breaks = c(15, (0:3)*10)) + 
    ylim(-0.15, 0.15)

# Create one example for the white noise
set.seed(520)
dat_WN <- tibble(time = 1:500, val = rnorm(500), band = 1.96/sqrt(500))
WN_acf <- acf(as.numeric(rnorm(500)), lag.max = 30)$acf[-1]
df_acf <- tibble(h = 1:30, rho = WN_acf,
                 band = 1.96/sqrt(500))
ggplot(data = df_acf, mapping = aes(x = h, y = rho)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = h, yend = 0), size = 1, 
                 col = "black") + 
    geom_ribbon(aes(ymin = -band, ymax = +band), 
                linetype = "longdash", fill = NA, color = "blue") + 
    labs(x = "Lag (h)", y = " ") +
    ggtitle("Autocorrelation plot of a white noise") + 
    # scale_x_continuous(breaks = c(15, (0:3)*10)) + 
    ylim(-0.15, 0.15)
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

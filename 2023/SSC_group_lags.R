N <- 50
Nt <- 101
source("./util.R")
pacman::p_load(pbapply, ggplot2, tibble, forcats)
theme_update(plot.title = element_text(hjust = 0.5, size = 18),
             legend.position = "none", 
             axis.text = element_text(size = 12),
             axis.title = element_text(size = 16))
set.seed(777)
s <- seq(0, 1, length.out = Nt)

B <- sapply(1:N, function(i){
  sde::BM(N = Nt - 1)
}) |> t()

x <- sapply(1:N, function(i){
  B[i, ] - s * B[i, Nt] 
}) |> t()

norm_raw <- sapply(1:N, function(i){
  sqrt(sum(x[i, ]^2))/(Nt-1)
})

SpMedian <- SpMed(s, x)

x_center <- sapply(1:N, function(i){
  x[i,] - SpMedian$med
}) |>  t()

norm_center <- sapply(1:N, function(i){
  sqrt(sum(x_center[i, ]^2))/(Nt-1)
})

Ndisplay <- 50
res_plot_raw <- pblapply(1:Ndisplay, function(i){
  tibble(id = i, val = x[i, ], time = s)
}) |> bind_rows()

res_plot_cen <- pblapply(1:Ndisplay, function(i){
  tibble(id = i, val = x_center[i, ], time = s)
}) |> bind_rows()

res_plot_raw %>% mutate(id = as_factor(id)) %>% group_by(id) %>%
  ggplot() + 
  geom_line(aes(x = time, y = val, col = id)) +
  theme(legend.position = "none") + xlab("Time (t)") + ylab("") +
  ggtitle(expression(paste("Brownian bridge ", B[i](t)))) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line(data = tibble(med = as.numeric(SpMedian$med), time = s), 
            aes(x = time, y = med), col = "black", size = 1.1)

# Plot ACF ----------------------------------------------------------------
N <- 1000
Nt <- 101
H <- 1:30
library(ggplot2)
library(dplyr)
set.seed(666)
s <- seq(0, 1, length.out = Nt)

B <- sapply(1:N, function(i){
  sde::BM(N = Nt - 1)
}) |> t()

x <- sapply(1:N, function(i){
  B[i, ] - s * B[i, Nt] 
}) |> t()

basis <- sapply(1:7, function(j){
  sqrt(2) * sin(j*pi*s)/(j*pi)
})
D <- 7
colnames(basis) <- paste0("Basis_", 1:D)
reshape2::melt(basis) %>% 
  mutate(Var1 = Var1/Nt, Var2 = factor(Var2)) %>% 
  ggplot(aes(x = Var1, y = value, col = Var2)) + 
  geom_line() +  labs(x = "Time (t)", y = "")

alpha <- 0.05
res <- my_new_receipt(x, H = 30) %>% 
  mutate(lb = qnorm(alpha/2) * std_0_cen/sqrt(N), 
         ub = qnorm(1 - alpha/2) * std_0_cen/sqrt(N))

ggplot(data = res, mapping = aes(x = h, y = rho_cen, colour = factor(h < 8 ))) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = h, yend = 0), size = 1) +
  geom_ribbon(aes(ymin = lb, ymax = ub), linetype = "longdash", fill = NA, color = "blue") +
  labs(x = "h", y = " ")  + 
  scale_x_continuous(breaks = c(1, 2,3,4,5,6,7)) + 
  scale_color_manual(values=c("black", "red"), guide = "none")
  

ggplot(data = res, mapping = aes(x = h, y = rho_cen, colour = factor(h == 2 ))) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = h, yend = 0), size = 1) +
  geom_ribbon(aes(ymin = lb, ymax = ub), linetype = "longdash", fill = NA, color = "blue") +
  labs(x = "h", y = " ")  + 
  scale_x_continuous(breaks = c(2)) + 
  scale_color_manual(values=c("black", "red"), guide = "none")

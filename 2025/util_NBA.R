# Time management -----------------------------------------------------------

## To remove OT
rm_ot <- function(df){
  df %>%
    filter(Quarter <= 4) 
}

## To choose OT game
select_ot <- function(df){
  df %>%
    group_by(game_num) %>%
    arrange(Quarter) %>%
    filter(dplyr::last(Quarter) != 4) %>%
    ungroup
}

## convert clock into time format
time_conversion <-  function(df){
  df %>% 
    mutate(clock = ifelse(grepl("\\.", clock), paste0("0:", clock), clock)) %>% 
    mutate(time = parse_date_time(clock, c("M!:S!", "M!:OS", "S!")) + years(1970)) %>% 
    mutate(time = (720 - seconds(time)) + 720 * (Quarter - 1)) %>% 
    mutate(time = as.numeric(time)) %>% 
    mutate(time_scale = time / 2880)
}

## Average the observation on the same time point
avg_tp <- function(df){
  df %>% group_by(game_num, time_scale, home, away) %>%
    #filter(type_id != 584) %>%  # substitution does not contribute any home_WP change
    mutate(home_WP = mean(home_WP)) %>% 
    group_by(game_num, time_scale) %>% 
    arrange(desc(home), desc(away)) %>% 
    slice(1) %>%
    ungroup
}

## sort_time
sort_time <- function(df){
  df %>%
    group_by(game_num) %>%
    arrange(game_num, time_scale) %>%
    ungroup
}

#
rm_prob_1 <- function(df){
  df %>%
    filter(!(home_WP %in% c(0, 1) &  time_scale != 1))
}

# Game selection/deletion -------------------------------------------

## Load in nba data for each season
#' 1. do time coversion from dttm to seconds
#' 2. remove the duplicate time point
#' 3. get the home_win
load_nba_data <- function(filename, grid_int, grid_final){
  filename %>% read_csv(file = ., col_names = T) %>% 
    time_conversion %>%
    dplyr::select(-c(play_id)) %>%
    sort_time %>% 
    group_by(game_num) %>% mutate(home_win = as.integer(last(home) > last(away))) %>% ungroup %>%  
    rm_ot %>% 
    rm_prob_1 %>% 
    {filter(., ! game_num %in%  rm_game_id(.))} %>%
    avg_tp %>% 
    pre_process(df = ., int_grid = grid_int) %>%
    filter(grid %in% grid_final) %>% score_check()
}


# Check if the score if strange -------------------------------------------
score_check <- function(df){
  df %>% mutate(SD = ifelse(SD > 100 | SD < -100, lag(SD), SD))
}

## Pre-process data

pre_process <- function(df, int_grid){
  df %>% group_by(game_num) %>% 
    summarize(home_approx = list(approx(time_scale, home, int_grid, method = "constant")$y), # step
              away_approx = list(approx(time_scale, away, int_grid, method = "constant")$y), # step
              phat_espn = list(approx(time_scale, home_WP, int_grid, method = "constant")$y), # step
              grid = list(int_grid),
              Y = first(home_win)) %>% unnest_legacy %>% 
    group_by(game_num) %>%
    mutate(IP = first(phat_espn), SD = home_approx - away_approx) %>%
    ungroup() %>%
    dplyr::select(-c(home_approx, away_approx))
}

## Calculate the nunmber of games
n_games <- function(df){
  df %>% 
    distinct(game_num) %>%
    nrow()
}

## Remove the games which does not start in the first quarter (i.e. infective egame)
rm_game_id <- function(df){
  df %>%
    group_by(game_num) %>%
    arrange(time_scale) %>%
    slice(c(1,n())) %>%
    filter( (time_scale != 1 | Quarter !=4)  & (time_scale != 0 | Quarter !=1) )%>%
    ungroup() %>%
    dplyr::select(game_num) %>%
    pull()
}

# Plotting functions ------------------------------------------------------
# Reliability Diagram
relia_plot <- function(df, prop, n_bin = 10){
  df %>% 
    filter(grid == grid[which.min(abs(grid - prop))]) %>% 
    {ReliabilityDiagram(probs = (.)$phat, obs = (.)$home_win, bins = n_bin, plot = T )}
}

# The time points (in $[0,1]$) where we do not have the estimate for SD.

mod_coef_err <- function(mod, grid) {
  sapply(mod, function(x) {
    x %>% coef %>% is.na %>% any %>% return
  }) %>% which %>% {
    grid[first(.):(last(.) + ceiling(length(grid) * 0.005))]
  }
}


# Smoothing function for plot ---------------------------------------------

L_smoothing <- function(df, ma_coef){
  df %>% 
    mutate(UB_t = L + qnorm(0.975) * sqrt(sigma2)/sqrt(n), 
           LB_t = L + qnorm(0.025) * sqrt(sigma2)/sqrt(n)) %>% 
    summarise(grid = grid,
              L = zoo::rollapply(L, ma_coef, mean, partial = TRUE),
              UB_t = zoo::rollapply(UB_t, ma_coef, mean,  partial = TRUE),
              LB_t = zoo::rollapply(LB_t, ma_coef, mean,  partial = TRUE))
}

coalesce_by_column <- function(df) {
  return(coalesce(df[1], df[2]))
}

#' To calculate the sigma2 and loss function of the input dataframe--------------
#' 
calc_L_s2 <- function(df){
  df  %>% mutate(si = L(1, phat_1) - L(0, phat_1) - 
                     (L(1, phat_2) - L(0, phat_2))) %>%
    group_by(grid) %>% 
    summarise(L = mean(L(phat_1, Y) - L(phat_2, Y)), 
              sigma2 = mean(si ^ 2) / 4, n = n(), .groups = "drop") 
}


# Plot the Confidence bands using naive t and FFSCB' ----------------------
plot_ffscb <- function(df, my_label){
  df %>% ggplot(aes(x = grid, y = L)) + 
    geom_line() +
    geom_ribbon(aes(ymax = UB_t,
                    ymin = LB_t, 
                    col = "naive-t"), alpha = 0.2) +
    geom_hline(yintercept = 0, colour = 'blue', size = 2) + 
    geom_ribbon(aes(ymax = UB_ffscb  ,
                    ymin = LB_ffscb,  col = "ffscb" ), alpha = 0.2) + 
    xlab("game (%)") +
    ylab(TeX('$\\hat{\\Delta}_n(t)$')) +  
    theme(axis.title = element_text(size = rel(2.5)),
          axis.text = element_text(size = rel(2.0)), 
          legend.title = element_blank()) + 
    annotate("label", -Inf, Inf, label = my_label,
             hjust = 0, vjust = 1, size = rel(4))
}


# Loss function -------------------------------------------------------------------------------
L <- function(x, y){
  return( (x - y)^2 )
}

# Sanity generation ---------------------------------------------------------------------------
sanity_generator <- function(N, Ngame, a = 1.0, b = 0.2){
  uu <- a * runif(Ngame, -1, +1) + b
  xt <- sapply(1:Ngame, function(x){
    tt <- seq(0, 1, 1/N)
    wt <- sde::BM(x = 0, t0 = 0, T = 1, N = N) %>% as.numeric() ##
    #' @BM 
    uu[x] * tt + wt
  })  # nsamp x ngame
  
  Yn <- ifelse(xt[N+1, ] > 0, 1, 0)
  
  lapply(1:(N+1), function(k){
    tibble(game = 1:Ngame, u = uu, Xt = xt[k, ],
           grid = (k-1)/N, Y = Yn) # each sample point
  }) %>% list.rbind() %>% mutate(p_true = pnorm(u*(1-grid) + Xt,
                                                sd = sqrt(1-grid)))
}

# Calculate P-value using our method ----------------------------------------------------------
calc_p_val <- function(dataframe, cent = FALSE){
  temp <- dataframe %>% group_by(grid) %>% group_split()
  
  if(!cent){
    eigV_hat <- lapply(1:nsamp, function(i){
      sapply(1:nsamp, function(j){
        as.numeric(temp[[i]]$diff_A %*% temp[[j]]$diff_A /ngame)
      })
    }) %>% list.rbind %>% {RSpectra::eigs_sym(A = (.), k = D, which = "LM",
                                              opts = list(retvec = FALSE))$values} %>%
      {(.)/nsamp}
    
    set.seed(520)
    MC_hat <- sapply(1:N_MC, function(x){
      crossprod(eigV_hat, rchisq(D, df = 1))
      
    })
    p_val <- 1 - ecdf(MC_hat)(Z)
  } else{
    eigV_til <- lapply(1:nsamp, function(i){
      sapply(1:nsamp, function(j){
        as.numeric(temp[[i]]$diff_B %*% temp[[j]]$diff_B /ngame)
      })
    }) %>% list.rbind %>% {RSpectra::eigs_sym(A = (.), k = D, which = "LM",
                                              opts = list(retvec = FALSE))$values} %>%
      {(.)/nsamp}
    
    set.seed(520)
    MC_til <- sapply(1:N_MC, function(x){
      crossprod(eigV_til, rchisq(D, df = 1))
    })
    
    p_val <- 1 - ecdf(MC_til)(Z)
  }
  
  return(p_val)
}


# Calculate Z in our method -------------------------------------------------------------------
calc_Z <- function(dataframe){
  dataframe %>% group_by(grid) %>%
    summarise(delta_n = mean( L(phat_1, Y) - L(phat_2, Y))) %>%
    summarise(sum(delta_n^2/nsamp*ngame)) %>% pull()
}


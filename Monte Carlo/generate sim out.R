#---------------[   Purpose    ]--------------------
#
# To pregenerate data for visualizations
#
#---------------[   Sys Info   ]--------------------
#
#  Date  : Sat Jan 01 14:06:27 2022
#  Author: Seunghyun Lee
#  OS    : Windows
#  Node  : DESKTOP-8FJP3KC
#
#---------------[ Pinned Notes ]--------------------
#
# 
#
#---------------[   Process    ]--------------------


# With 100 sims (for jitter plot)-------------------------------------------------------


#--- set seed ---#
set.seed(2021) 

#--- number of repetitions ---# 
num_reps <- 100

#--- sim func ---#
sim_func <- function(rep, n, gamma, b1, b2, dist_sim) {
  #--- DGP ---#
  df <- tibble(
    x1 = rnorm(n = n, 0, 1),
    x2 = gamma * x1 +  rnorm(n = n, sqrt(1 - gamma^2), 1),
    err = dist_list[[dist_sim]](n),
    Y = b1 * x1 + b2 * x2 + err
  )
  #--- estimate ---#
  reg <- lm(Y ~ 0+x1, data = df) # ignore intercept
  
  #--- store results of interest (potentially many) ---#
  results <- list(alpha = coefficients(reg)[1]) # put results in a list
  return(results)
}

#--- param list ---#
param_list <- expand.grid(
  rep = 1:num_reps,
  n = 100, 
  gamma = c(0, 0.5, 0.9),
  b1 = 1, 
  b2 = c(0, 0.5, 1), 
  dist_sim = c("Normal", "Student_t")
)

#--- dist list ---#
dist_list <- list(
  Normal = function(n) rnorm(n, 0, 1),
  Student_t = function(n) rt(n, 5)
)

#--- loop ---#
sim_out <-
  param_list %>%
  mutate(results = pmap(param_list, sim_func)) %>%
  unnest_wider(results) %>%  # this is useful if we stored multiple outputs
  group_by(gamma, b2) %>%
  mutate(mean_alpha = mean(alpha))


saveRDS(sim_out,"data/sim_out_100.rds")

readRDS("data/sim_out_100.rds")$b2 %>% unique()

# With 1000 sims (for shiny)-------------------------------------------------------


param_list <- expand.grid(
  rep = 1:1000,
  n = 100, 
  gamma = seq(0,1,.1),
  b1 = 1, 
  b2 = seq(0,1,.1), 
  dist_sim = c("Normal", "Student_t")
)

#--- loop ---#
plan(multisession)
sim_out <-
  param_list %>%
  mutate(results = future_pmap(.progress = T,param_list, sim_func)) %>%
  unnest_wider(results) %>%  # this is useful if we stored multiple outputs
  group_by(gamma, b2) %>%
  mutate(mean_alpha = mean(alpha))

saveRDS(sim_out,"data/sim_out_1000.rds")

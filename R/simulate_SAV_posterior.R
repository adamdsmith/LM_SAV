simulate_SAV_posterior <- function(nsim = 1000, seed = NULL, ncores = 6) {
  pacman::p_load(parallel)
  load("./Data/basin_bathymetry_rasters.rda")
  load("./Output/final_SAV_models.rda")
  input_w <- brick(west_rast, init(west_rast, "x"), init(west_rast, "y"))
  input_e <- brick(east_rast, init(east_rast, "x"), init(east_rast, "y"))
  names(input_w) <- names(input_e) <- c("depth", "x", "y")
  pred_yrs <- levels(sav$cYear)
  input_w <- na.omit(as.data.frame(input_w)) %>%
    reshape::expand.grid.df(., data.frame(cYear = factor(pred_yrs)))
  input_e <- na.omit(as.data.frame(input_e)) %>%
    reshape::expand.grid.df(., data.frame(cYear = factor(pred_yrs)))

  # Get linear predictor matrix
  # Contains values of the linear predictor when postmultiplied by the parameter vector
  Xp_w <- predict(west_dys_c, input_w, type = "lpmatrix")
  Xp_e <- predict(east_dys_c, input_e, type = "lpmatrix")
  
  # Retrieve final model coefficients and Bayesian covariance matrices
  B_w <- coef(west_dys_c)
  V_w <- vcov(west_dys_c)
  B_e <- coef(east_dys_c)
  V_e <- vcov(east_dys_c)
  
  # How many posterior simulations?
  if (!is.null(seed)) set.seed(seed)
  rands_w <- MASS::mvrnorm(nsim, B_w, V_w)
  rands_e <- MASS::mvrnorm(nsim, B_e, V_e)
  
  # Get inverse link function
  inv_link <- family(east_dys_c)$linkinv
  
  cl <- makeCluster(ncores)
  on.exit(stopCluster(cl))
  varList <- c("Xp_w", "Xp_e", "inv_link", "input_w", "input_e", 
               "rands_w", "rands_e", "CB_classify")
  clusterExport(cl, varList, envir = environment())
  clusterEvalQ(cl, pacman::p_load(dplyr, parallel))

  out <- parLapply(cl, seq_len(nsim), function(i) {
    pred_w <- inv_link(Xp_w %*% rands_w[i, ])
    pred_e <- inv_link(Xp_e %*% rands_e[i, ])
    w <- pred_w %>% 
      tapply(., input_w$cYear, CB_classify) %>%
      plyr::ldply(., .id = "year") %>%
      mutate(basin = "west")
    e <- pred_e %>% 
      tapply(., input_e$cYear, CB_classify) %>%
      plyr::ldply(., .id = "year") %>%
      mutate(basin = "east")
    rbind(w, e)
  })
  
  out <- do.call("rbind", out)
  
  return(out)
  
}

  
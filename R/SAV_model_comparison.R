pacman::p_load(bbmle)

### Compare four model specifications, and two link functions, for each basin
east <- sav[sav$basin == "east", ]
west <- sav[sav$basin == "west", ]

## EAST
# Smooth of depth across all years, main year effect
east_d <- gam(sav ~ s(depth) + cYear, data = east,
              family = betar(link = "logit"),
              method = "REML")
east_d_c <- gam(sav ~ s(depth) + cYear, data = east,
              family = betar(link = "cauchit"),
              method = "REML")
# Smooth of depth and spatial process across years, main year effect
east_ds <- gam(sav ~ s(depth) + s(x, y, bs="tp") + cYear, data = east,
               family = betar(link = "logit"),
               method = "REML")
east_ds_c <- gam(sav ~ s(depth) + s(x, y, bs="tp") + cYear, data = east,
               family = betar(link = "cauchit"),
               method = "REML")
# Smooth of depth by year and main year effect
east_dy <- gam(sav ~ s(depth, by = cYear) + cYear, data = east,
               family = betar(link = "logit"),
               method = "REML")
east_dy_c <- gam(sav ~ s(depth, by = cYear) + cYear, data = east,
               family = betar(link = "cauchit"),
               method = "REML")
# Smooth of depth and spatial process by year, and main year effect
east_dys <- gam(sav ~ s(depth, by = cYear) + s(x, y, bs="tp", by = cYear) + cYear, 
                data = east,
                family = betar(link = "logit"),
                method = "REML")
east_dys_c <- gam(sav ~ s(depth, by = cYear) + s(x, y, bs="tp", by = cYear) + cYear, 
                  data = east,
                  family = betar(link = "cauchit"),
                  method = "REML")
AICctab(east_d, east_ds, east_dy, east_dys, 
        east_d_c, east_ds_c, east_dy_c, east_dys_c, weights = TRUE)

## WEST
# Smooth of depth across all years, main year effect
west_d <- gam(sav ~ s(depth) + cYear, data = west,
              family = betar(link = "logit"),
              method = "REML")
west_d_c <- gam(sav ~ s(depth) + cYear, data = west,
                family = betar(link = "cauchit"),
                method = "REML")
# Smooth of depth and spatial process across years, main year effect
west_ds <- gam(sav ~ s(depth) + s(x, y, bs="tp") + cYear, data = west,
               family = betar(link = "logit"),
               method = "REML")
west_ds_c <- gam(sav ~ s(depth) + s(x, y, bs="tp") + cYear, data = west,
                 family = betar(link = "cauchit"),
                 method = "REML")
# Smooth of depth by year and main year effect
west_dy <- gam(sav ~ s(depth, by = cYear) + cYear, data = west,
               family = betar(link = "logit"),
               method = "REML")
west_dy_c <- gam(sav ~ s(depth, by = cYear) + cYear, data = west,
                 family = betar(link = "cauchit"),
                 method = "REML")
# Smooth of depth and spatial process by year, and main year effect
west_dys <- gam(sav ~ s(depth, by = cYear) + s(x, y, bs="tp", by = cYear) + cYear, 
                data = west,
                family = betar(link = "logit"),
                method = "REML")
west_dys_c <- gam(sav ~ s(depth, by = cYear) + s(x, y, bs="tp", by = cYear) + cYear, 
                  data = west,
                  family = betar(link = "cauchit"),
                  method = "REML")
AICctab(west_d, west_ds, west_dy, west_dys, 
        west_d_c, west_ds_c, west_dy_c, west_dys_c, weights = TRUE)

# Compare Deviance explained
# East
sapply(list(east_d, east_ds, east_dy, east_dys, 
       east_d_c, east_ds_c, east_dy_c, east_dys_c), 
       function(x) summary(x)$dev.expl*100)
# West
sapply(list(west_d, west_ds, west_dy, west_dys, 
            west_d_c, west_ds_c, west_dy_c, west_dys_c), 
       function(x) summary(x)$dev.expl*100)

## Check model fit
dsm::rqgam.check(east_dys_c)
dsm::rqgam.check(west_dys_c)

## Model summary
summary(east_dys_c)
summary(west_dys_c)
anova(east_dys_c)
anova(west_dys_c)

save(west_dys_c, east_dys_c, file = "./Output/final_SAV_models.rda")

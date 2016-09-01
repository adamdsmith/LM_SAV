##### FIGURE 3 ######
# Spatially-explicit predictions of SAV coverage based on final models

## Load final GAM models
# Final GAM models are loaded in `figure_2_SAV_depth.R`
#load("./Output/final_SAV_models.rda")

## Create bathymetry rasters for west and east basins for predictions
# This is based on a previously created bathymetry model not included with this repository

#source("./R/create_bath_df.R")
#source("./R/fit_LM_bath.R")
#bath <- create_bath_df()
#east_rast <- fit_LM_bath(bath, basins = "east", check_soap = FALSE,
#                         print_summary = FALSE, gam_check = FALSE,
#                         plot_smooth = FALSE, seed = 1050412280)[["pred_rast"]][[1]]
#west_rast <- fit_LM_bath(bath, basins = "west", check_soap = FALSE,
#                         print_summary = FALSE, gam_check = FALSE,
#                         plot_smooth = FALSE, seed = 1050412280)[["pred_rast"]][[1]]
#save(east_rast, west_rast, file = "./Output/basin_bathymetry_rasters.rda")
#load("./Output/basin_bathymetry_rasters.rda", verbose = TRUE)

## Prepare covariate rasters (bathymetry, x, y), by year, for prediction
#input_w <- brick(west_rast, init(west_rast, "x"), init(west_rast, "y"))
#input_e <- brick(east_rast, init(east_rast, "x"), init(east_rast, "y"))
#names(input_e) <- names(input_w) <- c("depth", "x", "y")

# Generate spatial predictions of SAV
#pred_yrs <- levels(sav$cYear)
#sav_rasts <- lapply(pred_yrs, function(yr) {
#  e <- predict(input_e, east_dys_c, 
#               const = data.frame(cYear = factor(yr)), 
#  w <- predict(input_w, west_dys_c, 
#              const = data.frame(cYear = factor(yr)), 
#               type = "response")
#  mosaic(e, w, fun = mean) # Function (i.e., mean) does not matter; they do not overlap
#})

# Stack them and name them
#sav_rasts <- stack(sav_rasts); names(sav_rasts) <- pred_yrs
#save(sav_rasts, file = "./Output/fitted_rasters.rda")
load("./Output/fitted_rasters.rda", verbose = TRUE)

## Create ggplot-accessible version of Lake Mattamuskeet boundary
if (!exists("LM")) {
  LM <- readOGR("./GIS", "LM_poly_orig", verbose = FALSE)
  LM <- gBuffer(LM, byid = TRUE, width = 0) # To correct invalid geometry
}
bound <- fortify(LM)

## Create plot by year
coords <- xyFromCell(sav_rasts, seq_len(ncell(sav_rasts)))
dat <- stack(as.data.frame(getValues(sav_rasts))) %>%
  mutate(ind = gsub("X", "", ind))
names(dat) <- c("sav", "year")
dat <- cbind(coords, dat) %>% na.omit()

panel_labels <- data.frame(x = min(dat$x), y = max(dat$y),
                           year = unique(dat$year),
                           p_label = unique(dat$year))

p <- ggplot(aes(x = x, y = y), data = dat) + geom_raster(aes(fill = sav)) + 
  coord_equal() + facet_wrap(~ year) + 
  geom_polygon(data = bound, aes(x = long, y = lat, group = group), color = "black", fill = NA) + 
  geom_point(data = sav, aes(fill = sav), shape = 21) + 
  scale_fill_viridis("Proportion\nSAV", breaks = c(0, 0.1, 0.4, 0.7, 1), limits = c(0,1)) +
  theme_classic() +
  geom_text(data = panel_labels, aes(label = p_label),
            hjust = 0, vjust = 1, colour = "black", size = 5) +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line=element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank(), axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

tiff(file = "./Output/Mean_SAV.tif", width = 10, height = 5.5, units = "in", 
     compression = "lzw", res = 300)
print(p)
dev.off()
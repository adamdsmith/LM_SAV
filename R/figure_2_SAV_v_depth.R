##### FIGURE 2 ######

## Load final GAM models
load("./Output/final_SAV_models.rda", verbose = TRUE)

east <- sav[sav$basin == "east", ]
west <- sav[sav$basin == "west", ]

# Cycle through each model
plotDat <- data.frame()
pResid <- data.frame()
  
for (basin in c("west", "east")) {
  if (basin == "west") {
    model <- west_dys_c
    modDat <- west
  } else {
    model <- east_dys_c
    modDat <- east
  }
  
  old_mai <- par()[["mai"]]
  par(mai = rep(0.01, 4))
  gamDat <- plot(model, seWithMean = TRUE, se = 1.96, residuals = TRUE, pages = 1)
  
  for (i in seq_along(levels(sav$cYear))) {
    yr <- levels(sav$cYear)[i]
    tmpList <- gamDat[[i]]
    
    ## Assemble plotting data
    tmpDat <- data.frame(basin = basin,
                         year = yr,
                         depth = tmpList$x,
                         fit = tmpList$fit,
                         lcl = tmpList$fit - tmpList$se, # Note this actually 1.96 * SE
                         ucl = tmpList$fit + tmpList$se, # Ditto
                         stringsAsFactors = FALSE)
    
    ## Assemble partial residuals
    pullMe <- which(modDat$cYear == yr)
    tmpResid <- data.frame(basin = basin,
                           year = yr,
                           depth = modDat$depth[pullMe],
                           resid = tmpList$p.resid[pullMe],
                           stringsAsFactors = FALSE)
    
    plotDat <- rbind(plotDat, tmpDat)
    pResid <- rbind(pResid, tmpResid)
    
  }
  
}

## Convert basin to factor so plot order is more intuitive
plotDat <- plotDat %>%
  mutate(basin = factor(basin, levels = c("west", "east")))
pResid <- pResid %>%
  mutate(basin = factor(basin, levels = c("west", "east")))

# Create plot
panel_labels <- data.frame(depth = c(min(west$depth), min(east$depth)),
                           fit = 7,
                           basin = unique(plotDat$basin),
                           year = 1989,
                           p_label = c("West", "East"))

## Set up some custom breaks
props <- c(0.25, 0.5, 0.75, 0.9)
prop_changes <- c(-1*rev(props), 0, props)
cust_breaks <- prop_change(prop_changes, inverse = TRUE)

theme_set(theme_classic(base_size = 11))
p <- ggplot(plotDat, aes(depth, fit)) + 
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.3) + 
  geom_line() + 
  geom_hline(yintercept = 0, lty = "dashed", size = 0.25) +
  facet_grid(year ~ basin, scales = "free_x", space = "free_x") +
#  geom_rug(data = pResid, aes(depth, y = 0), sides = "b") +
  geom_point(data = pResid, aes(depth, resid), 
             colour = "#21908CFF", alpha = 0.5, size = 0.75, stroke = 0) + 
  scale_x_continuous("Depth (ft)", breaks = -5:0) +
  scale_y_continuous("Centered additive predictor", 
                     breaks = seq(-9, 9, 3),
                     labels = every_nth(seq(-9, 9, 3), 2)) +
#  scale_y_continuous("Proportional change in SAV coverage", 
#                     breaks = cust_breaks,
#                     labels = prop_changes) +
  coord_cartesian(ylim=c(-7, 7)) +
  geom_text(data = panel_labels, aes(label = p_label),
            hjust = 0, vjust = 1, colour = "black", size = 3) +
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_blank(),
        strip.text.y = element_text(size = 8),
        axis.text = element_text(size = 8))

par(mai = old_mai)

tiff(file = "./Output/Changing_depth.tif", width = 3.25, height = 5, units = "in", 
     compression = "lzw", res = 1200)
print(p)
dev.off()

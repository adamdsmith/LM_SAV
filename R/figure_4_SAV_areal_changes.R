##### FIGURE 4 ######

## Load posterior simulations results
load("./Output/posterior_SAV.rda")

# Calculate summary statistics
CB_redux <- sav_fits %>%
  group_by(label, class, year, basin) %>%
  summarise(med = median(area_ha), 
            lcl = quantile(area_ha, 0.025),
            ucl = quantile(area_ha, 0.975)) %>%
  ungroup() %>%
  mutate(basin = factor(basin, levels = c("west", "east")),
         year = as.numeric(as.character(year)))
#  filter(class %in% as.character(c(1,4)))

export_excel <- FALSE
if (export_excel) {
  pacman::p_load(xlsx)
  tmp <- arrange(CB_redux, year, basin, class) %>%
    dplyr::select(-class) %>% rename(class = label) %>%
    as.data.frame()
  write.xlsx(tmp, "./Output/SAV_coverage_summary.xlsx", 
             sheetName = "SAV coverage summary",
             row.names = FALSE, showNA = FALSE)
}

## Plot the figure
pd <- position_dodge(width = 0.75)
custom_breaks <- seq(0, 10000, 1000)
custom_labels <- every_nth(custom_breaks/1000, 2, inverse = TRUE)
panel_labels <- data.frame(year = 1988.25, med = Inf, 
                           basin = levels(CB_redux$basin), 
                           p_label = c("West", "East"))
vir_cols <- c(viridis(1, b=0.9, e=0.9), #Dense
              viridis(1, b=0.6, e=0.6), #Moderate
              viridis(1, b=0.3, e=0.3), #Sparse
              viridis(1, b=0.05, e=0.05)) #Very sparse

theme_set(theme_classic(base_size = 16))
p <- ggplot(CB_redux, aes(x = year, y = med, colour = label)) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0, position = pd, size = 1) +
  geom_line(linetype = "dashed", position = pd, size = 1) +
  geom_point(aes(shape = label, fill = label), color = "black", size = 2.5, position = pd) + 
  scale_fill_manual("SAV coverage", values = vir_cols, 
                    guide = guide_legend(ncol = 2, title.hjust =0.5,
                                         title.position = "top")) +
  scale_color_manual("SAV coverage", values = vir_cols,
                     guide = guide_legend(ncol = 2, title.hjust =0.5,
                                          title.position = "top")) +
  scale_shape_manual("SAV coverage", values = 21:24,
                     guide = guide_legend(ncol = 2, title.hjust =0.5,
                                          title.position = "top")) +
  annotate("segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf) +
  scale_x_continuous("", breaks = unique(as.numeric(CB_redux$year)),
                     limits = c(1988, 2016), expand = c(0,0)) +
  scale_y_continuous("Area (ha x 1000) of SAV",
                     breaks = custom_breaks, labels = custom_labels) +
  facet_grid(basin ~ ., scales = "free_y", space = "free_y") +
  geom_text(data = panel_labels, aes(label = p_label),
            hjust = 0, vjust = 1, colour = "black", size = 4.5) +
  # Workaround for current ggplot2 bug
  theme(axis.line.x = element_line(),
        axis.line.y = element_line(),
        legend.justification=c(0.5,0.5), 
        legend.position="top",
        legend.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1))

tiff(file = "./Output/Changing_SAV.tif", width = 6, height = 7.5, units = "in", 
     compression = "lzw", res = 600)
print(p)
dev.off()

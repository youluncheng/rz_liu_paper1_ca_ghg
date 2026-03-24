library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(terra)
library(cowplot)

#ROT
theme_set(theme_bw())
r10 <- terra::rast('C:/Users/asus/Desktop/paper1/upscaling/LLdata_for_upscaling/N2O/scenario_SI0.tif')
r10.p <- as.data.frame(r10,xy=TRUE)
mean_value <- mean(r10.p$improvement, na.rm = TRUE)
r10.p$improvement[r10.p$improvement > 50] <- mean_value
r10.p$improvement[r10.p$improvement < -50] <- mean_value
mean_formatted <- sprintf("%.2f", mean_value)
world <- ne_countries(scale = "medium", returnclass = "sf")
p10 <- ggplot(data = world) + geom_sf(color = "gray50", fill = "#F7F5F2") +
  geom_tile(data = r10.p, aes(x=x, y=y, fill = improvement)) +
  scale_fill_gradientn(colours = c("#2c7bb6", "#abd9e9", "#C9E995", "#fdae61", "#f48c4f"), limits = c(-120, 120), na.value = "gray80", name = expression('N'[2] * 'O Change (%)'), guide = "none") +
  theme_void() +
  theme(panel.border = element_blank(),
        plot.title = ggtext::element_markdown(hjust = 0.5, size  = 12)) +
  ggtitle("**Crop rotation (ROT)**") +
  coord_sf(crs = 4326, expand = FALSE)  

hist_plot <- ggplot(r10.p, aes(x = improvement)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "#3B528B", color = "white", alpha = 0.8) +
  geom_density(color = "#E16462", linewidth = 0.3) +
  geom_vline(xintercept = mean_value, color = "#FDE725", linewidth = 0.3, linetype = "dashed") +
  annotate("text", x = mean_value+10, y = 0.06, label = paste0("Mean: ", mean_formatted, "%"), vjust = 2, color = "#E16462", fontface = "bold", size = 1.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01)), breaks = seq(0, 0.06, by = 0.03)) +
  labs(x = expression('N'[2]*'O Change (%)'), y = "Density") +
  theme_classic() +
  theme(panel.background = element_rect(fill = NA, colour = NA),
        plot.background  = element_rect(fill = NA, colour = NA),
        axis.text = element_text(color = "black", size = 4),
        axis.title = element_text(size = 5),
        axis.line = element_line(color = "black", linewidth = 0.3),
        axis.ticks = element_line(color = "black", linewidth = 0.3),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
c1 <- ggdraw() +
  draw_plot(p10, x=0.03, y=0.03, width=0.94, height=0.94) +
  draw_plot(hist_plot, x=0.02, y=0.26, width=0.25, height=0.30)

print(c1)

#NT
theme_set(theme_bw())
r11 <- terra::rast('C:/Users/asus/Desktop/paper1/upscaling/LLdata_for_upscaling/N2O/scenario_SI1.tif')
r11.p <- as.data.frame(r11,xy=TRUE)
mean_value <- mean(r11.p$improvement, na.rm = TRUE)
r11.p$improvement[r11.p$improvement > 120] <- mean_value
r11.p$improvement[r11.p$improvement < -120] <- mean_value
mean_value <- mean(r11.p$improvement, na.rm = TRUE)
mean_formatted <- sprintf("%.2f", mean_value)
world <- ne_countries(scale = "medium", returnclass = "sf")
p11 <- ggplot(data = world) + geom_sf(color = "gray50", fill = "#F7F5F2") +
  geom_tile(data = r11.p, aes(x=x, y=y, fill = improvement)) +
  scale_fill_gradientn(colours = c("#2c7bb6", "#abd9e9", "#C9E995", "#fdae61", "#f48c4f"), limits = c(-120, 120), na.value = "gray80", name = expression('N'[2] * 'O Change (%)'), guide = "none") +
  theme_void() +
  theme(panel.border = element_blank(),
        plot.title = ggtext::element_markdown(hjust = 0.5, size  = 12)) +
  ggtitle("**No tillage (NT)**") +
  coord_sf(crs = 4326, expand = FALSE)  

hist_plot <- ggplot(r11.p, aes(x = improvement)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "#3B528B", color = "white", alpha = 0.8) +
  geom_density(color = "#E16462", linewidth = 0.3) +
  geom_vline(xintercept = mean_value, color = "#FDE725", linewidth = 0.3, linetype = "dashed") +
  annotate("text", x = mean_value + 20, y = 0.018, label = paste0("Mean: ", mean_formatted, "%"), vjust = 2, color = "#E16462", fontface = "bold", size = 1.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01)), breaks = seq(0, 0.015, by = 0.006)) +
  labs(x = expression('N'[2]*'O Change (%)'), y = "Density") +
  theme_classic() +
  theme(panel.background = element_rect(fill = NA, colour = NA),
        plot.background  = element_rect(fill = NA, colour = NA),
        axis.text = element_text(color = "black", size = 4),
        axis.title = element_text(size = 5),
        axis.line = element_line(color = "black", linewidth = 0.3),
        axis.ticks = element_line(color = "black", linewidth = 0.3),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
c2 <- ggdraw() +
  draw_plot(p11, x=0.03, y=0.03, width=0.94, height=0.94) +
  draw_plot(hist_plot, x=0.02, y=0.26, width=0.25, height=0.30)

print(c2)

#RT
theme_set(theme_bw())
r12 <- terra::rast('C:/Users/asus/Desktop/paper1/upscaling/LLdata_for_upscaling/N2O/scenario_SI2.tif')
r12.p <- as.data.frame(r12,xy=TRUE)
mean_value <- mean(r12.p$improvement, na.rm = TRUE)
r12.p$improvement[r12.p$improvement < -20] <- mean_value
r12.p$improvement[r12.p$improvement > 20] <- mean_value
mean_value <- mean(r12.p$improvement, na.rm = TRUE)
mean_formatted <- sprintf("%.2f", mean_value)
world <- ne_countries(scale = "medium", returnclass = "sf")
p12 <- ggplot(data = world) + geom_sf(color = "gray50", fill = "#F7F5F2") +
  geom_tile(data = r12.p, aes(x=x, y=y, fill = improvement)) +
  scale_fill_gradientn(colours = c("#2c7bb6", "#abd9e9", "#C9E995", "#fdae61", "#f48c4f"), limits = c(-120, 120), na.value = "gray80", name = expression('N'[2] * 'O Change (%)'), guide = "none") +
  theme_void() +
  theme(panel.border = element_blank(),
        plot.title = ggtext::element_markdown(hjust = 0.5, size  = 12)) +
  ggtitle("**Reduced tillage (RT)**") +
  coord_sf(crs = 4326, expand = FALSE) 

hist_plot <- ggplot(r12.p, aes(x = improvement)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "#3B528B", color = "white", alpha = 0.8) +
  geom_density(color = "#E16462", linewidth = 0.3) +
  geom_vline(xintercept = mean_value, color = "#FDE725", linewidth = 0.3, linetype = "dashed") +
  annotate("text", x = mean_value +7, y = 0.3, label = paste0("Mean: ", mean_formatted, "%"), vjust = 2, color = "#E16462", fontface = "bold", size = 1.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01)), breaks = seq(0, 0.3, by = 0.1)) +
  labs(x = expression('N'[2]*'O Change (%)'), y = "Density") +
  theme_classic() +
  theme(panel.background = element_rect(fill = NA, colour = NA),
        plot.background  = element_rect(fill = NA, colour = NA),
        axis.text = element_text(color = "black", size = 4),
        axis.title = element_text(size = 5),
        axis.line = element_line(color = "black", linewidth = 0.3),
        axis.ticks = element_line(color = "black", linewidth = 0.3),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
c3 <- ggdraw() +
  draw_plot(p12, x=0.03, y=0.03, width=0.94, height=0.94) +
  draw_plot(hist_plot, x=0.02, y=0.26, width=0.25, height=0.30)

print(c3)

#RI
theme_set(theme_bw())
r13 <- terra::rast('C:/Users/asus/Desktop/paper1/upscaling/LLdata_for_upscaling/N2O/scenario_SI3.tif')
r13.p <- as.data.frame(r13,xy=TRUE)
mean_value <- mean(r13.p$improvement, na.rm = TRUE)
r13.p$improvement[r13.p$improvement < -70] <- mean_value
r13.p$improvement[r13.p$improvement > 70] <- mean_value
mean_value <- mean(r13.p$improvement, na.rm = TRUE)
mean_formatted <- sprintf("%.2f", mean_value)
world <- ne_countries(scale = "medium", returnclass = "sf")
p13 <- ggplot(data = world) + geom_sf(color = "gray50", fill = "#F7F5F2") +
  geom_tile(data = r13.p, aes(x=x, y=y, fill = improvement)) +
  scale_fill_gradientn(colours = c("#2c7bb6", "#abd9e9", "#C9E995", "#fdae61", "#f48c4f"), limits = c(-120, 120), na.value = "gray80", name = expression('N'[2] * 'O Change (%)'), guide = "none") +
  theme_void() +
  theme(panel.border = element_blank(),
        plot.title = ggtext::element_markdown(hjust = 0.5, size  = 12)) +
  ggtitle("**Row intercropping (RI)**") +
  coord_sf(crs = 4326, expand = FALSE)  

hist_plot <- ggplot(r13.p, aes(x = improvement)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "#3B528B", color = "white", alpha = 0.8) +
  geom_density(color = "#E16462", linewidth = 0.3) +
  geom_vline(xintercept = mean_value, color = "#FDE725", linewidth = 0.3, linetype = "dashed") +
  annotate("text", x = mean_value +15, y = 0.04, label = paste0("Mean: ", mean_formatted, "%"), vjust = 2, color = "#E16462", fontface = "bold", size = 1.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01)), breaks = seq(0, 0.04, by = 0.02)) +
  labs(x = expression('N'[2]*'O Change (%)'), y = "Density") +
  theme_classic() +
  theme(panel.background = element_rect(fill = NA, colour = NA),
        plot.background  = element_rect(fill = NA, colour = NA),
        axis.text = element_text(color = "black", size = 4),
        axis.title = element_text(size = 5),
        axis.line = element_line(color = "black", linewidth = 0.3),
        axis.ticks = element_line(color = "black", linewidth = 0.3),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
c4 <- ggdraw() +
  draw_plot(p13, x=0.03, y=0.03, width=0.94, height=0.94) +
  draw_plot(hist_plot, x=0.02, y=0.26, width=0.25, height=0.30)

print(c4)

#combine figures
main_plot <- plot_grid(c2, c3, c4, c1, ncol = 2, labels = c("a", "b", "c", "d"), label_size = 16, label_fontface = "bold")
create_colorbar_legend <- function() {
  dummy_plot <- ggplot() +
    geom_point(aes(x = 1, y = 1, color = 1), alpha = 0) +
    scale_color_gradientn(colours = c("#2c7bb6", "#abd9e9", "#C9E995", "#fdae61", "#d7191c"), limits = c(-120, 120), name = "Relative Change (%)", breaks = seq(-120, 120, by = 50)) +
    theme_void() +
    theme(legend.position = "bottom",
          legend.title = element_text(size = 10, face = "bold", vjust = 0.75, hjust = 0.5),
          legend.text = element_text(size = 10),
          legend.key.width = unit(8, "mm"),
          legend.key.height = unit(6, "mm"))
  legend <- get_legend(dummy_plot)
  return(legend)
}
colorbar_legend <- create_colorbar_legend()
final_plot <- plot_grid(main_plot, colorbar_legend, ncol = 1, rel_heights = c(10, 1))
print(final_plot)

ggsave("C:/Users/asus/Desktop/paper1/Supplementary_Fig_12.tiff",
       plot = final_plot,
       width = 183, height = 120, units = "mm",
       device = "tiff",
       dpi = 300,
       compression = "lzw")

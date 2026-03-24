library(ggplot2)
library(data.table)
library(rnaturalearth)  
library(terra)
library(cowplot)
library(ggtext)

#CO2
r4 <- terra::rast('C:/Users/asus/Desktop/paper1/upscaling/LLdata_for_upscaling/CO2/scenario_4.tif')
r4.p <- as.data.frame(r4,xy=TRUE)
mean_value <- mean(r4.p$improvement, na.rm = TRUE)
r4.p$improvement[r4.p$improvement < -100] <- mean_value
r4.p$improvement[r4.p$improvement > 50] <- mean_value
mean_value <- mean(r4.p$improvement, na.rm = TRUE)
mean_formatted <- sprintf("%.2f", mean_value)
world <- ne_countries(scale = "medium", returnclass = "sf")
p4 <- ggplot(data = world) + geom_sf(color = "gray50", fill = "#F5F4E8") +
  geom_tile(data = r4.p, aes(x=x, y=y, fill = improvement)) +
  scale_fill_gradientn(colours = c("#2c7bb6", "#abd9e9", "#C9E995", "#fdae61", "#d7191c"), limits = c(-120, 100),na.value = "gray80", guide = "none") +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0, unit = "pt"),
        plot.title = ggtext::element_markdown(hjust = 0.5, size  = 12)) +
  ggtitle("**CO<sub>2</sub> Emission**") +
  coord_sf(crs = 4326, expand = FALSE)    

hist_plot <- ggplot(r4.p, aes(x = improvement)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "#3B528B", color = "white", alpha = 0.8) +
  geom_density(color = "#E16462", linewidth = 0.3) +
  geom_vline(xintercept = mean_value, color = "#FDE725", linewidth = 0.3, linetype = "dashed") +
  annotate("text", x = mean_value -10 , y = 0.04, label = paste0("Mean: ", mean_formatted, "%"), vjust = 2, color = "#E16462", fontface = "bold", size = 1.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01)), breaks = seq(0, 0.04, by = 0.02), labels = function(x) ifelse(x == 0, "0", sprintf("%.2f", x))) +
  labs(x = expression('CO'[2] * ' Change (%)'), y = "Density") +
  theme_classic() +
  theme(panel.background = element_rect(fill = NA, colour = NA),
        plot.background  = element_rect(fill = NA, colour = NA),
        axis.text = element_text(color = "black", size = 4),
        axis.title = element_text(size = 5),
        axis.line = element_line(color = "black", linewidth = 0.3),
        axis.ticks = element_line(color = "black", linewidth = 0.3),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

df <- r4.p[!is.na(r4.p$improvement), ]
dt <- as.data.table(df)

prof_range <- dt[, .(q25 = as.numeric(quantile(improvement, 0.25, na.rm = TRUE)),
                     q50 = as.numeric(quantile(improvement, 0.50, na.rm = TRUE)),
                     q75 = as.numeric(quantile(improvement, 0.75, na.rm = TRUE))), by = y]
setorder(prof_range, y)

p_range <- ggplot(prof_range, aes(y = y)) +
  geom_ribbon(aes(xmin = q25, xmax = q75), alpha = 0.5, fill = "#9fb29b", color = NA) +
  geom_path(aes(x = q50), linewidth = 0.25, color = "#216738") +
  labs(y = "Latitude (°)") +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        axis.text  = element_text(color = "black", size = 4),
        axis.title = element_text(size = 5),
        axis.line  = element_blank(),               
        axis.ticks = element_line(color = "black", linewidth = 0.3),
        plot.margin = margin(0, 0, 0, 0),
        axis.title.y = element_text(margin = margin(r = 1)),
        axis.text.y  = element_text(margin = margin(r = 1)),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background  = element_rect(fill = "transparent", colour = NA))

map_with_hist <- ggdraw() +
  draw_plot(p4) +
  draw_plot(hist_plot,x = -0.01, y = 0.20, width = 0.3, height = 0.30)

p_range_fix <- ggdraw() +
  draw_plot(p_range, x = -0.12, y = 0.12, width = 1, height = 0.6)
c1 <- plot_grid(map_with_hist, p_range_fix, nrow = 1, rel_widths = c(0.8, 0.2), align = "h")

print(c1)

#CH4
r4 <- terra::rast('C:/Users/asus/Desktop/paper1/upscaling/LLdata_for_upscaling/CH4/scenario_4.tif')
r4.p <- as.data.frame(r4,xy=TRUE)
mean_value <- mean(r4.p$improvement, na.rm = TRUE)
r4.p$improvement[r4.p$improvement < -100] <- mean_value
r4.p$improvement[r4.p$improvement > 50] <- mean_value
mean_value <- mean(r4.p$improvement, na.rm = TRUE)
mean_formatted <- sprintf("%.2f", mean_value)
world <- ne_countries(scale = "medium", returnclass = "sf")
p4 <- ggplot(data = world) + geom_sf(color = "gray50", fill = "#F5F4E8") +
  geom_tile(data = r4.p, aes(x=x, y=y, fill = improvement)) +
  scale_fill_gradientn(colours = c("#2c7bb6", "#abd9e9", "#C9E995", "#fdae61", "#d7191c"), limits = c(-120, 100), na.value = "gray80", guide = "none") +
  
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0, unit = "pt"),
        plot.title = ggtext::element_markdown(hjust = 0.5, size  = 12)) +
  ggtitle("**CH<sub>4</sub> Emission**") +
  coord_sf(crs = 4326, expand = FALSE)    

hist_plot <- ggplot(r4.p, aes(x = improvement)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "#3B528B", color = "white", alpha = 0.8) +
  geom_density(color = "#E16462", linewidth = 0.3) +
  geom_vline(xintercept = mean_value, color = "#FDE725", linewidth = 0.3, linetype = "dashed") +
  annotate("text", x = mean_value+10, y = 0.04, label = paste0("Mean: ", mean_formatted, "%"), vjust = 2, color = "#E16462", fontface = "bold", size = 1.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01)), breaks = seq(0, 0.04, by = 0.02), labels = function(x) ifelse(x == 0, "0", sprintf("%.2f", x))) +
  labs(x = expression('CH'[4] * ' Change (%)'), y = "Density") +
  theme_classic() +
  theme(panel.background = element_rect(fill = NA, colour = NA),
        plot.background  = element_rect(fill = NA, colour = NA),
        axis.text = element_text(color = "black", size = 4),
        axis.title = element_text(size = 5),
        axis.line = element_line(color = "black", linewidth = 0.3),
        axis.ticks = element_line(color = "black", linewidth = 0.3),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

df <- r4.p[!is.na(r4.p$improvement), ]
dt <- as.data.table(df)

prof_range <- dt[, .(q25 = as.numeric(quantile(improvement, 0.25, na.rm = TRUE)), q50 = as.numeric(quantile(improvement, 0.50, na.rm = TRUE)), q75 = as.numeric(quantile(improvement, 0.75, na.rm = TRUE))), by = y]
setorder(prof_range, y)
p_range <- ggplot(prof_range, aes(y = y)) +
  geom_ribbon(aes(xmin = q25, xmax = q75), alpha = 0.5, fill = "#9fb29b", color = NA) +
  geom_path(aes(x = q50), linewidth = 0.25, color = "#216738") +
  labs(y = "Latitude (°)") +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        axis.text  = element_text(color = "black", size = 4),
        axis.title = element_text(size = 5),
        axis.line  = element_blank(),
        axis.ticks = element_line(color = "black", linewidth = 0.3),
        plot.margin = margin(0, 0, 0, 0),
        axis.title.y = element_text(margin = margin(r = 1)),
        axis.text.y  = element_text(margin = margin(r = 1)),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA), 
        plot.background  = element_rect(fill = "transparent", colour = NA))

map_with_hist <- ggdraw() +
  draw_plot(p4) +
  draw_plot(hist_plot, x = -0.01, y = 0.20, width = 0.3, height = 0.30)
p_range_fix <- ggdraw() +
  draw_plot(p_range, x = -0.12, y = 0.12, width = 1, height = 0.6)
c2 <- plot_grid(map_with_hist, p_range_fix, nrow = 1, rel_widths = c(0.8, 0.2), align = "h")
print(c2)

#N2O
r4 <- terra::rast('C:/Users/asus/Desktop/paper1/upscaling/LLdata_for_upscaling/N2O/scenario_4.tif')
r4.p <- as.data.frame(r4,xy=TRUE)
mean_value <- mean(r4.p$improvement, na.rm = TRUE)
r4.p$improvement[r4.p$improvement > 100] <- mean_value
r4.p$improvement[r4.p$improvement < -100] <- mean_value
mean_formatted <- sprintf("%.2f", mean_value)
world <- ne_countries(scale = "medium", returnclass = "sf")
p4 <- ggplot(data = world) + geom_sf(color = "gray50", fill = "#F5F4E8") +
  geom_tile(data = r4.p, aes(x=x, y=y, fill = improvement)) +
  scale_fill_gradientn(colours = c("#2c7bb6", "#abd9e9", "#C9E995", "#fdae61", "#f48c4f"), 
                       limits = c(-120, 100),na.value = "gray80", guide = "none") +
  
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0, unit = "pt"),
        plot.title = ggtext::element_markdown(hjust = 0.5, size  = 12)) +
  ggtitle("**N<sub>2</sub>O Emission**") +
  coord_sf(crs = 4326, expand = FALSE)    

hist_plot <- ggplot(r4.p, aes(x = improvement)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "#3B528B", color = "white", alpha = 0.8) +
  geom_density(color = "#E16462", linewidth = 0.3) +
  geom_vline(xintercept = mean_value, color = "#FDE725", linewidth = 0.3, linetype = "dashed") +
  annotate("text", x = mean_value+20, y = 0.025, label = paste0("Mean: ", mean_formatted, "%"), vjust = 2, color = "#E16462", fontface = "bold", size = 1.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01)), breaks = seq(0, 0.03, by = 0.01), labels = function(x) ifelse(x == 0, "0", sprintf("%.2f", x))) +
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

df <- r4.p[!is.na(r4.p$improvement), ]
dt <- as.data.table(df)

prof_range <- dt[, .(q25 = as.numeric(quantile(improvement, 0.25, na.rm = TRUE)),
                     q50 = as.numeric(quantile(improvement, 0.50, na.rm = TRUE)),
                     q75 = as.numeric(quantile(improvement, 0.75, na.rm = TRUE))), by = y]
setorder(prof_range, y)

p_range <- ggplot(prof_range, aes(y = y)) +
  geom_ribbon(aes(xmin = q25, xmax = q75), alpha = 0.5, fill = "#9fb29b", color = NA) +
  geom_path(aes(x = q50), linewidth = 0.25, color = "#216738") +
  labs(y = "Latitude (°)") +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        axis.text  = element_text(color = "black", size = 4),
        axis.title = element_text(size = 5),
        axis.line  = element_blank(),
        axis.ticks = element_line(color = "black", linewidth = 0.3),
        plot.margin = margin(0, 0, 0, 0),
        axis.title.y = element_text(margin = margin(r = 1)),
        axis.text.y  = element_text(margin = margin(r = 1)),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background  = element_rect(fill = "transparent", colour = NA))

map_with_hist <- ggdraw() +
  draw_plot(p4) +
  draw_plot(hist_plot, x = -0.01, y = 0.20, width = 0.3, height = 0.30)

p_range_fix <- ggdraw() +
  draw_plot(p_range, x = -0.12, y = 0.12, width = 1, height = 0.6)

c3 <- plot_grid(map_with_hist, p_range_fix, nrow = 1, rel_widths = c(0.8, 0.2), align = "h")
print(c3)

#yield
r4 <- terra::rast('C:/Users/asus/Desktop/paper1/upscaling/LLdata_for_upscaling/yield/scenario_4.tif')
r4.p <- as.data.frame(r4,xy=TRUE)
mean_value <- mean(r4.p$improvement, na.rm = TRUE)
r4.p$improvement[r4.p$improvement > 80] <- mean_value
r4.p$improvement[r4.p$improvement < -90] <- mean_value
mean_value <- mean(r4.p$improvement, na.rm = TRUE)
mean_formatted <- sprintf("%.2f", mean_value)
world <- ne_countries(scale = "medium", returnclass = "sf")
p4 <- ggplot(data = world) + geom_sf(color = "gray50", fill = "#F5F4E8") +
  geom_tile(data = r4.p, aes(x=x, y=y, fill = improvement)) +
  scale_fill_gradientn(colours = c("#2c7bb6", "#abd9e9", "#C9E995", "#fdae61", "#f48c4f"),
                                   limits = c(-120, 100), na.value = "gray80", guide = "none") +
  
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0, unit = "pt"),
        plot.title = ggtext::element_markdown(hjust = 0.5, size  = 12)) +
  ggtitle("**Yield**") +
  coord_sf(crs = 4326, expand = FALSE)    

hist_plot <- ggplot(r4.p, aes(x = improvement)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "#3B528B", color = "white", alpha = 0.8) +
  geom_density(color = "#E16462", linewidth = 0.3) +
  geom_vline(xintercept = mean_value, color = "#FDE725", linewidth = 0.3, linetype = "dashed") +
  annotate("text", x = mean_value, y = 0.05, label = paste0("Mean: ", mean_formatted, "%"), vjust = 2, color = "#E16462", fontface = "bold", size = 1.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01)), breaks = seq(0, 0.05, by = 0.02), labels = function(x) ifelse(x == 0, "0", sprintf("%.2f", x))) +
  labs(x = "Yield Change (%)", y = "Density") +
  theme_classic() +
  theme(panel.background = element_rect(fill = NA, colour = NA),
        plot.background  = element_rect(fill = NA, colour = NA),
        axis.text = element_text(color = "black", size = 4),
        axis.title = element_text(size = 5),
        axis.line = element_line(color = "black", linewidth = 0.3),
        axis.ticks = element_line(color = "black", linewidth = 0.3),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

df <- r4.p[!is.na(r4.p$improvement), ]
dt <- as.data.table(df)
prof_range <- dt[, .(q25 = as.numeric(quantile(improvement, 0.25, na.rm = TRUE)),
                     q50 = as.numeric(quantile(improvement, 0.50, na.rm = TRUE)),
                     q75 = as.numeric(quantile(improvement, 0.75, na.rm = TRUE))), by = y]
setorder(prof_range, y)
p_range <- ggplot(prof_range, aes(y = y)) +
  geom_ribbon(aes(xmin = q25, xmax = q75), alpha = 0.5, fill = "#9fb29b", color = NA) +
  geom_path(aes(x = q50), linewidth = 0.25, color = "#216738") +
  labs(y = "Latitude (°)") +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        axis.text  = element_text(color = "black", size = 4),
        axis.title = element_text(size = 5),
        axis.line  = element_blank(),
        axis.ticks = element_line(color = "black", linewidth = 0.3),
        plot.margin = margin(0, 0, 0, 0),
        axis.title.y = element_text(margin = margin(r = 1)),
        axis.text.y  = element_text(margin = margin(r = 1)),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background  = element_rect(fill = "transparent", colour = NA))

map_with_hist <- ggdraw() +
  draw_plot(p4) +
  draw_plot(hist_plot, x = -0.01, y = 0.20, width = 0.3, height = 0.30)

p_range_fix <- ggdraw() +
  draw_plot(p_range, x = -0.12, y = 0.12, width = 1, height = 0.6)

c4 <- plot_grid(map_with_hist, p_range_fix, nrow = 1, rel_widths = c(0.8, 0.2), align = "h")

print(c4)

main_plot <- plot_grid(c1, c2, c3, c4, ncol = 2, labels = c("a", "b", "c", "d"), label_size = 16, label_fontface = "bold")
create_colorbar_legend <- function() {
  dummy_plot <- ggplot() +
    geom_point(aes(x = 1, y = 1, color = 1), alpha = 0) +
    scale_color_gradientn(colours = c("#2c7bb6", "#abd9e9", "#C9E995", "#fdae61", "#d7191c"),limits = c(-120, 100), name = "Relative Change (%)", breaks = seq(-100, 100, by = 50)) +
    theme_void() +
    theme(legend.position = "bottom",
          legend.title = element_text(size = 10, face = "bold", margin = margin(b = 10), vjust = 0.75, hjust = 0.5),
          legend.text = element_text(size = 10),
          legend.key.width = unit(8, "mm"),
          legend.key.height = unit(6, "mm"))
  
  legend <- get_legend(dummy_plot)
  return(legend)
}
colorbar_legend <- create_colorbar_legend()

final_plot <- plot_grid(main_plot, colorbar_legend, ncol = 1, rel_heights = c(10, 1))
print(final_plot)

ggsave("C:/Users/asus/Desktop/paper1/Figure_3.tiff", plot = final_plot, width = 183, height = 120, units = "mm", device = "tiff", dpi = 300, compression = "lzw")
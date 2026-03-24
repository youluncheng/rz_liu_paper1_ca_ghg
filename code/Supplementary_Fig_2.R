library(ggplot2)
library(dplyr)
library(readxl)
library(cowplot)

df <- read_excel('C:/Users/asus/Desktop/paper1/meta-analysis/greenhouse.xlsx', sheet = 4)

common_theme <- theme_bw() +
  theme(legend.position = "none",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 11, color = "black", face = "plain"),
        axis.text.y = element_text(size = 11, color = "black", face = "plain"),
        axis.title.x = element_text(size = 12, color = "black", face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 12, color = "black", face = "bold", margin = margin(r = 10)),
        plot.title = element_text(size = 14, color = "black", face = "bold", hjust = 0.5),
        axis.ticks = element_line(color = "black"))

common_y <- scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
common_fill <- scale_fill_gradient(low = "#E8F5E8", high = "#BDE5D0", name = "Count")
#management practices
management_freq <- df %>%
  count(management) %>%
  mutate(management = reorder(management, n))

p1 <- ggplot(management_freq, aes(x = management, y = n, fill = n)) +
      geom_col() +
      geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
      common_fill +
      labs(x = "Management practices", y = "Number of observations") +
      common_theme +
      common_y

print(p1)

#Crop type 
crop_freq <- df %>%
  count(crop_type) %>%
  mutate(crop_type = reorder(crop_type, n))

crop_labels <- c("maize" = "Maize", "rice" = "Rice", "wheat" = "Wheat")

p2 <- ggplot(crop_freq, aes(x = crop_type, y = n, fill = n)) +
      geom_col() +
      geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
      scale_x_discrete(labels = crop_labels) +
      common_fill +
      labs(x = "Crop type", y = "Number of observations") +
      common_theme +
      common_y

print(p2)

#combined figures
p12 <- plot_grid(p1, p2, ncol = 2, align = "hv")
print(p12)

ggsave("C:/Users/asus/Desktop/paper1/Supplementary_Fig_2.tiff", plot = p12, width = 183, height = 100, units = "mm", device = "tiff", dpi = 600, compression = "lzw")
library(readxl)
library(tidyverse)
library(patchwork)

df <- read_excel("C:/Users/asus/Desktop/paper1/Economic_value/CH4 and N2O reduction.xlsx", sheet = 1) %>%
  mutate(management = factor(management, levels = c("NT", "RT", "ROT", "RI", "Total")),
         CH4 = as.numeric(CH4),
         N2O = as.numeric(N2O))
df_long <- df %>%
  pivot_longer(cols = c(CH4, N2O), names_to  = "metric", values_to = "value")
mk_rose <- function(data, title, colors) {
  ggplot(data, aes(x = management, y = value, fill = value)) +
    geom_col(width = 1, color = "white") +
    coord_polar(start = -pi/2) +
    scale_fill_stepsn(colours = colors, n.breaks = 6, show.limits = TRUE) +
    guides(fill = guide_bins(title = expression("value\nTg CO"[2]*"-eq/yr"), show.limits = TRUE)) +
    labs(title = title, x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(axis.text.y = element_blank(),
          axis.text.x = element_text(color = "black"),
          axis.ticks = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
          legend.key.height = unit(0.3, "cm"),
          legend.key.width = unit(0.3, "cm"),
          legend.title = element_text(size = 9),
          legend.text = element_text(size = 8),
          plot.title = element_text(face = "bold", hjust = 0.5))
}
pal_CH4 <- c("#d9f0ff", "#a6d8ff", "#6baed6", "#4f7fc4", "#2c4aa5", "#1f3b8f")
pal_N2O <- c("#fee391", "#fdbb84", "#c7d66d", "#7fbf7b", "#5a8f4e", "#3f6f3a")
p_ch4 <- mk_rose(df_long %>% filter(metric == "CH4"), expression("CH"[4]*" reduction"), pal_CH4)
p_n2o <- mk_rose(df_long %>% filter(metric == "N2O"), expression("N"[2]*"O reduction"), pal_N2O)
p_final <- (p_ch4 + p_n2o) &
  theme(plot.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0))

p_final

ggsave("C:/Users/asus/Desktop/paper1/Supplementary_Fig_14.tiff", plot = p_final, width = 175, height = 70, units = "mm", device = "tiff", dpi = 300, compression = "lzw")

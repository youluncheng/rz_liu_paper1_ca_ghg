library(ggplot2)
library(dplyr)
library(readxl)
library(ggpubr)

value_colors <- c("Significant Positive" = "#d73027",
                  "Significant Negative" = "#1a9850",
                  "Non-significant Positive" = "#fc8d59",
                  "Non-significant Negative" = "#91cf60")
#CO2
data <- read_xlsx("C:/Users/asus/Desktop/paper1/upscaling data/m1_co2_results.xlsx")
df <- data %>%
  mutate(X = 1:n(),value_color = case_when(pval < 0.05 & estimate > 0 ~ "Significant Positive",
                                           pval < 0.05 & estimate < 0 ~ "Significant Negative",
                                           pval >= 0.05 & estimate > 0 ~ "Non-significant Positive",
                                           pval >= 0.05 & estimate < 0 ~ "Non-significant Negative"))
p1 <- ggplot(df, aes(x = X, y = estimate)) +
  geom_segment(aes(xend = X, yend = 0, color = value_color), linewidth = 1) +
  geom_errorbar(aes(ymin = ci.lb, ymax = ci.ub, color = value_color), width = 0.2, alpha = 0.6) +
  geom_point(aes(color = value_color), shape = 16, size = 4) +
  geom_text(aes(y = ifelse(estimate > 0, ci.ub-0.005, ci.lb-0.01), label = significance, vjust = ifelse(estimate > 0, 0, 1)), size = 4, color = "black") +
  scale_x_continuous(breaks = 1:nrow(df), labels = df$term) +
  scale_y_continuous(breaks = pretty(df$estimate, n = 2), limits = c(-0.3, 0.3), labels = function(x) ifelse(x == 0, "0", sprintf("%.1f", x))) +
  scale_color_manual(values = value_colors, guide = "none") +
  labs(y = "CO\u2082 emissions\nparameter estimate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8, vjust = 1, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 8, margin = margin(r = 10)),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill = "#FCFFFB", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
        axis.ticks.x = element_line(color = "black", linewidth = 0.5),
        axis.ticks.y = element_line(color = "black", linewidth = 0.5),
        axis.ticks.length.x = unit(0.1, "cm"),
        axis.ticks.length.y = unit(0.1, "cm")
  ) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.5)

print(p1)

#CH4
data <- read_xlsx("C:/Users/asus/Desktop/paper1/upscaling data/m1_ch4_results.xlsx")
df <- data %>%
  mutate(X = 1:n(),value_color = case_when(pval < 0.05 & estimate > 0 ~ "Significant Positive",
                                           pval < 0.05 & estimate < 0 ~ "Significant Negative",
                                           pval >= 0.05 & estimate > 0 ~ "Non-significant Positive",
                                           pval >= 0.05 & estimate < 0 ~ "Non-significant Negative"))
p2 <- ggplot(df, aes(x = X, y = estimate)) +
  geom_segment(aes(xend = X, yend = 0, color = value_color), linewidth = 1) +
  geom_errorbar(
    aes(ymin = ci.lb, ymax = ci.ub, color = value_color), width = 0.2, alpha = 0.6) +
  geom_point(aes(color = value_color), shape = 16, size = 4) +
  geom_text(aes(y = ifelse(estimate > 0, ci.ub-0.005, ci.lb-0.01), label = significance, vjust = ifelse(estimate > 0, 0, 1)), size = 4, color = "black") +
  scale_x_continuous(breaks = 1:nrow(df), labels = df$term) +
  scale_y_continuous(breaks = pretty(df$estimate, n = 1), limits = c(-1.2, 1.2), labels = function(x) ifelse(x == 0, "0", sprintf("%.1f", x))) +
  scale_color_manual(values = value_colors, guide = "none") +
  labs(y = "CH\u2084 emissions\nparameter estimate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8, vjust = 1, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 8, margin = margin(r = 10)),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill = "#FCFFFB", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
        axis.ticks.x = element_line(color = "black", linewidth = 0.5),
        axis.ticks.y = element_line(color = "black", linewidth = 0.5),
        axis.ticks.length.x = unit(0.1, "cm"),
        axis.ticks.length.y = unit(0.1, "cm")
  ) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.5)

print(p2)

#N2O
data <- read_xlsx("C:/Users/asus/Desktop/paper1/upscaling data/m1_n2o_results.xlsx")
df <- data %>%
  mutate(X = 1:n(), value_color = case_when(pval < 0.05 & estimate > 0 ~ "Significant Positive",
                                            pval < 0.05 & estimate < 0 ~ "Significant Negative",
                                            pval >= 0.05 & estimate > 0 ~ "Non-significant Positive",
                                            pval >= 0.05 & estimate < 0 ~ "Non-significant Negative"))
p3 <- ggplot(df, aes(x = X, y = estimate)) +
  geom_segment(aes(xend = X, yend = 0, color = value_color), linewidth = 1) +
  geom_errorbar(aes(ymin = ci.lb, ymax = ci.ub, color = value_color), width = 0.2, alpha = 0.6) +
  geom_point(aes(color = value_color), shape = 16, size = 4) +
  geom_text(aes(y = ifelse(estimate > 0, ci.ub-0.005, ci.lb-0.01), label = significance, vjust = ifelse(estimate > 0, 0, 1)), size = 4, color = "black") +
  scale_x_continuous(breaks = 1:nrow(df), labels = df$term) +
  scale_y_continuous(breaks = pretty(df$estimate, n = 1), limits = c(-0.5, 0.5), labels = function(x) ifelse(x == 0, "0", sprintf("%.1f", x))) +
  scale_color_manual(values = value_colors, guide = "none") +
  labs(y = "N\u2082O emissions\nparameter estimate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8, vjust = 1, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 8, margin = margin(r = 10)),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill = "#FCFFFB", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
        axis.ticks.x = element_line(color = "black", linewidth = 0.5),
        axis.ticks.y = element_line(color = "black", linewidth = 0.5),
        axis.ticks.length.x = unit(0.1, "cm"),
        axis.ticks.length.y = unit(0.1, "cm")
  ) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.5)

print(p3)

#yield
data <- read_xlsx("C:/Users/asus/Desktop/paper1/upscaling data/m1_yield_results.xlsx")
df <- data %>%
  mutate(X = 1:n(), value_color = case_when(pval < 0.05 & estimate > 0 ~ "Significant Positive",
                                            pval < 0.05 & estimate < 0 ~ "Significant Negative",
                                            pval >= 0.05 & estimate > 0 ~ "Non-significant Positive",
                                            pval >= 0.05 & estimate < 0 ~ "Non-significant Negative"))
p4 <- ggplot(df, aes(x = X, y = estimate)) +
  geom_segment(aes(xend = X, yend = 0, color = value_color), linewidth = 1) +
  geom_errorbar(aes(ymin = ci.lb, ymax = ci.ub, color = value_color), width = 0.2, alpha = 0.6) +
  geom_point(aes(color = value_color), shape = 16, size = 4) +
  geom_text(aes(y = ifelse(estimate > 0, ci.ub-0.005, ci.lb-0.01), label = significance, vjust = ifelse(estimate > 0, 0, 1)), size = 4, color = "black") +
  scale_x_continuous(breaks = 1:nrow(df), labels = df$term) +
  scale_y_continuous(breaks = pretty(df$estimate, n = 2), limits = c(-0.18, 0.18) , labels = function(x) ifelse(x == 0, "0", sprintf("%.1f", x))) +
  scale_color_manual(values = value_colors, guide = "none") +
  labs(y = "Yield\nparameter estimate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8, vjust = 1, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 8, margin = margin(r = 10)),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill = "#FCFFFB", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
        axis.ticks.x = element_line(color = "black", linewidth = 0.5),
        axis.ticks.y = element_line(color = "black", linewidth = 0.5),
        axis.ticks.length.x = unit(0.1, "cm"),
        axis.ticks.length.y = unit(0.1, "cm")
  ) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.5)

print(p4)

g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g3 <- ggplotGrob(p3)
g4 <- ggplotGrob(p4)
max_h <- grid::unit.pmax(g1$heights, g2$heights, g3$heights, g4$heights)
g1$heights <- max_h
g2$heights <- max_h
g3$heights <- max_h
g4$heights <- max_h

p <- ggarrange(g1, g2, g3, g4, ncol = 1, nrow = 4, heights = rep(1, 4), labels = c("a","b","c","d"),
               font.label = list(size = 12, color = "black", face = "bold"),
               label.x = 0.01, label.y = 1.15, align = "v")

p_final <- annotate_figure(p, top = text_grob(" ", size = 6), bottom = text_grob("Management practices and site conditions", size = 10))
print(p_final)
ggsave("C:/Users/asus/Desktop/paper1/Figure_2.tiff", plot = p_final, width = 183, height = 170, units = "mm", compression = "lzw")

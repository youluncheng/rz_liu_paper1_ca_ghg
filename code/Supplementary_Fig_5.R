library(readxl)
library(data.table)
library(ggplot2)
library(cowplot)

#CO2
d02 <- as.data.table(read_xlsx("C:/Users/asus/Desktop/paper1/meta-analysis/CO2_d02.xlsx"))
d02 <- d02[!is.na(co2c_mean) & !is.na(co2t_mean)]
d02 <- d02[co2c_mean > 0 & co2t_mean > 0]

q1 <- ggplot(d02, aes(x = co2c_mean, y = co2t_mean, color = management)) +
  geom_point(alpha = 0.45, size = 0.8) +
  geom_abline(slope = 1,   intercept = 0, linetype = 2, linewidth = 0.8, alpha = 0.8) +
  geom_abline(slope = 1.2, intercept = 0, linetype = 3, linewidth = 0.6, alpha = 0.4) +
  geom_abline(slope = 0.8, intercept = 0, linetype = 3, linewidth = 0.6, alpha = 0.4) +
  scale_x_log10(breaks = c(500, 5000, 50000), labels = c("500", "5000", "50000")) +
  scale_y_log10(breaks = c(500, 5000, 50000), labels = c("500", "5000", "50000")) +
  coord_equal() +
  theme_bw() +
  labs(x = "<b>CO<sub>2</sub> control mean</b>", y = "<b>CO<sub>2</sub> treatment mean</b>") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.key.size   = unit(3, "mm"),
        legend.spacing.y  = unit(3, "mm"),
        legend.title      = element_text(size = 8, colour = "black", face = "plain"),
        legend.text       = element_text(size = 8, colour = "black"),
        axis.title.x      = ggtext::element_markdown(size = 8, colour = "black"),
        axis.title.y      = ggtext::element_markdown(size = 8, colour = "black"),
        axis.text.x       = element_text(size = 8, colour = "black"),
        axis.text.y       = element_text(size = 8, colour = "black"))

#CH4
d02 <- as.data.table(read_xlsx("C:/Users/asus/Desktop/paper1/meta-analysis/CH4_d02.xlsx"))
d02 <- d02[!is.na(ch4c_mean) & !is.na(ch4t_mean)]
d02 <- d02[ch4c_mean > 0 & ch4t_mean > 0]

q2 <- ggplot(d02, aes(x = ch4c_mean, y = ch4t_mean, color = management)) +
  geom_point(alpha = 0.45, size = 0.8) +
  geom_abline(slope = 1,   intercept = 0, linetype = 2, linewidth = 0.8, alpha = 0.8) +
  geom_abline(slope = 1.2, intercept = 0, linetype = 3, linewidth = 0.6, alpha = 0.4) +
  geom_abline(slope = 0.8, intercept = 0, linetype = 3, linewidth = 0.6, alpha = 0.4) +
  scale_x_log10(breaks = c(0.1, 1, 10, 100, 1000), labels = c("0.1", "1", "10", "100", "1000")) +
  scale_y_log10(breaks = c(0.1, 1, 10, 100, 1000), labels = c("0.1", "1", "10", "100", "1000")) +
  coord_equal() +
  theme_bw() +
  labs(x = "<b>CH<sub>4</sub> control mean</b>", y = "<b>CH<sub>4</sub> treatment mean</b>") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.key.size   = unit(3, "mm"),
        legend.spacing.y  = unit(3, "mm"),
        legend.title      = element_text(size = 8, colour = "black", face = "plain"),
        legend.text       = element_text(size = 8, colour = "black"),
        axis.title.x      = ggtext::element_markdown(size = 8, colour = "black"),
        axis.title.y      = ggtext::element_markdown(size = 8, colour = "black"),
        axis.text.x       = element_text(size = 8, colour = "black"),
        axis.text.y       = element_text(size = 8, colour = "black"))

#N2O
d02 <- as.data.table(read_xlsx("C:/Users/asus/Desktop/paper1/meta-analysis/N2O_d02.xlsx"))
d02 <- d02[!is.na(n2oc_mean) & !is.na(n2ot_mean)]
d02 <- d02[n2oc_mean > 0 & n2ot_mean > 0]

q3 <- ggplot(d02, aes(x = n2oc_mean, y = n2ot_mean, color = management)) +
  geom_point(alpha = 0.45, size = 0.8) +
  geom_abline(slope = 1,   intercept = 0, linetype = 2, linewidth = 0.8, alpha = 0.8) +
  geom_abline(slope = 1.2, intercept = 0, linetype = 3, linewidth = 0.6, alpha = 0.4) +
  geom_abline(slope = 0.8, intercept = 0, linetype = 3, linewidth = 0.6, alpha = 0.4) +
  scale_x_log10(breaks = c(100, 1000, 10000), labels = c("100", "1000", "10000")) +
  scale_y_log10(breaks = c(100, 1000, 10000), labels = c("100", "1000", "10000")) +
  coord_equal() +
  theme_bw() +
  labs(x = "<b>N<sub>2</sub>O control mean</b>", y = "<b>N<sub>2</sub>O treatment mean</b>") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.key.size   = unit(3, "mm"),
        legend.spacing.y  = unit(3, "mm"),
        legend.title      = element_text(size = 8, colour = "black", face = "plain"),
        legend.text       = element_text(size = 8, colour = "black"),
        axis.title.x      = ggtext::element_markdown(size = 8, colour = "black"),
        axis.title.y      = ggtext::element_markdown(size = 8, colour = "black"),
        axis.text.x       = element_text(size = 8, colour = "black"),
        axis.text.y       = element_text(size = 8, colour = "black"))

#Yield
d02 <- as.data.table(read_xlsx("C:/Users/asus/Desktop/paper1/meta-analysis/Yield_d02.xlsx"))
d02 <- d02[!is.na(yieldc_mean) & !is.na(yieldt_mean)]
d02 <- d02[yieldc_mean > 0 & yieldt_mean > 0]

q4 <- ggplot(d02, aes(x = yieldc_mean, y = yieldt_mean, color = management)) +
  geom_point(alpha = 0.45, size = 0.8) +
  geom_abline(slope = 1,   intercept = 0, linetype = 2, linewidth = 0.8, alpha = 0.8) +
  geom_abline(slope = 1.2, intercept = 0, linetype = 3, linewidth = 0.6, alpha = 0.4) +
  geom_abline(slope = 0.8, intercept = 0, linetype = 3, linewidth = 0.6, alpha = 0.4) +
  scale_x_log10(breaks = c(1000, 3000, 12000), labels = c("1000", "3000", "12000")) +
  scale_y_log10(breaks = c(1000, 3000, 12000), labels = c("1000", "3000", "12000")) +
  coord_equal() +
  theme_bw() +
  labs(x = "Yield control mean", y = "Yield treatment mean") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.key.size   = unit(3, "mm"),
        legend.spacing.y  = unit(3, "mm"),
        legend.title      = element_text(size = 8, colour = "black", face = "plain"),
        legend.text       = element_text(size = 8, colour = "black"),
        axis.title.x      = element_text(size = 8, colour = "black", face = "bold"),
        axis.title.y      = element_text(size = 8, colour = "black", face = "bold"),
        axis.text.x       = element_text(size = 8, colour = "black"),
        axis.text.y       = element_text(size = 8, colour = "black"))

legend_shared <- get_legend(q1 + theme(legend.position = "right"))

q1_noleg <- q1 + theme(legend.position = "none")
q2_noleg <- q2 + theme(legend.position = "none")
q3_noleg <- q3 + theme(legend.position = "none")
q4_noleg <- q4 + theme(legend.position = "none")

panel_2x2 <- plot_grid(q1_noleg, q2_noleg, q3_noleg, q4_noleg, ncol = 2, nrow = 2, align = "hv",
                       labels = c("a", "b", "c", "d"),
                       label_fontface = "bold",
                       label_size = 10,
                       label_x = 0.1,
                       label_y = 0.98,
                       hjust = 0, vjust = 1)

p_final <- plot_grid(panel_2x2, legend_shared, ncol = 2, rel_widths = c(1, 0.18))

print(p_final)

ggsave("C:/Users/asus/Desktop/paper1/Supplementary_Fig_5.tiff",
       plot = p_final,
       width = 150, height = 100, units = "mm",
       device = "tiff",
       dpi = 300,
       compression = "lzw",
       bg = "white")

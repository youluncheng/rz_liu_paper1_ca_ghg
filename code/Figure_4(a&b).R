library(readxl)
library(dplyr)
library(ggplot2)

#a
data <- read_excel("C:/Users/asus/Desktop/paper1/Economic_value/Fig_4.xlsx", sheet = 1) %>%
  transmute(management = as.character(management),
            EmissionReduction = as.numeric(EmissionReduction),
            EmissionEconomicValue = as.numeric(EmissionEconomicValue))

data$management <- factor(data$management, levels = c("NT", "RT", "ROT", "RI"))

p <- ggplot(data, aes(x = management)) +
  geom_bar(aes(y = EmissionReduction, fill = management), stat = "identity", width = 0.5) +
  scale_fill_manual(values = c("NT" = "#ddeed9", "RT" = "#b3d7ae", "ROT" = "#7dba7f", "RI" = "#44935b"),
                    guide = "none") +

  geom_line(aes(y = EmissionEconomicValue * 500 / 10000, group = 1), color = "red", linewidth = 1) +
  geom_point(aes(y = EmissionEconomicValue * 500 / 10000), color = "red", size = 3) +
  scale_y_continuous(name = expression(paste("Emission Reduction (Tg ", CO[2], "-eq/yr)")),
                     limits = c(0, 500),
                     expand = c(0, 0),
                     sec.axis = sec_axis(transform = ~ . * 10000 / 500, name = "Emission Reduction Benefit(million USD/yr)", breaks = seq(0, 10000, by = 2000))) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.text.y.right = element_text(size = 12, color = "red"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.y.right = element_text(size = 14, color = "red"),
        panel.grid = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.6),
        axis.line.x = element_line(color = "black", linewidth = 0.6),
        axis.line.y.left = element_line(color = "black", linewidth = 0.6),
        axis.line.y.right = element_line(color = "red", linewidth = 0.6),
        panel.border = element_blank(),
        axis.ticks = element_line(color = "black", linewidth = 0.6),
        axis.ticks.x = element_line(color = "black", linewidth = 0.6),
        axis.ticks.y = element_line(color = "black", linewidth = 0.6),
        axis.ticks.y.right = element_line(color = "red", linewidth = 0.6),
        axis.ticks.length = unit(0.2, "cm"),
        panel.background  = element_rect(fill = "transparent", colour = NA),
        plot.background   = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.box.background = element_rect(fill = "transparent", colour = NA)
  )+
  labs(x = "Conservation tillage practices") +
  annotate(geom = "segment", x = -Inf, xend = Inf, y = Inf, yend = Inf, color = "black", linewidth = 1)

print(p)
ggsave(
  filename = "C:/Users/asus/Desktop/paper1/Fig_4a.png", plot = p, width = 6, height = 4.5, dpi = 300, bg = "transparent")

#b
data <- read_excel("C:/Users/asus/Desktop/paper1/Economic_value/Fig_4.xlsx", sheet = 1) %>%
  transmute(management = as.character(management),
            YieldIncrease = as.numeric(YieldIncrease),
            YieldEconomicValue = as.numeric(YieldEconomicValue))

data$management <- factor(data$management, levels = c("NT", "RT", "ROT", "RI"))

q <- ggplot(data, aes(x = management)) +
  geom_bar(aes(y = YieldIncrease, fill = management), stat = "identity", width = 0.5) +
  scale_fill_manual(values = c("NT" = "#ddeed9", "RT" = "#b3d7ae", "ROT" = "#7dba7f", "RI" = "#44935b"), guide = "none") +
  geom_line(aes(y = YieldEconomicValue * 100 / 25000, group = 1), color = "red", linewidth = 1) +
  geom_point(aes(y = YieldEconomicValue * 100 / 25000), color = "red", size = 3) +
  scale_y_continuous(name = expression(paste("Yield Increase (Million Tons/yr)")),
                     limits = c(-50, 100),
                     expand = c(0, 0),
                     sec.axis = sec_axis(transform = ~ . * 25000 / 100, name = "Yield Increase Economic Value (Million USD/yr)", breaks = seq(-25000, 25000, by = 10000))) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.text.y.right = element_text(size = 12, color = "red"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.y.right = element_text(size = 14, color = "red"),
        panel.grid = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.6),
        axis.line.x = element_line(color = "black", linewidth = 0.6),
        axis.line.y.left = element_line(color = "black", linewidth = 0.6),
        axis.line.y.right = element_line(color = "red", linewidth = 0.6),
        panel.border = element_blank(),
        axis.ticks = element_line(color = "black", linewidth = 0.6),
        axis.ticks.x = element_line(color = "black", linewidth = 0.6),
        axis.ticks.y = element_line(color = "black", linewidth = 0.6),
        axis.ticks.y.right = element_line(color = "red", linewidth = 0.6),
        axis.ticks.length = unit(0.2, "cm"),
        panel.background  = element_rect(fill = "transparent", colour = NA),
        plot.background   = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.box.background = element_rect(fill = "transparent", colour = NA))+
  labs(x = "Conservation tillage practices") +
  annotate(geom = "segment", x = -Inf, xend = Inf, y = Inf, yend = Inf, color = "black", linewidth = 1)+
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.6)

print(q)
ggsave(filename = "C:/Users/asus/Desktop/paper1/Fig_4b.png", plot = q, width = 6, height = 5, dpi = 300, bg = "transparent")
library(ggplot2)
library(patchwork)
library(grid)
library(readr)

df <- read_csv("C:/Users/asus/Desktop/d02.csv")

rug_big <- geom_rug(sides = "b", alpha = 0.6, linewidth = 0.2, length = unit(0.08, "npc"))

base_theme <- theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y  = element_blank(),
        axis.title.x = element_blank(),
        plot.margin  = margin(1, 4, 1, 4),
        axis.text.x  = element_text(color = "black"),
        axis.ticks.x = element_line(color = "black"),
        axis.line.x  = element_line(color = "black"))

make_density <- function(data, var, fill_col) {
  x <- data[[var]]
  x <- x[!is.na(x)]
  plot_df <- data.frame(x = x)
  
  label_txt <- dplyr::case_when(var == "mat"    ~ "MAT",
                                var == "map"    ~ "MAP",
                                var == "ph"     ~ "pH",
                                var == "clay"   ~ "Clay",
                                var == "soc"    ~ "SOC",
                                var == "n_dose" ~ "N dose",
                                TRUE            ~ var)
  
  ggplot(plot_df, aes(x = x)) +
    geom_density(fill = fill_col, alpha = 0.7, linewidth = 0.2) +
    rug_big +
    annotate("text", x = Inf, y = Inf, label = label_txt, hjust = 1.1, vjust = 1.2, size = 4) +
    base_theme
}
p1 <- make_density(df, "mat",    "#686dcb")
p2 <- make_density(df, "map",    "#3abbc9")
p3 <- make_density(df, "ph",     "#9bca3e") 
p4 <- make_density(df, "clay",   "#feeb51")
p5 <- make_density(df, "soc",    "#ffb92a")
p6 <- make_density(df, "n_dose", "#ed5314")

p_all <- p1 / p2 / p3 / p4 / p5 / p6

p_all

ggsave("C:/Users/asus/Desktop/paper1/Supplementary_Fig_4.tiff", plot = p_all, width = 183, height = 160, units = "mm", device = "tiff", dpi = 600, compression = "lzw")

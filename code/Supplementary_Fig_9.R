library(randomForest)
library(rfPermute)
library(ggplot2)
library(RColorBrewer)
library(readxl)
library(dplyr)
library(ggpubr)

data <- read_excel('C:/Users/asus/Desktop/paper1/meta-analysis/greenhouse.xlsx',sheet = 4)
data_pca <- data %>% 
  filter(!is.na(yieldt_mean) & !is.na(yieldc_mean))
message("Original data: ", nrow(data), " rows | Filtered data: ", nrow(data_pca), " rows")
numeric_vars <- c("mat", "map", "clay", "ph", "soc", "n_dose")
for(var in numeric_vars) {
  if(var %in% names(data_pca)) {
    if(sum(is.na(data_pca[[var]])) > 0) {
      median_val <- median(data_pca[[var]], na.rm = TRUE)
      data_pca[is.na(data_pca[[var]]), var] <- median_val
    }
  }
}
data_pca$yield_ratio <- data_pca$yieldt_mean / data_pca$yieldc_mean
feature_columns <- c("mat", "map", "clay", "ph", "soc", "crop_type", "n_dose")
data_pca$crop_type <- as.factor(data_pca$crop_type)
data_pca$management <- as.factor(data_pca$management)
results_list <- list()
plot_list <- list()
label_map <- c(mat = "MAT", map = "MAP", clay = "Clay", ph = "pH", soc = "SOC", crop_type = "Crop Type", n_dose = "N dose")
analyze_management <- function(mgmt, y_limits = NULL) {
  cat("Analyzing conservation tillage practice:", mgmt, "\n")
  subset_data <- data_pca[data_pca$management == mgmt, ]
  if(nrow(subset_data) > 10) {
    model_formula <- as.formula(paste("yield_ratio ~", paste(feature_columns, collapse = " + ")))
    set.seed(123)
    rf_model <- randomForest(model_formula, ntree = 500, data = subset_data, importance = TRUE)
    set.seed(123)
    factor_rfP <- rfPermute(model_formula, data = subset_data, importance = TRUE, ntree = 500, num.cores = 6)
    importance_factor.scale <- data.frame(importance(factor_rfP, scale = TRUE), check.names = FALSE)
    importance_factor.scale.pval <- (factor_rfP$pval)[ , , 2]
    importance_factor.scale <- importance_factor.scale[order(importance_factor.scale$'%IncMSE', decreasing = TRUE), ]
    for (feature in rownames(importance_factor.scale)) {
      importance_factor.scale[feature, '%IncMSE.pval'] <- importance_factor.scale.pval[feature, '%IncMSE']
      if (importance_factor.scale[feature, '%IncMSE.pval'] >= 0.05) importance_factor.scale[feature, '%IncMSE.sig'] <- ''
      else if (importance_factor.scale[feature, '%IncMSE.pval'] >= 0.01 & importance_factor.scale[feature, '%IncMSE.pval'] < 0.05) importance_factor.scale[feature, '%IncMSE.sig'] <- '*'
      else if (importance_factor.scale[feature, '%IncMSE.pval'] >= 0.001 & importance_factor.scale[feature, '%IncMSE.pval'] < 0.01) importance_factor.scale[feature, '%IncMSE.sig'] <- '**'
      else if (importance_factor.scale[feature, '%IncMSE.pval'] < 0.001) importance_factor.scale[feature, '%IncMSE.sig'] <- '***'
    }
    results_list[[mgmt]] <<- list(management = mgmt, model = rf_model, factor_rfP = factor_rfP, importance = importance_factor.scale, sample_size = nrow(subset_data))
    importance_factor.scale$feature_name <- rownames(importance_factor.scale)
    importance_factor.scale$feature_name <- factor(importance_factor.scale$feature_name, levels = importance_factor.scale$feature_name)
    if(is.null(y_limits)) {
      y_max <- max(importance_factor.scale$`%IncMSE`) * 1.15
      y_limits <- c(0, y_max)
    }
    p <- ggplot() +
      geom_col(data = importance_factor.scale, 
               aes(x = reorder(feature_name, `%IncMSE`), y = `%IncMSE`, fill = `%IncMSE`),
               colour = 'black',size = 0.25, width = 0.8) +
      coord_flip() +
      scale_x_discrete(labels = label_map) +
      scale_fill_gradientn(colors = c(low = brewer.pal(5, "Blues")[2], mid = brewer.pal(5, "YlGnBu")[1], high = brewer.pal(6, "OrRd")[4]),
                           values = scales::rescale(c(0, max(importance_factor.scale$`%IncMSE`)/3, max(importance_factor.scale$`%IncMSE`))),
                           guide = FALSE,
                           aesthetics = "fill") +
      labs(title = mgmt, x = NULL, y = 'Increase in MSE (%)', fill = NULL) +
      theme(panel.background = element_rect(fill = "white", colour = NA),
            panel.grid = element_blank(),
            panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
            axis.line = element_line(colour = "black", size = 0),
            axis.ticks = element_line(colour = "black", size = 0.25),
            axis.text.x = element_text(size = 12, color = "black"),
            axis.text.y = element_text(size = 12, color = "black"),
            axis.title.x = element_text(size = 14, color = "black"),
            axis.title.y = element_text(size = 14, color = "black"),
            plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = y_limits) +
      geom_text(data = importance_factor.scale,
                aes(x = reorder(feature_name, `%IncMSE`), y = `%IncMSE`, label = sprintf("%.1f%% %s", `%IncMSE`, `%IncMSE.sig`)),
                hjust = ifelse(importance_factor.scale$`%IncMSE` >= 0, -0.1, 1.2),
                size = 3, 
                nudge_y = 0.3)
    y_stat <- ifelse(mgmt == "RI", y_limits[2] * 0.78, y_limits[2] * 0.81)
    p <- p +
      annotate("text", label = sprintf("n = %d", nrow(subset_data)), x = 1, y = y_stat, size = 3, hjust = 0) +
      annotate("text", label = sprintf("R² = %.3f", mean(factor_rfP[["rf"]][["rsq"]])), x = 2, y = y_stat, size = 3, hjust = 0)
    plot_list[[mgmt]] <<- p
    ggsave(p, file = paste0("yield_importance_", mgmt, ".pdf"), width = 6, height = 8)
    return(p)
  }
}
p_RES <- analyze_management("RES", y_limits = c(0, 26))
p_CC <- analyze_management("CC", y_limits = c(0, 22)) 
p_NT <- analyze_management("NT", y_limits = c(0, 28))
p_RT <- analyze_management("RT", y_limits = c(0, 16))
p_ROT <- analyze_management("ROT", y_limits = c(0, 20))
p_RI <- analyze_management("RI", y_limits = c(0, 24))

print(p_RES)
print(p_CC) 
print(p_NT)
print(p_RT)
print(p_ROT)
print(p_RI)

#combined figures
p_combined <- ggarrange(p_RES, p_CC, p_NT, p_RT, p_ROT, p_RI,
                        heights = rep(4, 6), 
                        ncol = 2, nrow = 3,
                        labels = c("a", "b", "c", "d", "e", "f"), 
                        font.label = list(size = 20, color = "black", face = "bold"),
                        label.x = 0.04,
                        label.y = 1)  

print(p_combined)

ggsave("C:/Users/asus/Desktop/paper1/Supplementary_Fig_9.tiff",
       plot = p_combined,
       width = 183, height = 200, units = "mm",
       device = "tiff",
       dpi = 600,
       compression = "lzw")


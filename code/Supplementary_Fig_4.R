library(metafor)
library(readxl)

mgs <- c("CC", "NT", "RES", "RI", "ROT", "RT")

files <- c("C:/Users/asus/Desktop/paper1/meta-analysis/CO2_d02.xlsx",
           "C:/Users/asus/Desktop/paper1/meta-analysis/CH4_d02.xlsx",
           "C:/Users/asus/Desktop/paper1/meta-analysis/N2O_d02.xlsx",
           "C:/Users/asus/Desktop/paper1/meta-analysis/yield_d02.xlsx")

row_labels <- list(expression(CO[2]~"(a)"),
                   expression(CH[4]~"(b)"),
                   expression(N[2]*O~"(c)"),
                   "yield(d)")
row_names <- c("CO2", "CH4", "N2O", "yield")
row_x <- list(CO2   = list(xlim = c(-1.5, 1.5), xticks = c(-1.5, 0, 1.5)),
              CH4   = list(xlim = c(-2,   2),   xticks = c(-2, -1, 0, 1, 2)),
              N2O   = list(xlim = c(-1,   2),   xticks = c(-1, 0, 1, 2)),
              yield = list(xlim = c(-0.8, 0.8), xticks = c(-0.5, 0, 0.5)))

dlist <- lapply(files, function(f){
  dat <- as.data.frame(read_excel(f))
  dat <- na.omit(dat[, c("study_id", "management", "yi", "vi")])
  dat <- subset(dat, is.finite(yi) & is.finite(vi) & vi > 0)
  dat
})

row_scales <- lapply(dlist, function(dat){
  
  se_all <- sqrt(dat$vi)
  ylim_row <- c(as.numeric(quantile(se_all, 0.01, na.rm = TRUE)),
                as.numeric(quantile(se_all, 0.975, na.rm = TRUE)))
  
  yticks_row <- pretty(ylim_row, n = 4)
  
  list(ylim = ylim_row, yticks = yticks_row)
})

tiff("C:/Users/asus/Desktop/paper1/Supplementary_Fig_4.tiff", units = "mm", width = 183, height = 125, res = 300, compression = "lzw")

op <- par(mfrow = c(4, 6), mar  = c(1.0, 1.6, 1.2, 0.2), oma  = c(2.4, 4.2, 1.2, 0.6), mgp  = c(1.6, 0.45, 0), tck  = -0.03, xaxs = "i", yaxs = "i", lend = "butt")

for (r in seq_along(dlist)) {
  
  dat <- dlist[[r]]
  row_name <- tools::file_path_sans_ext(basename(files[r]))
  
  ylim_row <- row_scales[[r]]$ylim
  yticks_row <- row_scales[[r]]$yticks
  
  for (c in seq_along(mgs)) {
    
    mg <- mgs[c]
    subd <- subset(dat, management == mg)
    
    if (nrow(subd) < 5) {
      plot.new()
      title(main = paste0(row_name, "\n", mg, " (too few)"), cex.main = 0.9)
      next
    }
    
    res <- rma(yi, vi, data = subd, method = "REML")
    funnel(res, refline = as.numeric(res$b), level = c(90, 95, 99), shade = c("white", "gray75", "gray55"),
           xlim = row_x[[ row_names[r] ]]$xlim, ylim = ylim_row,
           pch = 16, col = "black", cex = 0.5, lwd = 0.6,
           main = if (r == 1) mg else "", legend = FALSE,
           xlab = "", ylab = "", yaxt = "n", xaxt = "n",
           bty  = "n")
    
    axis(1, at = row_x[[ row_names[r] ]]$xticks, labels = row_x[[ row_names[r] ]]$xticks, cex.axis = 0.7, padj = -0.2, lwd = 0, lwd.ticks = 0.8)
    
    if (c == 1) {
      axis(2, at = yticks_row, labels = yticks_row, cex.axis = 0.8, lwd = 0, lwd.ticks = 0.6)
      par(xpd = NA)
      text(x = par("usr")[1] - 0.45 * diff(par("usr")[1:2]), y = mean(par("usr")[3:4]), labels = row_labels[[r]], srt = 90, adj = 0.5, cex = 1.0)
      par(xpd = FALSE)
      
    } else {
      axis(2, at = yticks_row, labels = FALSE, lwd = 0, lwd.ticks = 0.6)
    }
    
    box(bty = "o", lwd = 0.8)
  }
}
mtext("Effect size (lnRR)", side = 1, outer = TRUE, line = 1.3)
mtext("Standard error (SE)", side = 2, outer = TRUE, line = 3.0)

par(op)
dev.off()

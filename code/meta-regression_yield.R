
# Load libraries 
library(data.table)
library(metafor)
library(metagear)

d1 <- readxl::read_xlsx('C:/Users/asus/Desktop/paper1/meta-analysis/NEW_greenhouse.xlsx',sheet = 1)
d1 <- as.data.table(d1)
d2 <- d1[!is.na(yieldt_mean) & !is.na(yieldc_mean), ]

CV_yieldt_bar<-mean(d2$yieldt_sd[is.na(d2$yieldt_sd)==FALSE]/d2$yieldt_mean[is.na(d2$yieldt_sd)==FALSE])
d2$yieldt_sd[is.na(d2$yieldt_sd)==TRUE]<-d2$yieldt_mean[is.na(d2$yieldt_sd)==TRUE]*1.25*CV_yieldt_bar
CV_yieldc_bar<-mean(d2$yieldc_sd[is.na(d2$yieldc_sd)==FALSE]/d2$yieldc_mean[is.na(d2$yieldc_sd)==FALSE])
d2$yieldc_sd[is.na(d2$yieldc_sd)==TRUE]<-d2$yieldc_mean[is.na(d2$yieldc_sd)==TRUE]*1.25*CV_yieldc_bar
d2 <- as.data.table(d2)

setnames(d2,gsub('\\/','_',gsub(' |\\(|\\)','',colnames(d2))))
setnames(d2,tolower(colnames(d2)))

d2[is.na(n_dose), n_dose := median(d2$n_dose,na.rm=TRUE)]

cols_to_fill <- c("mat", "map", "ph", "soc", "clay")

d2[, (cols_to_fill) := lapply(.SD, function(x) as.numeric(as.character(x))), .SDcols = cols_to_fill]

for (col in cols_to_fill) {
  d2[is.na(get(col)), (col) := median(d2[[col]], na.rm = TRUE)]
}

es21 <- escalc(measure = "ROM", data = d2, 
               m1i = yieldt_mean, sd1i = yieldt_sd, n1i = replication,
               m2i = yieldc_mean, sd2i = yieldc_sd, n2i = replication )
d02 <- as.data.table(es21)

d02.treat <- data.table(treatment =  c('ALL',unique(d02$management)))

d02.treat[treatment=='ALL',desc := 'All']
d02.treat[treatment=='RES',desc := 'Residue retention']
d02.treat[treatment=='CC',desc := 'Crop Cover']
d02.treat[treatment=='ROT',desc := 'Crop rotation']
d02.treat[treatment=='RT',desc := 'Reduced tillage']
d02.treat[treatment=='NT',desc := 'No tillage']
d02.treat[treatment=='RI',desc := 'Row Intercropping']


out2 = out3 = list()

for(i in d02.treat$treatment){
  
  if(i=='ALL'){
    
    r_yield <- rma.mv(yi,vi, data=d02,random= list(~ 1|study_id), method="REML",sparse = TRUE)
    
  } else {
    
    r_yield <- rma.mv(yi,vi, data=d02[management==i,],random= list(~ 1|study_id), method="REML",sparse = TRUE)
    
  }
  
  out2[[i]] <- data.table(mean = as.numeric((exp(r_yield$b)-1)*100),
                          se = as.numeric((exp(r_yield$se)-1)*100), 
                          pval = round(as.numeric(r_yield$pval),4),
                          label =  paste0(d02.treat[treatment==i,desc],' (n=',r_yield$k,')')
  )
}

out2 <- rbindlist(out2)

# Meta-regression for main factors

d02[,clay_scaled := scale(clay)]
d02[,soc_scaled := scale(soc)]
d02[,ph_scaled := scale(ph)]
d02[,mat_scaled := scale(mat)]
d02[,map_scaled := scale(map)]
d02[,n_dose_scaled := scale(n_dose)]

d02[,ri := fifelse(management=='RI','yes','no')]
d02[,res := fifelse(management=='RES','yes','no')]
d02[,cc := fifelse(management=='CC','yes','no')]
d02[,rot := fifelse(management=='ROT','yes','no')]
d02[,nt := fifelse(management=='NT','yes','no')] 
d02[,rt := fifelse(management=='RT','yes','no')]

var.site <- c('mat_scaled','map_scaled','clay_scaled','soc_scaled','ph_scaled')
var.crop <- c('crop_type','n_dose_scaled')
var.trea <- c('ri', 'res', 'cc','rot','nt','rt')  

var.sel <- c(var.trea,var.crop,var.site)
r_yield_0 <- rma.mv(yi,vi, data = d02,random= list(~ 1|study_id), method="REML",sparse = TRUE)

out1.est = out1.sum = list()

for(i in var.sel){
  
  vartype = is.character(d02[,get(i)])
  
  if(vartype == TRUE){
    
    r_yield_1 <- rma.mv(yi,vi, 
                        mods = ~factor(varsel) -1, 
                        data = d02[,.(yi,vi,study_id,varsel = get(i))],
                        random = list(~ 1|study_id), method="REML",sparse = TRUE)
    
  } else {
    
    r_yield_1 <- rma.mv(yi,vi, 
                        mods = ~varsel, 
                        data = d02[,.(yi,vi,study_id,varsel = get(i))],
                        random = list(~ 1|study_id), method="REML",sparse = TRUE)
  }
  
  out1.est[[i]] <- data.table(var = i,
                              varname = gsub('factor\\(varsel\\)','',rownames(r_yield_1$b)),
                              mean = round(as.numeric(r_yield_1$b),3),
                              se = round(as.numeric(r_yield_1$se),3),
                              ci.lb = round(as.numeric(r_yield_1$ci.lb),3),
                              ci.ub = round(as.numeric(r_yield_1$ci.ub),3),
                              pval = round(as.numeric(r_yield_1$pval),3))
  
  out1.sum[[i]] <- data.table(var = i,
                              AIC = r_yield_1$fit.stats[4,2],
                              ll = r_yield_1$fit.stats[1,2],
                              ll_impr = round(100 * (1-r_yield_1$fit.stats[1,2]/r_yield_0$fit.stats[1,2]),2),
                              r2_impr = round(100*max(0,(sum(r_yield_0$sigma2)-sum(r_yield_1$sigma2))/sum(r_yield_0$sigma2)),2),
                              pval = round(anova(r_yield_1,r_yield_0)$pval,3)
  )
}

out1.sum <- rbindlist(out1.sum)
out1.est <- rbindlist(out1.est)

print(out1.sum)
print(out1.est)

fwrite(out1.est, "C:/Users/asus/Desktop/paper1/meta-regression/meta_regression_yield_estimates.csv")
fwrite(out1.sum, "C:/Users/asus/Desktop/paper1//meta-regression/meta_regression_yield_summary.csv")

# Meta-regression for main factors with interactions

estats <- function(model_new,model_base){
  out <- data.table(AIC = model_new$fit.stats[4,2],
                    ll = model_new$fit.stats[1,2],
                    ll_impr = round(100 * (1-model_new$fit.stats[1,2]/model_base$fit.stats[1,2]),2),
                    r2_impr = round(100*max(0,(sum(model_base$sigma2)-sum(model_new$sigma2))/sum(model_base$sigma2)),2),
                    pval = round(anova(r_yield_1,r_yield_0)$pval,3))
  return(out)
}

r_yield_0 <- rma.mv(yi,vi, data = d02,random= list(~ 1|study_id), method="REML",sparse = TRUE)


r_yield_final <- rma.mv(yi,vi,
                        mods = ~ nt +rt +rot + ri +
                          n_dose_scaled + crop_type + clay_scaled + soc_scaled + ph_scaled + mat_scaled + map_scaled +
                          ri:n_dose_scaled +
                          rt:soc_scaled +rt:ph_scaled +
                          nt:mat_scaled +nt:ph_scaled +
                          rot:soc_scaled -1,      
                        data = d02,
                        random = list(~ 1|study_id), method="REML",sparse = TRUE)

out = estats(model_new = r_yield_final,model_base = r_yield_0)
print(paste0('model improved the log likelyhood with ',round(out$ll_impr,1),'%'))
summary(r_yield_final)

summary_output <- summary(r_yield_final)

beta_table <- data.frame(
  Estimate = summary_output$beta[,1],
  SE = summary_output$se,
  Zval = summary_output$zval,
  Pval = summary_output$pval,
  CI.lb = summary_output$ci.lb,
  CI.ub = summary_output$ci.ub
)

beta_table$Variable <- rownames(summary_output$beta)

beta_table <- beta_table[, c("Variable", "Estimate", "SE", "Zval", "Pval", "CI.lb", "CI.ub")]

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

QE <- summary_output$QE %||% r_yield_final$QE

df_QE <- summary_output$df.QE %||% (r_yield_final$k - r_yield_final$p)

QEp <- summary_output$QEp %||% pchisq(QE, df = df_QE, lower.tail = FALSE)

I2 <- summary_output$I2 %||% NA_real_

extra_stats <- data.frame(
  Variable = c("QE", "df", "pval.QE", "I2"),
  Estimate = c(QE, df_QE, QEp, I2),
  SE = NA, Zval = NA, Pval = NA, CI.lb = NA, CI.ub = NA
)

export_table <- rbind(beta_table, extra_stats)

write.csv(export_table, "C:/Users/asus/Desktop/paper1/meta-regression/r_yield_try_summary_full.csv", row.names = FALSE)

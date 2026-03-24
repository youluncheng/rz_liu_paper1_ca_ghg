
# Meta-analysis
library(tidyverse)
library(sf)
library(data.table)
library(showtext)
library(metafor)
library(ggplot2)
library(ggpubr)
library(readxl)
library(cowplot)
library(patchwork)

#co2
d1 <- readxl::read_xlsx('C:/Users/asus/Desktop/paper1/meta-analysis/greenhouse.xlsx',sheet = 4)
d1 <- as.data.table(d1)
d2 <- d1[!is.na(co2t_mean) & !is.na(co2c_mean), ]

CV_co2t_bar<-mean(d2$co2t_sd[is.na(d2$co2t_sd)==FALSE]/d2$co2t_mean[is.na(d2$co2t_sd)==FALSE])
d2$co2t_sd[is.na(d2$co2t_sd)==TRUE]<-d2$co2t_mean[is.na(d2$co2t_sd)==TRUE]*1.25*CV_co2t_bar
CV_co2c_bar<-mean(d2$co2c_sd[is.na(d2$co2c_sd)==FALSE]/d2$co2c_mean[is.na(d2$co2c_sd)==FALSE])
d2$co2c_sd[is.na(d2$co2c_sd)==TRUE]<-d2$co2c_mean[is.na(d2$co2c_sd)==TRUE]*1.25*CV_co2c_bar

d2 <- as.data.table(d2)
setnames(d2,gsub('\\/','_',gsub(' |\\(|\\)','',colnames(d2))))
setnames(d2,tolower(colnames(d2)))

d2[is.na(n_dose), n_dose := median(d2$n_dose,na.rm=TRUE)]

es21 <- escalc(measure = "ROM", data = d2,
               m1i = co2t_mean, sd1i = co2t_sd, n1i = replication,
               m2i = co2c_mean, sd2i = co2c_sd, n2i = replication )

d02 <- as.data.table(es21)
d02.treat <- data.table(treatment =  c('ALL',unique(d02$management)))
d02.treat[treatment=='ALL',desc := 'Grand mean']
d02.treat[treatment=='RES',desc := 'Residue retention']
d02.treat[treatment=='CC',desc := 'Cover cropping']
d02.treat[treatment=='ROT',desc := 'Crop rotation']
d02.treat[treatment=='RT',desc := 'Reduced tillage']
d02.treat[treatment=='NT',desc := 'No tillage']
d02.treat[treatment=='RI',desc := 'Row intercropping']

out2 = list()

for(i in d02.treat$treatment){
  
  if(i=='ALL'){
    
    r_co2 <- rma.mv(yi,vi, data=d02,random= list(~ 1|study_id), method="REML",sparse = TRUE)
    
  } else {
    
    r_co2 <- rma.mv(yi,vi, data=d02[management==i,],random= list(~ 1|study_id), method="REML",sparse = TRUE)
    
  }
  
  out2[[i]] <- data.table(Management = d02.treat[treatment==i,desc],
                          mean = as.numeric((exp(r_co2$b)-1)*100),
                          se = as.numeric((exp(r_co2$se)-1)*100),
                          ci.lb = as.numeric((exp(r_co2$ci.lb)-1)*100),
                          ci.ub = as.numeric((exp(r_co2$ci.ub)-1)*100),
                          pval = round(as.numeric(r_co2$pval),4),
                          n = r_co2$k,
                          label =  paste0(d02.treat[treatment==i,desc],' (n=',r_co2$k,')'))
}

out2 <- rbindlist(out2)

write.csv(out2, file="C:/Users/asus/Desktop/paper1/meta-analysis/co2_data.csv")


#CH4
d1 <- readxl::read_xlsx('C:/Users/asus/Desktop/paper1/meta-analysis/greenhouse.xlsx',sheet = 4)
d1 <- as.data.table(d1)
d2 <- d1[!is.na(ch4t_mean) & !is.na(ch4c_mean), ]

CV_ch4t_bar<-mean(d2$ch4t_sd[is.na(d2$ch4t_sd)==FALSE]/d2$ch4t_mean[is.na(d2$ch4t_sd)==FALSE])
d2$ch4t_sd[is.na(d2$ch4t_sd)==TRUE]<-d2$ch4t_mean[is.na(d2$ch4t_sd)==TRUE]*1.25*CV_ch4t_bar
CV_ch4c_bar<-mean(d2$ch4c_sd[is.na(d2$ch4c_sd)==FALSE]/d2$ch4c_mean[is.na(d2$ch4c_sd)==FALSE])
d2$ch4c_sd[is.na(d2$ch4c_sd)==TRUE]<-d2$ch4c_mean[is.na(d2$ch4c_sd)==TRUE]*1.25*CV_ch4c_bar

d2 <- as.data.table(d2)
setnames(d2,gsub('\\/','_',gsub(' |\\(|\\)','',colnames(d2))))
setnames(d2,tolower(colnames(d2)))

d2[is.na(n_dose), n_dose := median(d2$n_dose,na.rm=TRUE)]

es21 <- escalc(measure = "ROM", data = d2,
               m1i = ch4t_mean, sd1i = ch4t_sd, n1i = replication,
               m2i = ch4c_mean, sd2i = ch4c_sd, n2i = replication )

d02 <- as.data.table(es21)
d02.treat <- data.table(treatment =  c('ALL',unique(d02$management)))
d02.treat[treatment=='ALL',desc := 'Grand mean']
d02.treat[treatment=='RES',desc := 'Residue retention']
d02.treat[treatment=='CC',desc := 'Cover cropping']
d02.treat[treatment=='ROT',desc := 'Crop rotation']
d02.treat[treatment=='RT',desc := 'Reduced tillage']
d02.treat[treatment=='NT',desc := 'No tillage']
d02.treat[treatment=='RI',desc := 'Row intercropping']

# a list to store the coefficients
out2 = list()

for(i in d02.treat$treatment){
  
  if(i=='ALL'){
    
    r_ch4 <- rma.mv(yi,vi, data=d02,random= list(~ 1|study_id), method="REML",sparse = TRUE)
    
  } else {
    
    r_ch4 <- rma.mv(yi,vi, data=d02[management==i,],random= list(~ 1|study_id), method="REML",sparse = TRUE)
    
  }
  
  out2[[i]] <- data.table(Management = d02.treat[treatment==i,desc],
                          mean = as.numeric((exp(r_ch4$b)-1)*100),
                          se = as.numeric((exp(r_ch4$se)-1)*100),
                          ci.lb = as.numeric((exp(r_ch4$ci.lb)-1)*100),
                          ci.ub = as.numeric((exp(r_ch4$ci.ub)-1)*100),
                          pval = round(as.numeric(r_ch4$pval),4),
                          n = r_ch4$k,
                          label =  paste0(d02.treat[treatment==i,desc],' (n=',r_ch4$k,')'))
}

# convert lists to vector
out2 <- rbindlist(out2)

write.csv(out2, file="C:/Users/asus/Desktop/paper1/meta-analysis/ch4_data.csv")


#n2o
d1 <- readxl::read_xlsx('C:/Users/asus/Desktop/paper1/meta-analysis/greenhouse.xlsx',sheet = 4)
d1 <- as.data.table(d1)
d2 <- d1[!is.na(n2ot_mean) & !is.na(n2oc_mean), ]

CV_n2ot_bar<-mean(d2$n2ot_sd[is.na(d2$n2ot_sd)==FALSE]/d2$n2ot_mean[is.na(d2$n2ot_sd)==FALSE])
d2$n2ot_sd[is.na(d2$n2ot_sd)==TRUE]<-d2$n2ot_mean[is.na(d2$n2ot_sd)==TRUE]*1.25*CV_n2ot_bar
CV_n2oc_bar<-mean(d2$n2oc_sd[is.na(d2$n2oc_sd)==FALSE]/d2$n2oc_mean[is.na(d2$n2oc_sd)==FALSE])
d2$n2oc_sd[is.na(d2$n2oc_sd)==TRUE]<-d2$n2oc_mean[is.na(d2$n2oc_sd)==TRUE]*1.25*CV_n2oc_bar

d2 <- as.data.table(d2)
setnames(d2,gsub('\\/','_',gsub(' |\\(|\\)','',colnames(d2))))
setnames(d2,tolower(colnames(d2)))

d2[is.na(n_dose), n_dose := median(d2$n_dose,na.rm=TRUE)]

es21 <- escalc(measure = "ROM", data = d2,
               m1i = n2ot_mean, sd1i = n2ot_sd, n1i = replication,
               m2i = n2oc_mean, sd2i = n2oc_sd, n2i = replication )

d02 <- as.data.table(es21)
d02.treat <- data.table(treatment =  c('ALL',unique(d02$management)))
d02.treat[treatment=='ALL',desc := 'Grand mean']
d02.treat[treatment=='RES',desc := 'Residue retention']
d02.treat[treatment=='CC',desc := 'Cover cropping']
d02.treat[treatment=='ROT',desc := 'Crop rotation']
d02.treat[treatment=='RT',desc := 'Reduced tillage']
d02.treat[treatment=='NT',desc := 'No tillage']
d02.treat[treatment=='RI',desc := 'Row intercropping']

out2 = list()

for(i in d02.treat$treatment){
  
  if(i=='ALL'){
    
    r_n2o <- rma.mv(yi,vi, data=d02,random= list(~ 1|study_id), method="REML",sparse = TRUE)
    
  } else {
    
    r_n2o <- rma.mv(yi,vi, data=d02[management==i,],random= list(~ 1|study_id), method="REML",sparse = TRUE)
    
  }
  
  out2[[i]] <- data.table(Management = d02.treat[treatment==i,desc],
                          mean = as.numeric((exp(r_n2o$b)-1)*100),
                          se = as.numeric((exp(r_n2o$se)-1)*100),
                          ci.lb = as.numeric((exp(r_n2o$ci.lb)-1)*100),
                          ci.ub = as.numeric((exp(r_n2o$ci.ub)-1)*100),
                          pval = round(as.numeric(r_n2o$pval),4),
                          n = r_n2o$k,
                          label =  paste0(d02.treat[treatment==i,desc],' (n=',r_n2o$k,')'))
}

out2 <- rbindlist(out2)

write.csv(out2, file="C:/Users/asus/Desktop/paper1/meta-analysis/n2o_data.csv")


#yield
d1 <- readxl::read_xlsx('C:/Users/asus/Desktop/paper1/meta-analysis/greenhouse.xlsx',sheet = 4)
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

es21 <- escalc(measure = "ROM", data = d2,
               m1i = yieldt_mean, sd1i = yieldt_sd, n1i = replication,
               m2i = yieldc_mean, sd2i = yieldc_sd, n2i = replication )

d02 <- as.data.table(es21)
d02.treat <- data.table(treatment =  c('ALL',unique(d02$management)))
d02.treat[treatment=='ALL',desc := 'Grand mean']
d02.treat[treatment=='RES',desc := 'Residue retention']
d02.treat[treatment=='CC',desc := 'Cover cropping']
d02.treat[treatment=='ROT',desc := 'Crop rotation']
d02.treat[treatment=='RT',desc := 'Reduced tillage']
d02.treat[treatment=='NT',desc := 'No tillage']
d02.treat[treatment=='RI',desc := 'Row intercropping']

out2 = list()

for(i in d02.treat$treatment){
  
  if(i=='ALL'){
    
    r_yield <- rma.mv(yi,vi, data=d02,random= list(~ 1|study_id), method="REML",sparse = TRUE)
    
  } else {
    
    r_yield <- rma.mv(yi,vi, data=d02[management==i,],random= list(~ 1|study_id), method="REML",sparse = TRUE)
    
  }
  
  out2[[i]] <- data.table(Management = d02.treat[treatment==i,desc],
                          mean = as.numeric((exp(r_yield$b)-1)*100),
                          se = as.numeric((exp(r_yield$se)-1)*100),
                          ci.lb = as.numeric((exp(r_yield$ci.lb)-1)*100),
                          ci.ub = as.numeric((exp(r_yield$ci.ub)-1)*100),
                          pval = round(as.numeric(r_yield$pval),4),
                          n = r_yield$k,
                          label =  paste0(d02.treat[treatment==i,desc],' (n=',r_yield$k,')'))
}

out2 <- rbindlist(out2)

write.csv(out2, file="C:/Users/asus/Desktop/paper1/meta-analysis/yield_data.csv")

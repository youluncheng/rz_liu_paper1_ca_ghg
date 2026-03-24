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

font_add("Arial", regular = "C:/Windows/Fonts/arial.ttf", bold    = "C:/Windows/Fonts/arialbd.ttf")
showtext_auto()
#map
site <- read_xlsx('C:/Users/asus/Desktop/paper1/meta-analysis/greenhouse.xlsx', sheet = 4)
site <- as.data.table(site)

site[management == 'CC', management := 'Cover cropping']
site[management == 'RT', management := 'Reduced tillage']
site[management == 'NT', management := 'No tillage'] 
site[management == 'RI', management := 'Row intercropping']
site[management == 'RES', management := 'Residue retention']
site[management == 'ROT', management := 'Crop rotation'] 

names(site)[names(site) == "management"] <- "Management"
site$Management <- factor(site$Management, 
                          levels = c('Cover cropping', 'Reduced tillage', 'No tillage', 'Row intercropping', 'Residue retention', 'Crop rotation'))

site[, lon := as.numeric(lon)]
site[, lat := as.numeric(lat)]

sites_sf <- st_as_sf(site, coords = c("lon", "lat"), crs = 4326)

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

proj_robin <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

world_no_antarctica <- world %>%
  filter(name_long != "Antarctica")

world_robin <- world_no_antarctica %>%
  st_transform(crs = proj_robin)
sites_robin <- sites_sf %>%
  st_transform(crs = proj_robin)

bbox_sites <- st_bbox(sites_robin)

bbox_world <- st_bbox(world_robin)

xmin <- min(bbox_sites["xmin"], bbox_world["xmin"])
ymin <- min(bbox_sites["ymin"], bbox_world["ymin"])
xmax <- max(bbox_sites["xmax"], bbox_world["xmax"])
ymax <- max(bbox_sites["ymax"], bbox_world["ymax"])

expand_ratio <- 0.01  
xrange <- xmax - xmin
yrange <- ymax - ymin
xmin <- xmin - xrange * expand_ratio
ymin <- ymin - yrange * expand_ratio
xmax <- xmax + xrange * expand_ratio
ymax <- ymax + yrange * expand_ratio

map <- ggplot() +
  geom_sf(data = world_robin, fill = "#BDE5D0", color = "#C3E7D4", size = 0.2) +
  geom_sf(data = sites_robin, aes(color = Management, shape = crop_type), size = 1.2, alpha = 0.8) +
  scale_color_manual(
    values = c("Cover cropping" = "#4DAF4A", "Reduced tillage" = "#F33737", "No tillage" = "#A749C7", "Row intercropping" = "#1656F8", "Residue retention" = "#E7298A", "Crop rotation" = "#E58C0F"),
    guide = guide_legend(nrow = 3, ncol = 2, override.aes = list(size = 2), direction = "vertical", title.position = "top", keyheight = unit(0.35, "cm"), keywidth = unit(0.45, "cm"))) +
  scale_shape_manual(values = c("wheat" = 17, "rice" = 15, "maize" = 16),
                     guide = guide_legend(title = "Crop Type", override.aes = list(size = 2), keyheight = unit(0.35, "cm"), keywidth = unit(0.45, "cm"))) +
  
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_blank(), 
        legend.position = c(0.15, 0.35), 
        legend.direction = "vertical", 
        legend.text = element_text(size = 18, color = "black", family = "Arial"),
        legend.title = element_text(face = "bold", size = 18, color = "black", family = "Arial"),
        legend.background = element_blank(),
        legend.box = "vertical",
        legend.spacing.x = unit(0.1, "cm"),
        legend.spacing.y = unit(-0.2, "cm"), 
        legend.box.just = "left",
        legend.justification = "center",
        legend.key = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"))

# Meta-analysis
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

metaresult_group <- out2
p1 <- ggplot(data = metaresult_group, aes(y = Management, x = mean)) +
  annotate("segment", x = -Inf, xend = Inf, y = 7.5, yend = 7.5, color = "black", size = 0.2, linetype = "solid") +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 7.5, ymax = Inf, fill = "#a9a9a9") +
  annotate("text", x = mean(c(-10,25)), y = 7.8, label = "Carbon dioxide emissions", size = 7, fontface = "plain", color = "black", family = "Arial") +
  annotate("rect", xmin = -Inf, ymin = 6.5, xmax = Inf, ymax = 7.5, fill = "#FCFFFB") +
  annotate("rect", xmin = -Inf, ymin = 4.5, xmax = Inf, ymax = 6.5, fill = "#F3FBF2") +
  annotate("rect", xmin = -Inf, ymin = 2.5, xmax = Inf, ymax = 4.5, fill = "#F3FBF2") +
  annotate("rect", xmin = -Inf, ymin = -Inf, xmax = Inf, ymax = 2.5, fill = "#F3FBF2") +
  annotate("segment", x = 0, xend = 0, y = 0.5, yend = 7.5, linetype = "longdash", size = 0.1, color = "black") +
  geom_errorbar(position = position_dodge(0.7), aes(xmin = ci.lb, xmax = ci.ub), width = 0.1, size = 0.3) +
  geom_point(position = position_dodge(0.7), size = 2.5, stroke = 0.2, shape = 23, fill = "#FF5733") +
  geom_text(aes(x = ci.ub, label =paste0("(", n, ")")), position = position_dodge(0.7), vjust = 0.5, hjust = -0.1, size = 6, color = "black", fontface = "plain", family = "Arial", check_overlap = FALSE) +
  
  scale_y_discrete(limits = rev(c("Grand mean", "Crop rotation", "Row intercropping", "Reduced tillage", "No tillage", "Cover cropping", "Residue retention")),
                   labels = rev(c("Grand mean", "Crop rotation", "Row intercropping", "Reduced tillage", "No tillage", "Cover cropping", "Residue retention"))) +
  labs(y = NULL, x = NULL) +
  coord_cartesian(xlim = c(-30, 40), ylim = c(1, 7.5),clip = "off") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.1),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 19, color = "black"),
        axis.ticks.x = element_line(color = "black", size = 0.1),
        axis.ticks.length.x = unit(0.1, "cm"),
        plot.margin = unit(c(1, 1, 1, 1), "cm"))

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

out2 <- rbindlist(out2)

write.csv(out2, file="C:/Users/asus/Desktop/paper1/meta-analysis/ch4_data.csv")

metaresult_group <- out2
p2 <- ggplot(data = metaresult_group, aes(y = Management, x = mean)) +
  annotate("segment", x = -Inf, xend = Inf, y = 7.5, yend = 7.5, color = "black", size = 0.2, linetype = "solid") +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 7.5, ymax = Inf, fill = "#a9a9a9") +
  annotate("text", x = mean(c(-50,130)), y = 7.8, label = "Methane emission",size = 7, fontface = "plain", color = "black", family = "Arial") +
  annotate("rect", xmin = -Inf, ymin = 6.5, xmax = Inf, ymax = 7.5, fill = "#FCFFFB") +
  annotate("rect", xmin = -Inf, ymin = 4.5, xmax = Inf, ymax = 6.5, fill = "#F3FBF2") +
  annotate("rect", xmin = -Inf, ymin = 2.5, xmax = Inf, ymax = 4.5, fill = "#F3FBF2") +
  annotate("rect", xmin = -Inf, ymin = -Inf, xmax = Inf, ymax = 2.5, fill = "#F3FBF2") +
  annotate("segment", x = 0, xend = 0, y = 0.5, yend = 7.5,
           linetype = "longdash", size = 0.1, color = "black") +
  geom_errorbar(position = position_dodge(0.7), aes(xmin = ci.lb, xmax = ci.ub), width = 0.1, size = 0.3) +
  geom_point(position = position_dodge(0.7), size = 2.5, stroke = 0.2, shape = 23, fill = "#FF5733") +
  geom_text(aes(x = ci.ub, label =paste0("(", n, ")")), position = position_dodge(0.7), vjust = 0.5, hjust = -0.1, size = 6, color = "black", fontface = "plain", family = "Arial", check_overlap = FALSE) +
  scale_y_discrete(limits = rev(c("Grand mean", "Crop rotation", "Row intercropping", "Reduced tillage", "No tillage", "Cover cropping", "Residue retention")),
                   labels = NULL) +
  labs(y = NULL, x = NULL) +
  coord_cartesian(xlim = c(-45, 125), ylim = c(1, 7.5),clip = "off") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.1),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 19, color = "black"),
        axis.ticks.x = element_line(color = "black", size = 0.1),
        axis.ticks.length.x = unit(0.1, "cm"),
        plot.margin = unit(c(1, 1, 1, 1), "cm"))

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

metaresult_group <- out2
p3 <- ggplot(data = metaresult_group, aes(y = Management, x = mean)) +
  annotate("segment", x = -Inf, xend = Inf, y = 7.5, yend = 7.5, color = "black", size = 0.2, linetype = "solid") +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 7.5, ymax = Inf, fill = "#a9a9a9") +
  annotate("text", x = mean(c(-35,40)), y = 7.8, label = "Nitrous oxide emissions", size = 7, fontface = "plain", color = "black", family = "Arial") +
  annotate("rect", xmin = -Inf, ymin = 6.5, xmax = Inf, ymax = 7.5, fill = "#FCFFFB") +
  annotate("rect", xmin = -Inf, ymin = 4.5, xmax = Inf, ymax = 6.5, fill = "#F3FBF2") +
  annotate("rect", xmin = -Inf, ymin = 2.5, xmax = Inf, ymax = 4.5, fill = "#F3FBF2") +
  annotate("rect", xmin = -Inf, ymin = -Inf, xmax = Inf, ymax = 2.5, fill = "#F3FBF2") +
  annotate("segment", x = 0, xend = 0, y = 0.5, yend = 7.5, linetype = "longdash", size = 0.1, color = "black") +
  geom_errorbar(position = position_dodge(0.7), aes(xmin = ci.lb, xmax = ci.ub), width = 0.1, size = 0.3) +
  geom_point(position = position_dodge(0.7), size = 2.5, stroke = 0.2, shape = 23, fill = "#FF5733") +
  geom_text(aes(x = ci.ub, label =paste0("(", n, ")")), position = position_dodge(0.7), vjust = 0.5, hjust = -0.1, size = 6, color = "black", fontface = "plain", check_overlap = FALSE) +
  scale_y_discrete(limits = rev(c("Grand mean", "Crop rotation", "Row intercropping", "Reduced tillage", "No tillage", "Cover cropping", "Residue retention")),
                   labels = NULL) +
  labs(y = NULL, x = NULL) +
  coord_cartesian(xlim = c(-35, 40), ylim = c(1, 7.5),clip = "off") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.1),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 19, color = "black"),
        axis.ticks.x = element_line(color = "black", size = 0.1),
        axis.ticks.length.x = unit(0.1, "cm"),
        plot.margin = unit(c(1, 1, 1, 1), "cm"))

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

metaresult_group <- out2
p4 <- ggplot(data = metaresult_group, aes(y = Management, x = mean)) +
  annotate("segment", x = -Inf, xend = Inf, y = 7.5, yend = 7.5, color = "black", size = 0.2, linetype = "solid") +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 7.5, ymax = Inf, fill = "#a9a9a9") +
  annotate("text", x = mean(c(-10,25)), y = 7.8, label = "Yield", size = 7, fontface = "plain", color = "black", family = "Arial") +
  annotate("rect", xmin = -Inf, ymin = 6.5, xmax = Inf, ymax = 7.5, fill = "#FCFFFB") +
  annotate("rect", xmin = -Inf, ymin = 4.5, xmax = Inf, ymax = 6.5, fill = "#F3FBF2") +
  annotate("rect", xmin = -Inf, ymin = 2.5, xmax = Inf, ymax = 4.5, fill = "#F3FBF2") +
  annotate("rect", xmin = -Inf, ymin = -Inf, xmax = Inf, ymax = 2.5, fill = "#F3FBF2") +
  annotate("segment", x = 0, xend = 0, y = 0.5, yend = 7.5,
           linetype = "longdash", size = 0.1, color = "black") +
  geom_errorbar(position = position_dodge(0.7), aes(xmin = ci.lb, xmax = ci.ub), width = 0.1, size = 0.3) +
  geom_point(position = position_dodge(0.7), size = 2.5, stroke = 0.2, shape = 23, fill = "#FF5733") +
  geom_text(aes(x = ci.ub, label =paste0("(", n, ")")), position = position_dodge(0.7), vjust = 0.5, hjust = -0.1, size = 6, color = "black", fontface = "plain", family = "Arial", check_overlap = FALSE) +
  scale_y_discrete(limits = rev(c("Grand mean", "Crop rotation", "Row intercropping", "Reduced tillage", "No tillage", "Cover cropping", "Residue retention")), labels = NULL) +
  labs(y = NULL, x = NULL) +
  coord_cartesian(xlim = c(-10, 25), ylim = c(1, 7.5),clip = "off") + 
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.1),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 19, color = "black"),
        axis.ticks.x = element_line(color = "black", size = 0.1),
        axis.ticks.length.x = unit(0.1, "cm"),
        plot.margin = unit(c(1, 1, 1, 1), "cm"))


#combined figures
full_top_to_bottom <- c("Grand mean", "Crop rotation", "Row intercropping",
                        "Reduced tillage", "No tillage", "Cover cropping", "Residue retention")
abbr_top_to_bottom <- c("GM", "ROT", "RI", "RT", "NT", "CC", "RES")

full_levels <- rev(full_top_to_bottom)
abbr_levels <- rev(abbr_top_to_bottom)

lab_df <- data.frame(
  Management = factor(full_levels, levels = full_levels),
  abbr = abbr_levels
)

p_lab <- ggplot(lab_df, aes(y = Management, x = 1.2, label = abbr)) +
  geom_text(hjust = 1, size = 6, family = "Arial", nudge_y = 1.0) + 
  scale_y_discrete(limits = full_levels) +
  coord_cartesian(xlim = c(0, 1), ylim = c(1, 8.8), clip = "off") + 
  theme_void() +
  theme(plot.margin = ggplot2::margin(t = 4, r = 6, b = 5.5, l = 0, unit = "pt"))

same_margin <- ggplot2::margin(t = 5.5, r = 3, b = -5, l = 3, unit = "pt")
p1a <- p1 + theme(plot.margin = same_margin)
p2a <- p2 + theme(plot.margin = same_margin)
p3a <- p3 + theme(plot.margin = same_margin)
p4a <- p4 + theme(plot.margin = same_margin)

aligned  <- cowplot::align_plots(p1a, p2a, p3a, p4a, align = "hv", axis = "tblr")
p_effect <- cowplot::plot_grid(plotlist = aligned, nrow = 1, rel_widths = rep(1, 4))
B_body <- cowplot::plot_grid(p_lab, p_effect, nrow = 1, rel_widths = c(0.1, 2), align = "h", axis = "tb")
final_plot <- ggpubr::annotate_figure(B_body, 
                                      left = ggpubr::text_grob("Management practices", rot = 90, size = 28, face = "plain", vjust = 0.5, hjust = 0.38),
                                      bottom = ggpubr::text_grob("Effect size change (%)", size = 28, face = "plain",vjust = 0.1, family = "Arial"))

print(final_plot)

combined_final <- (map / final_plot) +
  plot_layout(heights = c(12, 10)) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag.position = c(0.02, 0.98),
        plot.tag = element_text(size = 36, face = "bold", family = "Arial"))

print(combined_final)

ggsave("C:/Users/asus/Desktop/paper1/Figure_1.tiff",
       plot = combined_final,
       width = 183, height = 150, units = "mm",
       device = "tiff",
       dpi = 300,
       compression = "lzw")


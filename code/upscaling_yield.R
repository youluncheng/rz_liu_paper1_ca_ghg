# load packages
require(sf);require(terra);require(data.table);require(metafor)
library(sf);library(terra);library(data.table);library(metafor)

rm(list=ls())

# load all current practices and site properties
r.soil <- terra::rast('C:/Users/asus/Desktop/paper1/upscaling/data_for_upscaling/soil.tif')
r.clim <- terra::rast('C:/Users/asus/Desktop/paper1/upscaling/data_for_upscaling/climate.tif')
r.agtech <- terra::rast('C:/Users/asus/Desktop/paper1/upscaling/data_for_upscaling/r.agtech.tif')
r.nman <- terra::rast('C:/Users/asus/Desktop/paper1/upscaling/data_for_upscaling/nofert.tif')
r.nfert <- terra::rast('C:/Users/asus/Desktop/paper1/upscaling/data_for_upscaling/nifert.tif')
r.cwheat <- terra::rast('C:/Users/asus/Desktop/paper1/upscaling/data_for_upscaling/r.cwheat.tif')
r.cmaize <- terra::rast('C:/Users/asus/Desktop/paper1/upscaling/data_for_upscaling/r.cmaize.tif')
r.crice <- terra::rast('C:/Users/asus/Desktop/paper1/upscaling/data_for_upscaling/r.crice.tif')
r.notill <- terra::rast('C:/Users/asus/Desktop/paper1/upscaling/data_for_upscaling/r.notill.tif')
r.cropint <- terra::rast('C:/Users/asus/Desktop/paper1/upscaling/data_for_upscaling/r.cropint.tif')
r.dt <- as.data.table(as.data.frame(r.cropint,xy=TRUE, na.rm = FALSE))
r.ccrop <- terra::rast('C:/Users/asus/Desktop/paper1/upscaling/data_for_upscaling/r.ccrop.tif')
r.residue <- terra::rast('C:/Users/asus/Desktop/paper1/upscaling/data_for_upscaling/r.residue.tif')
r.all <- c(r.soil,r.clim,r.agtech, r.nman, r.nfert,r.cwheat,r.cmaize,r.crice,r.notill,r.cropint,r.ccrop,r.residue)
r.dt <- as.data.table(as.data.frame(r.all,xy=TRUE, na.rm = FALSE))

setnames(r.dt,
         old = c('climate_mat', 'climate_pre','isric_phw_mean_0_5','isric_clay_mean_0_5','isric_soc_mean_0_5','nfert_nh4','nfert_no3','nofert'),
         new = c('mat','pre','phw','clay','soc','nh4','no3','nam'),skip_absent = T)

r.dt <- r.dt[!(is.na(mat)|is.na(pre))]
r.dt <- r.dt[!(is.na(cwheat) & is.na(cmaize) & is.na(crice))]
r.dt[is.na(NOTILL), NOTILL := 0]
r.dt[is.na(cropintensity), cropintensity := 0]
r.dt[is.na(CCROP), CCROP := 0]
r.dt[is.na(RES), RES := 0]
r.dt[is.na(nam), nam := 0]
r.dt[is.na(nh4), nh4 := 0]
r.dt[is.na(no3), no3 := 0]

r.dt[,c('CROPINT','TECH_LOW') := NULL]

# read data  
d1 <- readxl::read_xlsx('C:/Users/asus/Desktop/paper1/meta-analysis/greenhouse.xlsx',sheet = 4)
d1 <- as.data.table(d1)
d2 <- d1[!is.na(yieldt_mean) & !is.na(yieldc_mean), ]

CV_yieldt_bar<-mean(d2$yieldt_sd[is.na(d2$yieldt_sd)==FALSE]/d2$yieldt_mean[is.na(d2$yieldt_sd)==FALSE])
d2$yieldt_sd[is.na(d2$yieldt_sd)==TRUE]<-d2$yieldt_mean[is.na(d2$yieldt_sd)==TRUE]*1.25*CV_yieldt_bar
CV_yieldc_bar<-mean(d2$yieldc_sd[is.na(d2$yieldc_sd)==FALSE]/d2$yieldc_mean[is.na(d2$yieldc_sd)==FALSE])
d2$yieldc_sd[is.na(d2$yieldc_sd)==TRUE]<-d2$yieldc_mean[is.na(d2$yieldc_sd)==TRUE]*1.25*CV_yieldc_bar

setnames(d2,gsub('\\/','_',gsub(' |\\(|\\)','',colnames(d2))))
setnames(d2,tolower(colnames(d2)))

es21 <- escalc(measure = "ROM", data = d2,
               m1i = yieldt_mean, sd1i = yieldt_sd, n1i = replication,
               m2i = yieldc_mean, sd2i = yieldc_sd, n2i = replication )

d02 <- as.data.table(es21)

d02.treat <- data.table(treatment =  c('ALL',unique(d02$management)))

d02.treat[treatment=='ALL',desc := 'All']
d02.treat[treatment=='RES',desc := 'Residue retention']
d02.treat[treatment=='ROT',desc := 'Crop rotation']
d02.treat[treatment=='RT',desc := 'Reduced tillage']
d02.treat[treatment=='NT',desc := 'No tillage']
d02.treat[treatment=='CC',desc := 'Crop cover']
d02.treat[treatment=='RI',desc := 'Row intercropping']

d02[is.na(n_dose), n_dose := median(d02$n_dose,na.rm=TRUE)]
d02[is.na(mat), mat := median(d02$mat,na.rm=TRUE)]
d02[is.na(map), map := median(d02$map,na.rm=TRUE)]
d02[is.na(ph), ph := median(d02$ph,na.rm=TRUE)]
d02[is.na(soc), soc := median(d02$soc,na.rm=TRUE)]
d02[is.na(clay), clay := median(d02$clay,na.rm=TRUE)]

d02[,mat_scaled := scale(mat)]
d02[,map_scaled := scale(map)]
d02[,clay_scaled := scale(clay)]
d02[,ph_scaled := scale(ph)]
d02[,soc_scaled := scale(soc)]
d02[,n_dose_scaled := scale(n_dose)]

d02[,res := fifelse(management=='RES','yes','no')]
d02[,cc := fifelse(management=='CC','yes','no')]
d02[,rot := fifelse(management=='ROT','yes','no')]
d02[,ri := fifelse(management=='RI','yes','no')]
d02[,nt := fifelse(management=='NT','yes','no')] 
d02[,rt := fifelse(management=='RT','yes','no')]

m1 <- rma.mv(yi,vi,
             mods = ~ nt +rt +rot + ri +
               n_dose_scaled + crop_type + clay_scaled + soc_scaled + ph_scaled + mat_scaled + map_scaled +
               ri:n_dose_scaled +
               rt:soc_scaled +rt:ph_scaled +
               nt:mat_scaled +nt:ph_scaled +
               rot:soc_scaled -1,      
             data = d02,
             random = list(~ 1|study_id), method="REML",sparse = TRUE)
m1
p1 <- predict(m1,addx=T)
m1.cols <- colnames(p1$X)
dt.new <- copy(r.dt)

# add the columns required for the ma model, baseline scenario 
dt.new[, ntno := fifelse(NOTILL == 1,1,0)]
dt.new[, ntyes := fifelse(NOTILL == 1,0,1)]
dt.new[, rtyes := fifelse(NOTILL == 1,0,1)]
dt.new[, riyes := fifelse(cropintensity == 1,0,1)]
dt.new[, rotyes := fifelse(cropintensity == 1,0,1)]

dt.new[, crop_typerice := crice]
dt.new[, crop_typewheat := cwheat]
dt.new[, crop_typemaize := cmaize]

dt.new[, ph_scaled := (phw * 0.1 - mean(d02$ph)) / sd(d02$ph)]
dt.new[, clay_scaled := (clay * 0.1 - mean(d02$clay)) / sd(d02$clay)]
dt.new[, soc_scaled := (soc * 0.1 - mean(d02$soc)) / sd(d02$soc)]
dt.new[, n_dose_scaled := scale(nh4+no3+nam)]
dt.new[, map_scaled := (pre - mean(d02$map)) / sd(d02$map)]
dt.new[, mat_scaled := (mat  - mean(d02$mat)) / sd(d02$mat)]

dt.new[, `riyes:n_dose_scaled` := riyes * n_dose_scaled]
dt.new[, `rtyes:soc_scaled` := rtyes * soc_scaled]
dt.new[, `rtyes:ph_scaled` := rtyes * ph_scaled]
dt.new[, `ntyes:mat_scaled` := ntyes * mat_scaled]
dt.new[, `ntyes:ph_scaled` := ntyes * ph_scaled]
dt.new[, `rotyes:soc_scaled` := rotyes * soc_scaled]

dt.newmod <- as.matrix(dt.new[,mget(c(m1.cols))])

dt.pred <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))

cols <- c('pROMmean','pROMse','pROMcil','pROMciu','pROMpil','pROMpiu')
dt.new[,c(cols) := dt.pred]

dt.new[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]
dt.new[,pROMseAbs := (exp(pROMse) - 1) * 100]

##############################################NT##############################################

dt.s10 <- copy(dt.new)

dt.fert.bs <- dt.new[,list(mean = mean(nh4+no3+nam), sd = sd(nh4+no3+nam))]

dt.s10[, ntyes := fifelse(NOTILL ==1,1,0)]

dt.s10[, `riyes:n_dose_scaled` := riyes * n_dose_scaled]
dt.s10[, `rtyes:soc_scaled` := rtyes * soc_scaled]
dt.s10[, `rtyes:ph_scaled` := rtyes * ph_scaled]
dt.s10[, `ntyes:mat_scaled` := ntyes * mat_scaled]
dt.s10[, `ntyes:ph_scaled` := ntyes * ph_scaled]
dt.s10[, `rotyes:soc_scaled` := rotyes * soc_scaled]

dt.newmod <- as.matrix(dt.s10[,mget(c(m1.cols))])

dt.pred.s10 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
dt.s10[,c(cols) := dt.pred.s10]
dt.s10[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]

dt.fin <- dt.new[,.(x,y,base = pROMmeanAbs)]

dt.fin <- merge(dt.fin,dt.s10[,.(x,y,s10 = pROMmeanAbs)],by=c('x','y'))

dt.fin[, improvement := (s10 - base)]
summary(dt.fin)

r.fin <- terra::rast(dt.fin,type='xyz')
terra::crs(r.fin) <- 'epsg:4326'

# write as output
terra::writeRaster(r.fin,'C:/Users/asus/Desktop/paper1/upscaling/LLdata_for_upscaling/yield/scenario_SI0.tif', overwrite = TRUE)

##############################################RT##############################################

dt.s11 <- copy(dt.new)

dt.fert.bs <- dt.new[,list(mean = mean(nh4+no3+nam), sd = sd(nh4+no3+nam))]

dt.s11[, rtyes := fifelse(NOTILL == 1,1,0)]

dt.s11[, `riyes:n_dose_scaled` := riyes * n_dose_scaled]
dt.s11[, `rtyes:soc_scaled` := rtyes * soc_scaled]
dt.s11[, `rtyes:ph_scaled` := rtyes * ph_scaled]
dt.s11[, `ntyes:mat_scaled` := ntyes * mat_scaled]
dt.s11[, `ntyes:ph_scaled` := ntyes * ph_scaled]
dt.s11[, `rotyes:soc_scaled` := rotyes * soc_scaled]

dt.newmod <- as.matrix(dt.s11[,mget(c(m1.cols))])

dt.pred.s11 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
dt.s11[,c(cols) := dt.pred.s11]
dt.s11[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]

dt.fin <- dt.new[,.(x,y,base = pROMmeanAbs)]

dt.fin <- merge(dt.fin,dt.s11[,.(x,y,s11 = pROMmeanAbs)],by=c('x','y'))

dt.fin[, improvement := (s11 - base)]
summary(dt.fin)

r.fin <- terra::rast(dt.fin,type='xyz')
terra::crs(r.fin) <- 'epsg:4326'

# write as output
terra::writeRaster(r.fin,'C:/Users/asus/Desktop/paper1/upscaling/LLdata_for_upscaling/yield/scenario_SI1.tif', overwrite = TRUE)


##############################################ROT##############################################

dt.s12 <- copy(dt.new)

dt.fert.bs <- dt.new[,list(mean = mean(nh4+no3+nam), sd = sd(nh4+no3+nam))]

dt.s12[, rotyes := fifelse(cropintensity == 1,1,0)]

dt.s12[, `riyes:n_dose_scaled` := riyes * n_dose_scaled]
dt.s12[, `rtyes:soc_scaled` := rtyes * soc_scaled]
dt.s12[, `rtyes:ph_scaled` := rtyes * ph_scaled]
dt.s12[, `ntyes:mat_scaled` := ntyes * mat_scaled]
dt.s12[, `ntyes:ph_scaled` := ntyes * ph_scaled]
dt.s12[, `rotyes:soc_scaled` := rotyes * soc_scaled]

dt.newmod <- as.matrix(dt.s12[,mget(c(m1.cols))])

dt.pred.s12 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
dt.s12[,c(cols) := dt.pred.s12]
dt.s12[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]

dt.fin <- dt.new[,.(x,y,base = pROMmeanAbs)]

dt.fin <- merge(dt.fin,dt.s12[,.(x,y,s12 = pROMmeanAbs)],by=c('x','y'))

dt.fin[, improvement := (s12 - base)]
summary(dt.fin)

r.fin <- terra::rast(dt.fin,type='xyz')
terra::crs(r.fin) <- 'epsg:4326'

# write as output
terra::writeRaster(r.fin,'C:/Users/asus/Desktop/paper1/upscaling/LLdata_for_upscaling/yield/scenario_SI2.tif', overwrite = TRUE)


##############################################RI##############################################

dt.s13 <- copy(dt.new)

dt.fert.bs <- dt.new[,list(mean = mean(nh4+no3+nam), sd = sd(nh4+no3+nam))]

dt.s13[, riyes := fifelse(cropintensity == 1,1,0)]

dt.s13[, `riyes:n_dose_scaled` := riyes * n_dose_scaled]
dt.s13[, `rtyes:soc_scaled` := rtyes * soc_scaled]
dt.s13[, `rtyes:ph_scaled` := rtyes * ph_scaled]
dt.s13[, `ntyes:mat_scaled` := ntyes * mat_scaled]
dt.s13[, `ntyes:ph_scaled` := ntyes * ph_scaled]
dt.s13[, `rotyes:soc_scaled` := rotyes * soc_scaled]

dt.newmod <- as.matrix(dt.s13[,mget(c(m1.cols))])

dt.pred.s13 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
dt.s13[,c(cols) := dt.pred.s13]
dt.s13[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]

dt.fin <- dt.new[,.(x,y,base = pROMmeanAbs)]

dt.fin <- merge(dt.fin,dt.s13[,.(x,y,s13 = pROMmeanAbs)],by=c('x','y'))

dt.fin[, improvement := (s13 - base)]
summary(dt.fin)

r.fin <- terra::rast(dt.fin,type='xyz')
terra::crs(r.fin) <- 'epsg:4326'

# write as output
terra::writeRaster(r.fin,'C:/Users/asus/Desktop/paper1/upscaling/LLdata_for_upscaling/yield/scenario_SI3.tif', overwrite = TRUE)



##############################################combined optimal management practices##############################################

dt.s4 <- copy(dt.new)

dt.fert.bs <- dt.new[,list(mean = mean(nh4+no3+nam), sd = sd(nh4+no3+nam))]

dt.s4[, rtyes := fifelse(NOTILL == 1,1,0)]
dt.s4[, rotyes := fifelse(cropintensity == 1,1,0)]
dt.s4[, riyes := fifelse(cropintensity == 1,1,0)]

dt.s4[, `riyes:n_dose_scaled` := riyes * n_dose_scaled]
dt.s4[, `rtyes:soc_scaled` := rtyes * soc_scaled]
dt.s4[, `rtyes:ph_scaled` := rtyes * ph_scaled]
dt.s4[, `ntyes:mat_scaled` := ntyes * mat_scaled]
dt.s4[, `ntyes:ph_scaled` := ntyes * ph_scaled]
dt.s4[, `rotyes:soc_scaled` := rotyes * soc_scaled]

dt.newmod <- as.matrix(dt.s4[,mget(c(m1.cols))])

dt.pred.s4 <- as.data.table(predict(m1,newmods = dt.newmod,addx=F))
dt.s4[,c(cols) := dt.pred.s4]
dt.s4[,pROMmeanAbs := (exp(pROMmean) - 1) * 100]

dt.fin <- dt.new[,.(x,y,base = pROMmeanAbs)]

dt.fin <- merge(dt.fin,dt.s4[,.(x,y,s4 = pROMmeanAbs)],by=c('x','y'))

dt.fin[, improvement := (s4 - base)]
summary(dt.fin)

r.fin <- terra::rast(dt.fin,type='xyz')
terra::crs(r.fin) <- 'epsg:4326'

# write as output
terra::writeRaster(r.fin,'C:/Users/asus/Desktop/paper1/upscaling/LLdata_for_upscaling/yield/scenario_4.tif', overwrite = TRUE)   



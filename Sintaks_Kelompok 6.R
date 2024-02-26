library(sf)
library(tidyverse)
library(tidycensus)
library(tmap)
library(spdep)
library(tigris)
library(rmapshaper)
library(broom)
library(car)
library(spatialreg)
library(knitr)
library(stargazer)
library(spdep)
library(haven)
library(rgdal)
library(dplyr)
library(mapview)
library(stringr)
library(lubridate)
library(readxl)

train_fktp <- read.csv("D:/1.Perkuliahan/KULIAH HAKAM SEMESTER 6/Tugas/Analisis Data Spasial/Pekan 10/train_fktp.csv", 
                       header = FALSE,sep = ",")
colnames(train_fktp)=c('PSTV01','PSTV02','PSTV15','FKP02','FKP03','FKP04','FKP05','FKP06','FKP07','FKP08','FKP09',
                       'FKP10','FKP11','FKP12','FKP14','FKP14A','FKP15','FKP15A','FKP16','FKP17','FKP18','FKP19','FKP20',
                       'FKP21','FKP22','FKP13CLASS')
test_fktp <-  read.csv("D:/1.Perkuliahan/KULIAH HAKAM SEMESTER 6/Tugas/Analisis Data Spasial/Pekan 10/test_fktp.csv", 
                       header = FALSE,sep = ",")
colnames(test_fktp)=c('PSTV01','PSTV02','PSTV15','FKP02','FKP03','FKP04','FKP05','FKP06','FKP07','FKP08','FKP09',
                      'FKP10','FKP11','FKP12','FKP14','FKP14A','FKP15','FKP15A','FKP16','FKP17','FKP18','FKP19','FKP20',
                      'FKP21','FKP22')
train_fktp<-train_fktp[,-26]
fktp <- rbind(train_fktp,test_fktp)

fktp_Jatim <- filter(fktp, FKP06 %in% c(3501:3529,3571:3579))

#memfilter berdasarkan nomor peserta
fktp_Jatim_ICD10_I10 <- fktp_Jatim %>%
  filter(str_detect(FKP15,"R19"))
fktp_Jatim_ICD10_I10<-fktp_Jatim_ICD10_I10[!duplicated(fktp_Jatim_ICD10_I10$PSTV01),] %>%
  count(FKP06)

write.table(fktp_Jawa_ICD10_I10 ,file="D:/1.Perkuliahan/KULIAH HAKAM SEMESTER 6/Tugas/Analisis Data Spasial/Pekan 15/Data ICD R19_kunjungan Pulau Jawa.csv",
            row.names = FALSE,quote=FALSE,sep = ";", dec =".")

Indonesia <- st_read("D:/1.Perkuliahan/KULIAH HAKAM SEMESTER 6/Tugas/Analisis Data Spasial/Pekan 10/idn_adm_bps_20200401_shp/idn_admbnda_adm2_bps_20200401.shp")

#membaca data
Data2=read_excel("D:/1.Perkuliahan/KULIAH HAKAM SEMESTER 6/Tugas/Analisis Data Spasial/Pekan 16/Jatim.xlsx")
#Data multipoligon
JATIM=subset(Indonesia,ADM1_EN%in% c('Jawa Timur'))
JATIM <- JATIM %>%
  dplyr::select(ADM2_EN, geometry)

Prevalance_JATIM <- left_join(JATIM,Data2, by="ADM2_EN")


#SHP Indonesia
indonesia.shp=readOGR(dsn = "D:/1.Perkuliahan/KULIAH HAKAM SEMESTER 6/Tugas/Analisis Data Spasial/Pekan 10/idn_adm_bps_20200401_shp",
                      layer = "idn_admbnda_adm2_bps_20200401")

Jatim<- indonesia.shp[which(indonesia.shp@data$ADM1_EN =='Jawa Timur'),]

#pembobot knn distance 
coords <- coordinates(Jatim)
WM3 <- knearneigh(coords, k=1)
knn.MW <- nb2listw(knn2nb(WM3))
Matriks = listw2mat(knn.MW)

#uji indeks moran
moran.test(Prevalance_JATIM$n,knn.MW,randomisation=FALSE)
moran.plot(Prevalance_JATIM$n,knn.MW)

library(nortest)
library(lmtest)
library(SciViews)


#PERSAMAAN REGRESI 
reg.eq=n~X2+X3+X6
#OLS 
reg.OLS=lm(reg.eq,data=Data2) 
summary(reg.OLS) 

vif(reg.OLS)
#uji LM
llm=lm.LMtests(reg.OLS,knn.MW,test=c('LMerr','LMlag','SARMA')) 
summary(llm)

#SAR
SAR=lagsarlm(reg.eq,data=Data2, knn.MW) 
summary(SAR)

tmap_mode("plot")

View_G30 <- tm_shape(Prevalance_JATIM, unit = "mi", simplify = 0.5) +
  tm_basemap(leaflet::providers$Esri.WorldImagery)+
  tm_polygons(col = "n", style = "jenks",palette ="Blues", n=4,
              border.alpha = 0.3, title = "R19") +
  tm_text("ADM2_EN",size = 0.5, legend.size.show = T, col = "black")+
  tm_view(set.view = c(118.641492,-2.943041,5),text.size.variable=T)+
  tm_minimap(leaflet::providers$CartoDB.VoyagerNoLabels,toggle = F)

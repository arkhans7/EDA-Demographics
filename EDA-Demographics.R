library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(RColorBrewer)
library(gganimate)
library(gifski)
library(ggthemes)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(tidyverse)
library(raster)
library(sf)
library(sp)
library(scales)
library(ggsn)
library(plotly)
library(dbscan)
library(rgdal)
library(spatialreg)
library(spatial)
library(mapview)
library(dplyr)
library(tidyselect)
library(tidyr)
library(rgeos)
library(tmap)

# DATASET

profil <- read_xlsx("Profil Kabupaten Kota.xlsx")
lahir <- read_xlsx("kelahiranbayi.xlsx")%>% arrange(desc(`Kelahiran`))
reg <- read_xlsx("Penduduk Miskin TPT.xlsx")
propor <- read_xlsx("Proporsi Jenis Kelamin.xlsx")
IPM <- read_xlsx("IPM.xlsx")
pd <- read_xlsx("Data Peta.xlsx")

profil$`Laju Pertumbuhan` = as.numeric(profil$`Laju Pertumbuhan`)
profil$`Penduduk Laki-laki`= as.numeric(profil$`Penduduk Laki-laki`)
profil$`Penduduk Perempuan`= as.numeric(profil$`Penduduk Perempuan`)
profil$`Banyak Penduduk`= as.numeric(profil$`Banyak Penduduk`)


profil1 <- profil[,-1]
summary(profil1)
profil2 <- colSums(profil1)

# BARPLOT
lahir <- lahir[order(lahir$Kelahiran, decreasing = T),]
lahir <- lahir[1:5,]
lahir1 <- lahir$Kelahiran
jum_lahir = paste0(lahir$Kelahiran)
ggplot(lahir) +
  aes(x = reorder(Kabupaten,Kelahiran), weight = Kelahiran) +
  geom_bar(fill ="#B8860B") + labs(title="5 Kabupaten/Kota dengan Kelahiran terbanyak (Ribu)",
                                     x="Kabupaten/Kota", y= "Jumlah Kelahiran") +   
  geom_text(aes(y = lahir$Kelahiran, label = jum_lahir, hjust = 1), size = 5) +
  theme_solarized() + coord_flip()


# HISTOGRAM
ggplot(profil, aes(x=profil$`Angka Kematian`)) + 
  geom_histogram(binwidth=1, colour="#111F31", fill="#B8860B") +
  labs(title="Angka Kematian Penduduk di Kabupaten/Kota Provinsi Jawa Timur (persen)",x="Angka Kematian (persen)", y = "Frekuensi") 
+ theme_solarized()

# STACKED STACKED BAR PLOT
Gender <- propor$Jenis
ggplot(propor,                                  
       aes(x = propor$Umur,
           y = propor$Jumlah,
           fill = Gender)) + 
  labs(title = "Proporsi Gender pada Usia Produktif dan Non-Produktif",
       x = "Kelompok Umur",
       y = "Jumlah Penduduk") +
  geom_bar(stat = "identity",
           position = "dodge") +
  theme_solarized()

# LINE CHART
line <- ggplot(data=IPM, aes(x=Tahun, y=`IPM`, group=1)) +
  geom_line(linetype = 1,color = "#111F31", size = 2)+
  geom_point(color = "black", size = 3)+
  scale_x_continuous(breaks = seq(2013,2018))+
  ggtitle("Perkembangan Indeks Pembangunan Manusia Provinsi Jawa Timur Dalam Rentang 2013-2018" , col = "blue")+
  ylab("Indeks Pembangunan Manusia (persen)")+
  theme_solarized()+
  transition_reveal(Tahun)
gganimate::animate(line,
                   width = 800,
                   renderer = gifski_renderer("line1.gif"))

# REGRESI
par(mfrow=c(1.1))

modelA <- lm(data=reg, reg$`Tingkat Pengangguran Terbuka`~reg$`Tenaga Kerja (Ribu Jiwa)`)
summary(modelA)
info <- summary(modelA)
plot(reg$`Tenaga Kerja (Ribu Jiwa)`, reg$`Tingkat Pengangguran Terbuka`, pch = 19,
     main = "Regresi Linier",
     xlab = "Tenaga Kerja",
     ylab = "TPT (persen)")
abline(modelA)
mtext(paste("R-sq :",round(info$r.squared,3)),line = -1,cex = 0.7)

pred <- fitted(modelA)
res <- resid(modelA)
sres <- rstandard(modelA)

# IDENTIK
plot(pred, sres, ylim=c(-3,3), 
     xlab="Fitted Value", 
     ylab="Standardized Residuals", 
     main="Fitted Value vs Standardized Residuals")
abline(h=2, lty = 2)
abline(h=-2, lty = 2)

# INDEPENDENT
n <- nrow(reg)
plot(1:n, sres, type="b", 
     xlab="Observation Order", 
     ylab="Standardized Residuals", 
     main="Observation Order vs Standardized Residuals")

# NORMAL
p <- (1:n - 0.5)/n * 100; p
sorted_sres <- sort(sres); sorted_sres
plot(sorted_sres, p,
     xlab="Standardized Residuals",
     ylab="Probability", main="Normal Probability Plot")


# BOXPLOT ANGKA KELAHIRAN
hist(profil$`Angka Kelahiran`, probability = TRUE, ylab = "",xlab = "Jumlah Kelahiran", col = "#B8860B",
     axes = FALSE, main = "")
axis(1)
lines(density(profil$`Angka Kelahiran`), col = "#E9967A", lwd = 2)
par(new = TRUE)
boxplot(profil$`Angka Kelahiran`, horizontal = TRUE, axes = FALSE,
        lwd = 2, col = "#8B0000",
        main = "Boxplot Angka Kelahiran Kabupaten/Kota",
        pch =19)

# MAPS
indonesia <- st_read('gadm40_IDN_2.shp')

names(indonesia)
indonesia$NAME_1

jatim <- indonesia %>%
  subset(indonesia$NAME_1 == "Jawa Timur")
jatim$NAME_1
jatim$NAME_2


View(pd)
Upah <- pd$UMR
jatim %>% mutate(namakolom = Upah)
View(pd)
pal <- colorNumeric(
  palette = "YlGnBu",
  domain = jatim$Upah
)


m <- leaflet(jatim) %>%
  addTiles() %>% 
  addPolygons(
    color = ~pal(Upah),
    weight = 2,
    opacity = 1,
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    
    label = paste0(jatim$NAME_2, " ", jatim$Upah),
    
    
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
    
    
  )  
m

## LEGEND
pal <- colorNumeric(
  palette = "YlGnBu",
  domain = jatim$Upah
)


m <- leaflet(jatim) %>%
  addTiles() %>% 
  addPolygons(
    color = ~pal(Upah),
    weight = 2,
    opacity = 1,
    dashArray = "3",
    fillOpacity = 0.7,
    
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    
    
    label = paste0(jatim$NAME_2, " ", jatim$Upah),
    
    
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
    
    
    
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~Upah,
    title = "UMR"
  )

m
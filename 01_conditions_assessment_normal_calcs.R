# assessing 'normal' conditions and deviations from normal in known drougth years.

library(ggplot2)
library(lubridate)
library(nlme)
library(mgcv)
# read in data
Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought")
#Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")


path.figs <- file.path(google.drive, "data/exploratory figures/")
if(!dir.exists(path.figs)) dir.create(path.figs)

ndvi.all <- readRDS(file.path(google.drive, "data/r_files/processed_files/landsat_ndvi_all.RDS"))
head(ndvi.all)
summary(ndvi.all)

# reading in Trent's SPI
ChicagolandSPI <- read.csv(file.path(google.drive, "data/data_sets/Daily Meteorological Data/Chicagoland_Daily_SPI.csv"))
ChicagolandSPEI <- read.csv(file.path(google.drive, "data/data_sets/Daily Meteorological Data/Chicagoland_Daily_SPEI.csv"))
ChicagolandTemp <- read.csv(file.path(google.drive, "data/data_sets/Daily Meteorological Data/Chicagoland_Daily_Temps.csv"))

# create column with date in ISO format; making it lowercase "date" so that it merges easier
ChicagolandSPI$date <- as.Date(ChicagolandSPI$Date, "%m/%d/%Y")
ChicagolandSPEI$date <- as.Date(ChicagolandSPEI$Date, "%m/%d/%Y")
ChicagolandTemp$date <- as.Date(ChicagolandTemp$Date, "%m/%d/%Y")
summary(ChicagolandSPI)
summary(ChicagolandSPEI)
summary(ChicagolandTemp)

dim(ChicagolandSPI); dim(ChicagolandSPEI); dim(ChicagolandTemp)


ndviMet <- merge(ndvi.all, ChicagolandTemp, all.x=T, all.y=F)
ndviMet <- merge(ndviMet, ChicagolandSPI, all.x=T, all.y=F)
ndviMet <- merge(ndviMet, ChicagolandSPEI, all.x=T, all.y = F)
summary(ndviMet)

#removing duplicate Date column (column2)
ndviMet <- ndviMet[,!names(ndviMet) %in% "Date"]

names(ndviMet) <- tolower(names(ndviMet))

# holding back data for 2023 to be used as a validation data set later
ndviMet <- ndviMet[!ndviMet$year==2023,]


# Wanting to build a model that defines what normal is. This would be the black curve in christy's figs from 2/14/24

test <- gamm(ndvi ~ s(doy, by=type, k=12) + type -1, random=list(satellite=~1), data=ndviMet)

ndvi.norm.df <- data.frame(doy =1:365, type=rep(unique(ndviMet$type), each=365))
head(ndvi.norm.df)

ndvi.norm.df$ndvi.normal <- predict(test$gam, newdata = ndvi.norm.df)
head(ndvi.norm.df)

# saving the modeled normals
saveRDS(ndvi.norm.df, file.path(google.drive, "data/r_files/processed_files", "gamm_modeled_ndvi_norms.rds"))

ndviMet2 <- merge(ndviMet, ndvi.norm.df, by=c("doy", "type"), all.x=T)
head(ndviMet2)

ggplot(data=ndviMet2) +
  geom_path(aes(x=doy, y = ndvi.normal, group=type, col=type))+
  #geom_point(aes(x=doy, y=ndvi), col="dodgerblue") +
  scale_color_manual(values=c("#7fc97f", "#beaed4", "#fdc086", "#e31a1c", "#386cb0", "#f0027f", "#bf5b17"))+
  theme_bw()


# wanting to identify periods of drought within the data
summary(ndviMet2)

# try cutting each met variable into 5 bins to start
ndviMet2$spi.14d.bins <- cut(ndviMet2$x14d.spi, breaks=5, labels=c(1:5))
ndviMet2$spi.30d.bins <- cut(ndviMet2$x30d.spi, breaks=5, labels=c(1:5))
ndviMet2$spi.60d.bins <- cut(ndviMet2$x60d.spi, breaks=5, labels=c(1:5))
ndviMet2$spi.90d.bins <- cut(ndviMet2$x90d.spi, breaks=5, labels=c(1:5))

ndviMet2$spei.14d.bins <- cut(ndviMet2$x14d.spei, breaks=5, labels=c(1:5))
ndviMet2$spei.30d.bins <- cut(ndviMet2$x30d.spei, breaks=5, labels=c(1:5))
ndviMet2$spei.60d.bins <- cut(ndviMet2$x60d.spei, breaks=5, labels=c(1:5))
ndviMet2$spei.90d.bins <- cut(ndviMet2$x90d.spei, breaks=5, labels=c(1:5))

# reversing the labels on the temperature variables here to be in the same direction as the drougth variables above
ndviMet2$tmin.14d.bins <- cut(ndviMet2$tmin14d, breaks=5, labels=rev(c(1:5)))
ndviMet2$tmin.30d.bins <- cut(ndviMet2$tmin30d, breaks=5, labels=rev(c(1:5)))
ndviMet2$tmin.60d.bins <- cut(ndviMet2$tmin60d, breaks=5, labels=rev(c(1:5)))
ndviMet2$tmin.90d.bins <- cut(ndviMet2$tmin90d, breaks=5, labels=rev(c(1:5)))

ndviMet2$tmax.14d.bins <- cut(ndviMet2$tmax14d, breaks=5, labels=rev(c(1:5)))
ndviMet2$tmax.30d.bins <- cut(ndviMet2$tmax30d, breaks=5, labels=rev(c(1:5)))
ndviMet2$tmax.60d.bins <- cut(ndviMet2$tmax60d, breaks=5, labels=rev(c(1:5)))
ndviMet2$tmax.90d.bins <- cut(ndviMet2$tmax90d, breaks=5, labels=rev(c(1:5)))

ndviMet2$month <- month(ndviMet2$date)


ggplot(data=ndviMet2)+ facet_grid(month~.) +
  geom_histogram(aes(x=x14d.spei, fill=as.factor(spei.14d.bins)))

# sanity check to make sure that the normal curve fits within the distribution
ggplot(data=ndviMet2[ndviMet2$type %in% "urban-medium",]) +
  geom_point(aes(x=doy, y=ndvi), col="grey50") +
  geom_line(aes(x=doy, y=ndvi.normal), col="black", linewidth=2) +
  theme_bw()



# plotting NDVI obervatsions vs modeled normal for the lowest SPEI periods
# SPEI14d
plot.spei14 <- ggplot() + facet_wrap(month~., scales="free_x") +
  geom_violin(data = ndviMet2[!is.na(ndviMet2$spei.14d.bins) & ndviMet2$spei.14d.bins %in% 3 & ndviMet2$month %in% c(3:10) & ndviMet2$type %in% "urban-medium",], aes(x=month-0.5, y=ndvi, fill= "All Conditions")) +
  geom_violin(data = ndviMet2[!is.na(ndviMet2$spei.14d.bins) & ndviMet2$spei.14d.bins %in% 1 & ndviMet2$type=="urban-medium" & ndviMet2$month %in% c(3:10),], aes(x=month+0.5, y=ndvi, fill = "Driest Conditions")) +
  geom_point(data = ndviMet2[!is.na(ndviMet2$spei.14d.bins) &ndviMet2$type=="urban-medium" & ndviMet2$year==2005& ndviMet2$month %in% c(3:10),], aes(x=month, y=ndvi, col = "2005"), size=2)+
  geom_point(data = ndviMet2[!is.na(ndviMet2$spei.14d.bins) &ndviMet2$type=="urban-medium" & ndviMet2$year==2012& ndviMet2$month %in% c(3:10),], aes(x=month, y=ndvi, col = "2012"), pch=17, size = 2)+
  scale_fill_manual(values = c("All Conditions" = "black", "Driest Conditions"="orange2"))+
  scale_color_manual(values = c("2005" = "tan2", "2012" = "red3"))+
  labs(x="Month", y="NDVI Value", title="Lowest SPEI14 compared to normal with 2005 & 2012")+
  theme_bw()

# SPEI90d
plot.spei90 <- ggplot() + facet_wrap(month~., scales="free_x") +
  geom_violin(data = ndviMet2[ndviMet2$month %in% c(3:10) & ndviMet2$type %in% "urban-medium",], aes(x=month-0.5, y=ndvi, fill= "All Conditions")) +
  geom_violin(data = ndviMet2[!is.na(ndviMet2$spei.90d.bins) & ndviMet2$spei.90d.bins %in% 1 & ndviMet2$type=="urban-medium" & ndviMet2$month %in% c(3:10),], aes(x=month+0.5, y=ndvi, fill = "Driest Conditions")) +
  geom_point(data = ndviMet2[!is.na(ndviMet2$spei.90d.bins) &ndviMet2$type=="urban-medium" & ndviMet2$year==2005& ndviMet2$month %in% c(3:10),], aes(x=month, y=ndvi, col = "2005"), size=2)+
  geom_point(data = ndviMet2[!is.na(ndviMet2$spei.90d.bins) &ndviMet2$type=="urban-medium" & ndviMet2$year==2012& ndviMet2$month %in% c(3:10),], aes(x=month, y=ndvi, col="2012"), pch=17, size = 2)+
  labs(x="Month", y="NDVI Value", title="Lowest SPEI90 compared to normal with 2005 & 2012") +
  scale_fill_manual(values = c("All Conditions" = "black", "Driest Conditions"="orange2"))+
  scale_color_manual(values = c("2005" = "tan2", "2012" = "red3"))+
  theme_bw()



# SPI14d
plot.spi14 <- ggplot() + facet_wrap(month~., scales="free_x") +
  geom_violin(data = ndviMet2[!is.na(ndviMet2$spi.14d.bins) & ndviMet2$spi.14d.bins %in% 3 & ndviMet2$month %in% c(3:10) & ndviMet2$type %in% "urban-medium",], aes(x=month-0.5, y=ndvi, fill= "All Conditions")) +
  geom_violin(data = ndviMet2[!is.na(ndviMet2$spi.14d.bins) & ndviMet2$spi.14d.bins %in% 1 & ndviMet2$type=="urban-medium" & ndviMet2$month %in% c(3:10),], aes(x=month+0.5, y=ndvi, fill = "Driest Conditions")) +
  geom_point(data = ndviMet2[!is.na(ndviMet2$spi.14d.bins) &ndviMet2$type=="urban-medium" & ndviMet2$year==2005& ndviMet2$month %in% c(3:10),], aes(x=month, y=ndvi, col = "2005"), size=2)+
  geom_point(data = ndviMet2[!is.na(ndviMet2$spi.14d.bins) &ndviMet2$type=="urban-medium" & ndviMet2$year==2012& ndviMet2$month %in% c(3:10),], aes(x=month, y=ndvi, col = "2012"), pch=17, size = 2)+
  scale_fill_manual(values = c("All Conditions" = "black", "Driest Conditions"="orange2"))+
  scale_color_manual(values = c("2005" = "tan2", "2012" = "red3"))+
  labs(x="Month", y="NDVI Value", title="Lowest SPI14 compared to normal with 2005 & 2012")+
  theme_bw()

# SPI90d
plot.spi90 <- ggplot() + facet_wrap(month~., scales="free_x") +
  geom_violin(data = ndviMet2[ndviMet2$month %in% c(3:10) & ndviMet2$type %in% "urban-medium",], aes(x=month-0.5, y=ndvi, fill= "All Conditions")) +
  geom_violin(data = ndviMet2[!is.na(ndviMet2$spi.90d.bins) & ndviMet2$spi.90d.bins %in% 1 & ndviMet2$type=="urban-medium" & ndviMet2$month %in% c(3:10),], aes(x=month+0.5, y=ndvi, fill = "Driest Conditions")) +
  geom_point(data = ndviMet2[!is.na(ndviMet2$spi.90d.bins) &ndviMet2$type=="urban-medium" & ndviMet2$year==2005& ndviMet2$month %in% c(3:10),], aes(x=month, y=ndvi, col = "2005"), size=2)+
  geom_point(data = ndviMet2[!is.na(ndviMet2$spi.90d.bins) &ndviMet2$type=="urban-medium" & ndviMet2$year==2012& ndviMet2$month %in% c(3:10),], aes(x=month, y=ndvi, col="2012"), pch=17, size = 2)+
  labs(x="Month", y="NDVI Value", title="Lowest SPI90 compared to normal with 2005 & 2012") +
  scale_fill_manual(values = c("All Conditions" = "black", "Driest Conditions"="orange2"))+
  scale_color_manual(values = c("2005" = "tan2", "2012" = "red3"))+
  theme_bw()

library(cowplot)
png(filename = file.path(path.figs, "14day_drought_violins.png"), height = 11, width = 15, unit = "in", res = 300)
plot_grid(plot.spei14, plot.spi14, ncol=2)
dev.off()

png(filename = file.path(path.figs, "90day_drought_violins.png"), height = 11, width = 15, unit = "in", res = 300)
plot_grid(plot.spei90, plot.spi90, ncol=2)
dev.off()

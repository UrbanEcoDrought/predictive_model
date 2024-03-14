library(tidyverse)
library(randomForest)  #Random forests
library(units)         #Change units in spatial work
library(pdp)           #Partial dependence plots
library(vip)           #Variable importance plots

set.seed(123)

# Read in ndviMet data
ndviMet <- readRDS("G:/Shared drives/Urban Ecological Drought/data/r_files/processed_files/landsat_ndvi_metVars_combined.RDS")
head(ndviMet)
summary(ndviMet)
# need to remove missing data from the NDVI column
ndviMet2 <- ndviMet[!is.na(ndviMet$NDVI),]
summary(ndviMet2)
head(ndviMet2[is.na(ndviMet2$X30d.SPEI),])

# Subsetting to months 4-10; cutting off at 2021 becasue we don't have 2022-2023 met data yet

ndviMet3 <- ndviMet2[ndviMet2$doy %in% c(150:245) & ndviMet2$year %in% c(2001:2021),]
summary(ndviMet3)

ndviMet3[is.na(ndviMet3$X14d.SPEI),]
# want this to be empty so NO NA's

randoAll<-randomForest(NDVI~., data=ndviMet3[ndviMet3$type=="urban-medium",!names(ndviMet3) %in% c("date", "Date", "year", "month", "type")], mtry = 6, ntree = 500)
randoAll #97% variance explained

AllVIP<- vip(randoAll, include_type = TRUE, horizontal = TRUE, 
             aesthetics = list(fill = '#2a2e38'), num_features = 15) +
  labs(title = "Days 150-245, Urban Medium") +
  theme_bw() +
  theme(axis.title.x = element_blank())

AllVIP

ggsave(paste0(path, "Dropbox/LindsayWorking/GradSchool/Dissertation/Figures/AllVIP.png"), 
       AllVIP, width = 3, height = 2.25, unit = "in", dpi = 300)

#This pdp section is kind of obnoxious, but it makes really pretty plots.
#It makes each plot separately, combines them, then writes them out with
#a facet wrap. You need to specify which variables you want to do them for.
#I chose the top six from the VIP.

#There is a less verbose and less pretty way to make pdps at the end.

#Make top six pdps
pdp::partial(randoAll,
             pred.var = 'X90d.SPI',
             plot = FALSE,
             plot.engine = "ggplot2",
             rug = TRUE,
             progress = "text",
             quantiles = TRUE,
             probs = 0:20/20,
             ice = TRUE,
             #center = TRUE,
             alpha = .1,
             parallel = TRUE) %>%
  as_tibble() %>%
  group_by(X90d.SPI) %>%
  summarize(ci = list(mean_cl_normal(yhat))) %>%
  unnest(ci) %>%
  rename(mean_pred = y, lower_ci = ymin, upper_ci = ymax, value = X90d.SPI) %>% 
  mutate(type = 'X90d.SPI') -> presetPrct_pdpAll;

pdp::partial(randoAll,
             pred.var = 'X60d.SPI',
             plot = FALSE,
             plot.engine = "ggplot2",
             rug = TRUE,
             progress = "text",
             quantiles = TRUE,
             probs = 0:20/20,
             ice = TRUE,
             #center = FALSE,
             alpha = .1) %>% 
  #parallel = TRUE) %>%
  as_tibble() %>%
  group_by(X60d.SPI) %>%
  summarize(ci = list(mean_cl_normal(yhat))) %>%
  unnest(ci) %>%
  rename(mean_pred = y, lower_ci = ymin, upper_ci = ymax, value = X60d.SPI) %>% 
  mutate(type = 'X60d.SPI') -> Med_ncm_pdpAll;

pdp::partial(randoAll,
             pred.var = 'satellite',
             plot = FALSE,
             plot.engine = "ggplot2",
             rug = TRUE,
             progress = "text",
             quantiles = TRUE,
             probs = 0:20/20,
             ice = TRUE,
             #center = FALSE,
             alpha = .1) %>% 
  #parallel = TRUE) %>%
  as_tibble() %>%
  group_by(satellite) %>%
  summarize(ci = list(mean_cl_normal(yhat))) %>%
  unnest(ci) %>%
  rename(mean_pred = y, lower_ci = ymin, upper_ci = ymax, value = satellite) %>% 
  mutate(type = 'satellite') -> popDen_pdpAll;

pdp::partial(randoAll,
             pred.var = 'X60d.SPEI',
             plot = FALSE,
             plot.engine = "ggplot2",
             rug = TRUE,
             progress = "text",
             quantiles = TRUE,
             probs = 0:20/20,
             ice = TRUE,
             #center = FALSE,
             alpha = .1) %>% 
  #parallel = TRUE) %>%
  as_tibble() %>%
  group_by(X60d.SPEI) %>%
  summarize(ci = list(mean_cl_normal(yhat))) %>%
  unnest(ci) %>%
  rename(mean_pred = y, lower_ci = ymin, upper_ci = ymax, value = X60d.SPEI) %>% 
  mutate(type = 'X60d.SPEI') -> ownerP_pdpAll;

pdp::partial(randoAll,
             pred.var = 'X90d.SPEI',
             plot = FALSE,
             plot.engine = "ggplot2",
             rug = TRUE,
             progress = "text",
             quantiles = TRUE,
             probs = 0:20/20,
             ice = TRUE,
             #center = FALSE,
             alpha = .1) %>% 
  #parallel = TRUE) %>%
  as_tibble() %>%
  group_by(X90d.SPEI) %>%
  summarize(ci = list(mean_cl_normal(yhat))) %>%
  unnest(ci) %>%
  rename(mean_pred = y, lower_ci = ymin, upper_ci = ymax, value = X90d.SPEI) %>% 
  mutate(type = 'X90d.SPEI') -> HighEd_pdpAll;

pdp::partial(randoAll,
             pred.var = 'TMAX90d',
             plot = FALSE,
             plot.engine = "ggplot2",
             rug = TRUE,
             progress = "text",
             quantiles = TRUE,
             probs = 0:20/20,
             ice = TRUE,
             #center = FALSE,
             alpha = .1) %>% 
  #parallel = TRUE) %>%
  as_tibble() %>%
  group_by(TMAX90d) %>%
  summarize(ci = list(mean_cl_normal(yhat))) %>%
  unnest(ci) %>%
  rename(mean_pred = y, lower_ci = ymin, upper_ci = ymax, value = TMAX90d) %>% 
  mutate(type = 'TMAX90d') -> House_age_pdpAll;

#Combine
comboAll <- rbind(presetPrct_pdpAll, Med_ncm_pdpAll, popDen_pdpAll, 
                  ownerP_pdpAll, HighEd_pdpAll, House_age_pdpAll)

#plot
allPDP <- ggplot(comboAll, aes(x = value, y = mean_pred, group=type)) +
  geom_line(color = "black") +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = 'gray',alpha = .4) +
  #scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  #scale_x_continuous(labels = pretty(comboAll[!comboAll$type=="satellite", "value"], n = 5)) +
  #ylim(c(.22,.3)) +
  labs(title="Day 150-245; urban Medium") +
  ylab("NDVI") +
  theme_bw() +
  # theme(
  #   axis.title.x = element_blank(),
  #   axis.text.x = element_blank(),
  #   axis.ticks.x = element_blank(),
  #   axis.ticks.y = element_blank()) +
  theme(axis.text.x = element_text(angle=45)) +
  facet_wrap(~type, ncol = 3)
allPDP

ggsave(paste0(path, "Dropbox/LindsayWorking/GradSchool/Dissertation/Figures/allPDP.png"), 
       allPDP, width = 3.25, height = 2.05, unit = "in", dpi = 300)


##############

#Simple pdps

#PDP values for random forest 1
Owner <- partial(rando, pred.var = 'OwnerP')
Pct1940 <- partial(rando, pred.var = 'Prct1940')
HouseAge <- partial(rando, pred.var = 'House_age')

#Plot them side by side
par(mfcol=c(1,3))
plot(Pct1940, xlab = 'Percent 1940 forest', ylab = 'Partial dependence')
plot(Owner, xlab = 'Percent owner-occupied homes', ylab = 'Partial dependence')
plot(HouseAge, xlab = 'Median house age', ylab = 'Partial dependence')
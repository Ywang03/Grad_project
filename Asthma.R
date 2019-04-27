library(readr)
library(janitor)
library(dplyr)
library(stringr)
library(tidyverse)
library(lme4)
library(rvest)

library(choroplethr)
library(choroplethrMaps)
library(ggplot2)
#pollutant = Annual average ambient concentrations of PM2.5 in micrograms per cubic meter (based on seasonal averages and daily measurement)

setwd("C:/Users/syrav/Desktop/19spring/Data")
air_asthma_ppl <- read_csv("air_asthma_ppl.csv") 
temperature<- read_csv("temperature.csv") 
ppl<- read_csv("ppl.csv") 
temperature$X1<-NULL
air_asthma_ppl$X1<-NULL

#map pollutant
pollutant_m <- air_asthma_ppl[, c("CountyName","CountyFips","ReportYear","pollutant")]
pollutant_m$CountyFips <- as.character(pollutant_m$CountyFips)
pollutant_m$CountyName <- tolower(pollutant_m$CountyName)
ppl2 <- group_by(ppl, Id2)
ppl2 <- filter(ppl2, population == max(population))
head(ppl2)
ppl2$Id2 <- as.character(ppl2$Id2)
ppl2$county <- tolower(ppl2$county)
pollutant_m$CountyFips <- as.character(pollutant_m$CountyFips)
ppl_pollutant_m<- left_join(ppl2, pollutant_m , by = c("Id2" = "CountyFips"))
ppl_pollutant_m <- group_by(ppl_pollutant_m, county)
ppl_pollutant_m <- summarize(ppl_pollutant_m, value = sum(pollutant, na.rm = TRUE))
ppl_pollutant_m$county[ppl_pollutant_m$county == "le"] <- "le flore"
ppl_pollutant_m$county[ppl_pollutant_m$county == "roger"] <- "roger mills"
data(county.regions)
county.regions <- filter(county.regions, state.name == "oklahoma")
ppl_pollutant_m <- left_join(ppl_pollutant_m, county.regions, by = c("county" = "county.name"))%>%
  filter(is.na(region) == F)
county_map_pollutant <- county_choropleth(ppl_pollutant_m, legend = "pollutant", 
                                          state_zoom = "oklahoma")+
  labs(title = "  pollutant more concertrated in Eastern Oklahoma")
county_map_pollutant


#map rate
head(air_asthma_ppl)
rate_m<-air_asthma_ppl[, c("CountyName","CountyFips","ReportYear","rate")]
rate_m$CountyFips <- as.character(rate_m$CountyFips)
rate_m$CountyName <- tolower(rate_m$CountyName)
head(rate_m)
ppl3 <- group_by(ppl, Id2)
ppl3 <- filter(ppl3, population == max(population))
head(ppl3)
ppl3$Id2 <- as.character(ppl3$Id2)
ppl3$county <- tolower(ppl3$county)
ppl_rate_m<- left_join(ppl3, rate_m, by = c("Id2" = "CountyFips"))
ppl_rate_m <- group_by(ppl_rate_m, county)
ppl_rate_m <- summarize(ppl_rate_m, value = sum(rate, na.rm = TRUE))
head(ppl_rate_m)
ppl_rate_m$county[ppl_rate_m$county == "le"] <- "le flore"
ppl_rate_m$county[ppl_rate_m$county == "roger"] <- "roger mills"
data(county.regions)
county.regions <- filter(county.regions, state.name == "oklahoma")
ppl_rate_m <- left_join(ppl_rate_m, county.regions, by = c("county" = "county.name"))%>%
  filter(is.na(region) == F)
county_map_rate <- county_choropleth(ppl_rate_m, legend = "rate", 
                                     state_zoom = "oklahoma")+
  labs(title = "  rate higer in Southwest Oklahoma")
county_map_rate


# map asin(sqrt(rate))
head(air_asthma_ppl)
asin_sqrt_rate_m<-air_asthma_ppl[, c("CountyName","CountyFips","ReportYear","rate_new")]
asin_sqrt_rate_m$CountyFips <- as.character(asin_sqrt_rate_m$CountyFips)
asin_sqrt_rate_m$CountyName <- tolower(asin_sqrt_rate_m$CountyName)
head(asin_sqrt_rate_m)
ppl_asin_sqrt_rate_m<- left_join(ppl2, asin_sqrt_rate_m, by = c("Id2" = "CountyFips"))
ppl_asin_sqrt_rate_m <- group_by(ppl_asin_sqrt_rate_m, county)
ppl_asin_sqrt_rate_m <- summarize(ppl_asin_sqrt_rate_m, value = sum(rate_new, na.rm = TRUE))
head(ppl_asin_sqrt_rate_m)
data(county.regions)
county.regions <- filter(county.regions, state.name == "oklahoma")
ppl_asin_sqrt_rate_m <- left_join(ppl_asin_sqrt_rate_m, county.regions, by = c("county" = "county.name"))%>%
  filter(is.na(region) == F)
county_map_ppl_asin_sqrt_rate <- county_choropleth(ppl_asin_sqrt_rate_m, legend = "asin_sqrt_rate", 
                                                   state_zoom = "oklahoma")+
  labs(title = "  asin_sqrt_rate higer in Southwest Oklahoma")
county_map_ppl_asin_sqrt_rate


#map log rate
head(air_asthma_ppl)
log_rate_m<-air_asthma_ppl[, c("CountyName","CountyFips","ReportYear","log_rate")]
log_rate_m$CountyFips <- as.character(log_rate_m$CountyFips)
log_rate_m$CountyName <- tolower(log_rate_m$CountyName)
head(log_rate_m)
ppl_log_rate_m<- left_join(ppl2, log_rate_m, by = c("Id2" = "CountyFips"))
ppl_log_rate_m <- group_by(ppl_log_rate_m, county)
ppl_log_rate_m <- summarize(ppl_log_rate_m, value = sum(log_rate, na.rm = TRUE))
head(ppl_log_rate_m)
data(county.regions)
county.regions <- filter(county.regions, state.name == "oklahoma")
ppl_log_rate_m <- left_join(ppl_log_rate_m, county.regions, by = c("county" = "county.name"))%>%
  filter(is.na(region) == F)
county_map_ppl_log_rate <- county_choropleth(ppl_log_rate_m, legend = "log_rate", 
                                             state_zoom = "oklahoma")+
  labs(title = "  log rate higer in central Oklahoma")
county_map_ppl_log_rate


#map z-pop
head(air_asthma_ppl)
z_pop_m<-air_asthma_ppl[, c("CountyName","CountyFips","ReportYear","z_pop")]
z_pop_m$CountyFips <- as.character(z_pop_m$CountyFips)
z_pop_m$CountyName <- tolower(z_pop_m$CountyName)
head(z_pop_m)
ppl_z_pop_m <- left_join(ppl3, z_pop_m, by = c("Id2" = "CountyFips"))
ppl_z_pop_m <- group_by(ppl_z_pop_m, county)
ppl_z_pop_m <- summarize(ppl_z_pop_m, value = sum(z_pop, na.rm = TRUE))
head(ppl_z_pop_m)
ppl_z_pop_m$county[ppl_z_pop_m$county == "le"] <- "le flore"
ppl_z_pop_m$county[ppl_z_pop_m$county == "roger"] <- "roger mills"
data(county.regions)
county.regions <- filter(county.regions, state.name == "oklahoma")
ppl_z_pop_m <- left_join(ppl_z_pop_m, county.regions, by = c("county" = "county.name"))%>%
  filter(is.na(region) == F)
county_map_z_pop <- county_choropleth(ppl_z_pop_m, legend = "z_pop", 
                                      state_zoom = "oklahoma")+
  labs(title = "  z-pop higer in central Oklahoma")
county_map_z_pop

#map z-pollutant
head(air_asthma_ppl)
z_pollutant_m<-air_asthma_ppl[, c("CountyName","CountyFips","ReportYear","z_pollutant")]
z_pollutant_m$CountyFips <- as.character(z_pollutant_m$CountyFips)
z_pollutant_m$CountyName <- tolower(z_pollutant_m$CountyName)
head(z_pollutant_m)
ppl_z_pollutant_m<- left_join(ppl3, z_pollutant_m, by = c("Id2" = "CountyFips"))
ppl_z_pollutant_m <- group_by(ppl_z_pollutant_m, county)
ppl_z_pollutant_m <- summarize(ppl_z_pollutant_m, value = sum(z_pollutant, na.rm = TRUE))
head(ppl_z_pollutant_m)
ppl_z_pollutant_m$county[ppl_z_pollutant_m$county == "le"] <- "le flore"
ppl_z_pollutant_m$county[ppl_z_pollutant_m$county == "roger"] <- "roger mills"
data(county.regions)
county.regions <- filter(county.regions, state.name == "oklahoma")
ppl_z_pollutant_m <- left_join(ppl_z_pollutant_m, county.regions, by = c("county" = "county.name"))%>%
  filter(is.na(region) == F)
county_map_z_pollutant<-county_choropleth(ppl_z_pollutant_m, state_zoom = "oklahoma", legend = "z_pollutant", num_colors = 1) +
  labs(title = "  Z-score of pollutant more concertrated in Eastern Oklahoma")
county_map_z_pollutant

# log pollutant
air_asthma_ppl$log_pollutant <-log(air_asthma_ppl$pollutant)
#map
log_pollutant_m<-air_asthma_ppl[, c("CountyName","CountyFips","ReportYear","log_pollutant")]
log_pollutant_m$CountyFips <- as.character(log_pollutant_m$CountyFips)
log_pollutant_m$CountyName <- tolower(log_pollutant_m$CountyName)
ppl_log_pollutant_m<- left_join(ppl3, log_pollutant_m, by = c("Id2" = "CountyFips"))
ppl_log_pollutant_m <- group_by(ppl_log_pollutant_m, county)
ppl_log_pollutant_m <- summarize(ppl_log_pollutant_m, value = sum(log_pollutant, na.rm = TRUE))
head(ppl_log_pollutant_m)
ppl_log_pollutant_m$county[ppl_log_pollutant_m$county == "le"] <- "le flore"
ppl_log_pollutant_m$county[ppl_log_pollutant_m$county == "roger"] <- "roger mills"
data(county.regions)
county.regions <- filter(county.regions, state.name == "oklahoma")
ppl_log_pollutant_m <- left_join(ppl_log_pollutant_m, county.regions, by = c("county" = "county.name"))%>%
  filter(is.na(region) == F)
county_map_log_pollutant<-county_choropleth(ppl_log_pollutant_m, state_zoom = "oklahoma", legend = "log_pollutant")+
  labs(title = "  log pollutant higer in Northeast Oklahoma")
county_map_log_pollutant


# maps
county_map_ppl_log_rate
county_map_ppl_asin_sqrt_rate
county_map_rate
county_map_pollutant
county_map_z_pop
county_map_z_pollutant
county_map_log_pollutant






air_asthma_ppl$CountyName<-as.factor(air_asthma_ppl$CountyName)
air_asthma_ppl$ReportYear<-as.character(air_asthma_ppl$ReportYear)


par(mfrow=c(1,2)) 
# mixed model
# rate
rate<-air_asthma_ppl[, c("CountyName","pollutant","population","rate","z_pop","z_pollutant","log_pollutant","log_ppl")]
rate$CountyName<-as.factor(rate$CountyName)
str(rate)
plot(rate)
summary(rate)
library(lmerTest)
#fixed effects "pollutant","population"
#random effect "CountyName"
rate.lme4 = lmer(rate ~ pollutant+population+(1|CountyName),
                 data = rate)
summary(rate.lme4)
anova(rate.lme4)
extractAIC(rate.lme4)
plot(fitted(rate.lme4), residuals(rate.lme4),
     xlab = "Fitted Values", ylab = "Residuals")
abline(h=0, lty=2)
#lines(smooth.spline(fitted(rate.lme4), residuals(rate.lme4))) 
qqnorm(resid(rate.lme4))
qqline(resid(rate.lme4))
#plot(rate$pollutant, rate.lme4, main="Residual Plot\npollutant")
#abline(h=c(-2,2))
plot(rate$pollutant, rate$rate)
plot(rate2$pollutant, rate2$rate)
plot(rate$population, rate$rate)
plot(rate2$population, rate2$rate)
plot(rate$log_ppl, rate$rate)
plot(rate2$log_ppl, rate2$rate)
#fixed effects "population","z_pollutant"
#random effect "CountyName"
rate2.lme4 = lmer(rate ~ population+z_pollutant+(1|CountyName),
                  data = rate)
summary(rate2.lme4)
anova(rate2.lme4)
extractAIC(rate2.lme4)
plot(fitted(rate2.lme4), residuals(rate2.lme4),
     xlab = "Fitted Values", ylab = "Residuals")
abline(h=0, lty=2)
#lines(smooth.spline(fitted(rate2.lme4), residuals(rate2.lme4))) 
qqnorm(resid(rate2.lme4))
qqline(resid(rate2.lme4))


#fixed effects "log_ppl","log_pollutant"
#random effect "CountyName"
rate3.lme4 = lmer(rate ~ population+log_pollutant+(1|CountyName),
                  data = rate)
summary(rate3.lme4)
anova(rate3.lme4)
extractAIC(rate3.lme4)
plot(fitted(rate3.lme4), residuals(rate3.lme4),
     xlab = "Fitted Values", ylab = "Residuals")
abline(h=0, lty=2)
#lines(smooth.spline(fitted(rate3.lme4), residuals(rate3.lme4))) 
qqnorm(resid(rate3.lme4))
qqline(resid(rate3.lme4))


#fixed effects "pollutant","z_pop"
#random effect "CountyName"
rate4.lme4 = lmer(rate ~ z_pop+pollutant+(1|CountyName),
                  data = rate)
summary(rate4.lme4)
anova(rate4.lme4)
extractAIC(rate4.lme4)
plot(fitted(rate4.lme4), residuals(rate4.lme4),
     xlab = "Fitted Values", ylab = "Residuals")
abline(h=0, lty=2)
#lines(smooth.spline(fitted(rate4.lme4), residuals(rate4.lme4))) 
qqnorm(resid(rate4.lme4))
qqline(resid(rate4.lme4))

#fixed effects "pollutant","log_ppl"
#random effect "CountyName"
rate5.lme4 = lmer(rate ~ log_ppl*pollutant+(1|CountyName),
                  data = rate)
summary(rate5.lme4)
anova(rate5.lme4)
extractAIC(rate5.lme4)
plot(fitted(rate5.lme4), residuals(rate5.lme4),
     xlab = "Fitted Values", ylab = "Residuals")
abline(h=0, lty=2)
#lines(smooth.spline(fitted(rate5.lme4), residuals(rate5.lme4))) 
qqnorm(resid(rate5.lme4))
qqline(resid(rate5.lme4))

#fixed effects "z_pollutant","log_ppl"
#random effect "CountyName"
rate6.lme4 = lmer(rate ~ log_ppl+z_pollutant+(1|CountyName),
                  data = rate)
summary(rate6.lme4)
anova(rate6.lme4)
extractAIC(rate6.lme4)
plot(fitted(rate6.lme4), residuals(rate6.lme4),
     xlab = "Fitted Values", ylab = "Residuals")
abline(h=0, lty=2)
#lines(smooth.spline(fitted(rate6.lme4), residuals(rate6.lme4))) 
qqnorm(resid(rate6.lme4))
qqline(resid(rate6.lme4))




# temperature
air_asthma_ppl$CountyName<-tolower(air_asthma_ppl$CountyName)
temp_rate<-left_join(temperature, air_asthma_ppl, by = c("county" = "CountyName"))

temp_rate$county<-as.factor(temp_rate$county)
str(temp_rate)
summary(temp_rate)
plot(temp_rate$meantemp,temp_rate$rate)
#fixed effects "pollutant","meantemp"
#random effect "county"
temp_rate.lme4 = lmer(rate ~ pollutant+meantemp+(1|county),
                      data = temp_rate)
summary(temp_rate.lme4)
anova(temp_rate.lme4)
extractAIC(temp_rate.lme4)
plot(fitted(temp_rate.lme4), residuals(temp_rate.lme4),
     xlab = "Fitted Values", ylab = "Residuals")
abline(h=0, lty=2)
#lines(smooth.spline(fitted(temp_rate.lme4), residuals(temp_rate.lme4))) 
qqnorm(resid(temp_rate.lme4))
qqline(resid(tate.lme4))emp_r
#plot(rate$pollutant, temp_rate.lme4, main="Residual Plot\npollutant")
#abline(h=c(-2,2))



temp_rate3.lme4 = lmer(rate ~meantemp+(1|county),
                      data = temp_rate)
summary(temp_rate3.lme4)




temp_rate2<-temp_rate %>% 
  filter(!grepl('harmon', county))
temp_rate2.lme4 = lmer(rate ~ pollutant+meantemp+(1|county),
                      data = temp_rate2)
summary(temp_rate2.lme4)
plot(temp_rate2$meantemp,temp_rate2$rate)

# log rate
log_rate<-air_asthma_ppl[, c("CountyName","pollutant","population","log_rate","z_pop","z_pollutant","log_pollutant","log_ppl")]
log_rate$CountyName<-as.factor(log_rate$CountyName)
str(log_rate)
plot(log_rate)

summary(log_rate)
#fixed effects "pollutant","population"
#random effect "CountyName"
log_rate.lme4 = lmer(log_rate ~ pollutant+population+(1|CountyName),
                     data = log_rate)
summary(log_rate.lme4)
anova(log_rate.lme4)
extractAIC(log_rate.lme4)
plot(fitted(log_rate.lme4), residuals(log_rate.lme4),
     xlab = "Fitted Values", ylab = "Residuals")
abline(h=0, lty=2)
#lines(smooth.spline(fitted(log_rate.lme4), residuals(log_rate.lme4))) 
qqnorm(resid(log_rate.lme4))
qqline(resid(log_rate.lme4))



#fixed effects "z_pop","pollutant"
log_rate2.lme4 = lmer(log_rate ~ pollutant+z_pop+(1|CountyName),
                      data = log_rate)
summary(log_rate2.lme4)
anova(log_rate2.lme4)
extractAIC(log_rate2.lme4)
plot(fitted(log_rate2.lme4), residuals(log_rate2.lme4),
     xlab = "Fitted Values", ylab = "Residuals")
abline(h=0, lty=2)
#lines(smooth.spline(fitted(log_rate2.lme4), residuals(log_rate2.lme4))) 
qqnorm(resid(log_rate2.lme4))
qqline(resid(log_rate2.lme4))

#fixed effects "pollutant","log_ppl"
log_rate3.lme4 = lmer(log_rate ~ pollutant+log_ppl+(1|CountyName),
                      data = log_rate)
summary(log_rate3.lme4)
anova(log_rate3.lme4)
extractAIC(log_rate3.lme4)
plot(fitted(log_rate3.lme4), residuals(log_rate3.lme4),
     xlab = "Fitted Values", ylab = "Residuals")
abline(h=0, lty=2)
#lines(smooth.spline(fitted(log_rate3.lme4), residuals(log_rate3.lme4))) 
qqnorm(resid(log_rate3.lme4))
qqline(resid(log_rate3.lme4))


# remove Harmon county
log_rate2<-log_rate %>% 
  filter(!grepl('harmon', CountyName))

# log rate2
str(log_rate2)
plot(log_rate2)
summary(log_rate2)
#fixed effects "pollutant","population"
#random effect "CountyName"
log_rate_without_Harmon.lme4 = lmer(log_rate ~ pollutant+population+(1|CountyName),
                                    data = log_rate2)
summary(log_rate_without_Harmon.lme4)
anova(log_rate_without_Harmon.lme4)
extractAIC(log_rate_without_Harmon.lme4)
plot(fitted(log_rate_without_Harmon.lme4), residuals(log_rate_without_Harmon.lme4),
     xlab = "Fitted Values", ylab = "Residuals")
abline(h=0, lty=2)
#lines(smooth.spline(fitted(log_rate.lme4), residuals(log_rate.lme4))) 
qqnorm(resid(log_rate_without_Harmon.lme4))
qqline(resid(log_rate_without_Harmon.lme4))

#fixed effects "z_pop","z_pollutant"
log_rate_without_Harmon2.lme4 = lmer(log_rate ~ z_pollutant+z_pop+(1|CountyName),
                                     data = log_rate2)
summary(log_rate_without_Harmon2.lme4)
anova(log_rate_without_Harmon2.lme4)
extractAIC(log_rate_without_Harmon2.lme4)
plot(fitted(log_rate_without_Harmon2.lme4), residuals(log_rate_without_Harmon2.lme4),
     xlab = "Fitted Values", ylab = "Residuals")
abline(h=0, lty=2)
#lines(smooth.spline(fitted(log_rate2.lme4), residuals(log_rate2.lme4))) 
qqnorm(resid(log_rate_without_Harmon2.lme4))
qqline(resid(log_rate_without_Harmon2.lme4))

#fixed effects "log_pollutant","log_ppl"
log_rate_without_Harmon3.lme4 = lmer(log_rate ~ log_pollutant+log_ppl+(1|CountyName),
                                     data = log_rate2)
summary(log_rate_without_Harmon3.lme4)
anova(log_rate_without_Harmon3.lme4)
extractAIC(log_rate_without_Harmon3.lme4)
plot(fitted(log_rate_without_Harmon3.lme4), residuals(log_rate_without_Harmon3.lme4),
     xlab = "Fitted Values", ylab = "Residuals")
abline(h=0, lty=2)
#lines(smooth.spline(fitted(log_rate3.lme4), residuals(log_rate3.lme4))) 
qqnorm(resid(log_rate_without_Harmon3.lme4))
qqline(resid(log_rate_without_Harmon3.lme4))




# remove Harmon county
rate2<-rate %>% 
  filter(!grepl('harmon', CountyName))
# rate2
str(rate2)
plot(rate2)
summary(rate2)
#fixed effects "pollutant","population"
#random effect "CountyName"
rate2_without_Harmon.lme4 = lmer(rate ~ pollutant+population+(1|CountyName),
                                 data = rate2)
summary(rate2_without_Harmon.lme4)
anova(rate2_without_Harmon.lme4)
extractAIC(rate2_without_Harmon.lme4)
plot(fitted(rate2_without_Harmon.lme4), residuals(rate2_without_Harmon.lme4),
     xlab = "Fitted Values", ylab = "Residuals")
abline(h=0, lty=2)
#lines(smooth.spline(fitted(rate2_without_Harmon.lme4), residuals(rate2_without_Harmon.lme4))) 
qqnorm(resid(rate2_without_Harmon.lme4))
qqline(resid(rate2_without_Harmon.lme4))

#fixed effects "z_pop","z_pollutant"
rate2_without_Harmon2.lme4 = lmer(rate ~ z_pollutant+z_pop+(1|CountyName),
                                  data = rate2)
summary(rate2_without_Harmon2.lme4)
anova(rate2_without_Harmon2.lme4)
extractAIC(rate2_without_Harmon2.lme4)
plot(fitted(rate2_without_Harmon2.lme4), residuals(rate2_without_Harmon2.lme4),
     xlab = "Fitted Values", ylab = "Residuals")
abline(h=0, lty=2)
#lines(smooth.spline(fitted(rate2_without_Harmon2.lme4), residuals(rate2_without_Harmon2.lme4))) 
qqnorm(resid(rate2_without_Harmon2.lme4))
qqline(resid(rate2_without_Harmon2.lme4))

#fixed effects "log_pollutant","log_ppl"
rate_without_Harmon3.lme4 = lmer(rate ~ log_pollutant+log_ppl+(1|CountyName),
                                 data = rate2)
summary(rate_without_Harmon3.lme4)
anova(rate_without_Harmon3.lme4)
extractAIC(rate_without_Harmon3.lme4)
plot(fitted(rate_without_Harmon3.lme4), residuals(rate_without_Harmon3.lme4),
     xlab = "Fitted Values", ylab = "Residuals")
abline(h=0, lty=2)
#lines(smooth.spline(fitted(rate_without_Harmon3.lme4), residuals(rate_without_Harmon3.lme4))) 
qqnorm(resid(rate_without_Harmon3.lme4))
qqline(resid(rate_without_Harmon3.lme4))

#compare with/without Harmon
par(mfrow=c(2,2)) 
plot(rate$rate,rate$log_ppl)
plot(rate2$rate,rate2$log_ppl)
plot(rate$rate,rate$log_pollutant)
plot(rate2$rate,rate2$log_pollutant)

#### Load and tidy data ####

setwd("S:/Science/Shared/JonesO/Musquash/Working Directories/MARFIS")
MARFIS <- data.table(read.csv("R Workspace/Musquash_MARFIS_fisheriesdata_23JUL2018.csv"))
MARFIS_COMMUNITIES <- data.table(read.csv("R Workspace/MARFIS_MUSQUASH_COMMUNITIES.csv"))

#librarys 

library(dplyr)
library(lubridate)
library(broom)
library(car)
library(ggplot2)
library(data.table)
library(mgcv)
require(MASS)
library(glmm)

#tidy

MARFIS$fYEAR <- factor(MARFIS$YEAR)
MARFIS$SEASON <- as.factor(MARFIS$SEASON)
MARFIS$DATE<- dmy(MARFIS$DATE)
MARFIS$MONTH <- as.factor(MARFIS$MONTH)
MARFIS$FISHING.AREA <- as.factor(MARFIS$FISHING.AREA)
MARFIS$TRIP.ID <- as.factor(MARFIS$TRIP.ID)
MARFIS$doy <- lubridate::yday(MARFIS$DATE)
MARFIS$nrow <- nrow(MARFIS)

MARFIS_COMMUNITIES$fYEAR <- factor(MARFIS_COMMUNITIES$YEAR)
MARFIS_COMMUNITIES$SEASON <- as.factor(MARFIS_COMMUNITIES$SEASON)
MARFIS_COMMUNITIES$MONTH <- as.factor(MARFIS_COMMUNITIES$MONTH)
MARFIS_COMMUNITIES$FISHING.AREA <- as.factor(MARFIS_COMMUNITIES$FISHING.AREA)
MARFIS_COMMUNITIES$TRIP.ID <- as.factor(MARFIS_COMMUNITIES$TRIP.ID)
MARFIS_COMMUNITIES$doy <- lubridate::yday(MARFIS_COMMUNITIES$DATE)
MARFIS_COMMUNITIES$DATE <- as.character(MARFIS_COMMUNITIES$DATE)
MARFIS_COMMUNITIES$DATE <- ymd(MARFIS_COMMUNITIES$DATE)

#### exploratory graphics ####

ggplot(MARFIS,(aes( x = DATE, y = WEIGHT))) + geom_smooth(method = "loess", colour = "black") + 
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) + 
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") + 
  xlab("Date") + 
  ylab("Weight")
  
ggplot(subset(MARFIS, SPECIES.LANDED == "HADDOCK" | SPECIES.LANDED == "WINTER FLOUNDER" | SPECIES.LANDED == "COD" | SPECIES.LANDED == "SCULPIN" | SPECIES.LANDED == "POLLOCK" | SPECIES.LANDED == "GREYSOLE/WITCH"), (aes( x = DATE, y = WEIGHT))) + 
  facet_wrap(~SPECIES.LANDED) + geom_smooth(method = "loess", colour = "black") + 
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) + 
  scale_x_date(date_labels = "%y", date_breaks = "2 years") + 
  xlab("Year") + 
  ylab("Weight")
  
ggplot(subset(MARFIS_COMMUNITIES, SPECIES.LANDED == "LOBSTER" | SPECIES.LANDED == "SEA URCHINS" | SPECIES.LANDED == "SCALLOP, SEA" | SPECIES.LANDED == "CRAB, JONAH"), (aes( x = DATE, y = WEIGHT))) + 
  facet_wrap(~SPECIES.LANDED) + geom_smooth(method = "loess", colour = "black") +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) + 
  scale_x_date(date_labels = "%y", date_breaks = "2 years") + 
  xlab("Year") + 
  ylab("Weight")

ggplot(MARFIS, aes(x = COMMUNITY, y = WEIGHT)) + geom_bar(stat = "summary", fun.y = "mean")


a <- MARFIS %>%
  group_by(COMMUNITY) %>%
  summarise(sum = mean(WEIGHT))

#create a table summarising catch per species over year

PER_TRIP <- MARFIS %>%
  group_by(TRIP.ID, COMMUNITY, SPECIES.LANDED, LAT, LONG, YEAR, fYEAR, doy) %>%
  summarise(WEIGHT = sum(WEIGHT))

#### GLM ####

m0 <- gam(WEIGHT ~ SPECIES.LANDED + YEAR + doy + TRIP.ID + COMMUNITY + s(LAT) + s(LONG), MARFIS, family = "poisson")
m1 <- gam(WEIGHT ~ SPECIES.LANDED + YEAR + doy + COMMUNITY + s(LAT, LONG), MARFIS, family = "poisson")
m2 <- gam(WEIGHT ~ SPECIES.LANDED + fYEAR + COMMUNITY, a, family = "poisson")

summary(m2)

# add residuals

a$FIT = predict.glm(m2, type="response")
a$RES = residuals.glm(m2, type="pearson")

# check residuals

ggplot(MARFIS, aes(x=SPECIES.LANDED, y=RES, group=1)) + geom_jitter() + geom_smooth(method="loess")
ggplot(MARFIS, aes(x=YEAR, y=FIT, group=1)) + geom_jitter() + geom_smooth(method="loess")
ggplot(MARFIS, aes(x=doy, y=RES, group=1)) + geom_jitter() + geom_smooth(method="loess")
ggplot(MARFIS, aes(x=COMMUNITY, y=RES, group=1)) + geom_jitter() + geom_smooth(method="loess")
ggplot(MARFIS, aes(x=LAT, y=RES, group=1)) + geom_jitter() + geom_smooth(method="loess")
ggplot(MARFIS, aes(x=LONG, y=RES, group=1)) + geom_jitter() + geom_smooth(method="loess")
ggplot(a, aes(x=FIT, y=RES, group=1)) + geom_jitter() + geom_smooth(method="loess")


#expand grid

df1 =   expand.grid(YEAR = levels(a$fYEAR),
                    SPECIES.LANDED = levels(a$SPECIES.LANDED),
                    COMMUNITY = levels(a$COMMUNITY))

a$FIT = predict(m2, newdata=a, type="response", se.fit = FALSE)
a$SE = predict(m2, newdata=a, type="response", se.fit = TRUE)$se


#plot GAM

ggplot(a, aes(x=fYEAR, y=FIT)) + geom_point(stat = "summary", fun.y = "mean")


#### Top 5 GLM ####

#filter haddock

TOP_FIVE <- MARFIS %>%
  filter(SPECIES.LANDED == "HADDOCK" | SPECIES.LANDED == "WINTER FLOUNDER"| SPECIES.LANDED == "HERRING"| SPECIES.LANDED == "SCULPIN"| SPECIES.LANDED == "COD") %>%
  droplevels()

m0 <- gam(WEIGHT ~ fYEAR, TOP_FIVE, family = "poisson")
m1 <- gam(WEIGHT ~ fYEAR + s(doy), TOP_FIVE, family = "poisson")
m2 <- gam(WEIGHT ~ fYEAR + s(doy) + SPECIES.LANDED, TOP_FIVE, family = "poisson")
m3 <- gam(WEIGHT ~ fYEAR + s(doy) + SPECIES.LANDED + s(LAT) + s(LONG), TOP_FIVE, family = "poisson")
m4 <- gam(WEIGHT ~ fYEAR + s(doy) + SPECIES.LANDED + s(LAT) + s(LONG) + COMMUNITY, TOP_FIVE, family = "poisson")

summary(m4)

plot(m1,pages=1,residuals = T,pch='.',cex=0.5,all.terms = T  )

# 2 by 2 correlation

cor(MARFIS_HADDOCK[,c("YEAR","doy","LAT", "LONG", "VALUE")])

# add residuals

MARFIS_HADDOCK$FIT = predict.glm(m4, type="response")
MARFIS_HADDOCK$RES = residuals.glm(m4, type="pearson")

# check for patterns in the residuals

ggplot(MARFIS_HADDOCK, aes(x=fYEAR, y=RES)) + geom_jitter(aes(col = SPECIES.LANDED)) + geom_smooth()
ggplot(MARFIS_HADDOCK, aes(x=doy, y=FIT, group=1)) + geom_jitter() + geom_smooth(method="loess")
ggplot(MARFIS_HADDOCK, aes(x=LAT, y=RES, group=1)) + geom_jitter() + geom_smooth(method="loess")
ggplot(MARFIS_HADDOCK, aes(x=LONG, y=RES, group=1)) + geom_jitter() + geom_smooth(method="loess")
ggplot(MARFIS_HADDOCK, aes(x=VALUE, y=RES, group=1)) + geom_jitter() + geom_smooth(method="loess")


#plot main effects

df1 = expand.grid(fYEAR = levels(TOP_FIVE$fYEAR),
                  doy = mean(TOP_FIVE$doy),
                  LAT = mean(TOP_FIVE$LAT),
                  LONG = mean(TOP_FIVE$LONG),
                  COMMUNITY = levels(TOP_FIVE$COMMUNITY),
                  SPECIES.LANDED = levels(TOP_FIVE$SPECIES.LANDED))

df1$FIT = predict(m4, newdata=df1, type="response", se.fit = FALSE)
df1$SE = predict(m4, newdata=df1, type="response", se.fit = TRUE)$se

#plot catch abundance over year

ggplot(df1, aes(x = as.numeric(as.character(fYEAR)), y = FIT)) + 
        geom_jitter() +
        theme_bw() +
        coord_cartesian(ylim = c(0,5000)) +
        geom_smooth()



ggplot(df1, aes(x=fYEAR, y = FIT, col = SPECIES.LANDED)) + geom_point(stat = "summary", fun.y = "mean") + theme_bw()
ggplot(df1, aes(x=fYEAR, y = FIT, col = SPECIES.LANDED)) + geom_point(stat = "summary", fun.y = "mean") + geom_smooth(aes(col = SPECIES.LANDED))


#### TOP_FIVE_COMMUNITY ####

TOP_FIVE_COMMUNITIES <- MARFIS_COMMUNITIES %>%
  filter(SPECIES.LANDED == "LOBSTER" | SPECIES.LANDED == "SCALLOP, SEA"| SPECIES.LANDED == "SEA URCHINS"| SPECIES.LANDED == "PERIWINKLES") %>%
  droplevels()

m5 <- gam(WEIGHT ~ YEAR, TOP_FIVE_COMMUNITIES, family = "poisson")
m6 <- gam(WEIGHT ~ fYEAR + s(doy), TOP_FIVE_COMMUNITIES, family = "poisson")
m7 <- gam(WEIGHT ~ fYEAR + s(doy) + SPECIES.LANDED, TOP_FIVE_COMMUNITIES, family = "poisson")
m8 <- gam(WEIGHT ~ fYEAR + s(doy) + SPECIES.LANDED + COMMUNITY, TOP_FIVE_COMMUNITIES, family = "poisson")

summary(m8)

TOP_FIVE_COMMUNITIES$FIT = predict.glm(m8, type="response")
TOP_FIVE_COMMUNITIES$RES = residuals.glm(m8, type="pearson")

ggplot(TOP_FIVE_COMMUNITIES, aes(x=fYEAR, y=RES)) + geom_jitter(aes(col = SPECIES.LANDED)) + geom_smooth()
ggplot(TOP_FIVE_COMMUNITIES, aes(x=doy, y=FIT, group=1)) + geom_jitter() + geom_smooth(method="loess")
ggplot(TOP_FIVE_COMMUNITIES, aes(x=VALUE, y=RES, group=1)) + geom_jitter() + geom_smooth(method="loess")
ggplot(TOP_FIVE_COMMUNITIES, aes(x=RES, y=FIT, group=1)) + geom_jitter() + geom_smooth(method="loess")

df2 = expand.grid(fYEAR = levels(TOP_FIVE_COMMUNITIES$fYEAR),
                  doy = mean(TOP_FIVE_COMMUNITIES$doy),
                  COMMUNITY = levels(TOP_FIVE_COMMUNITIES$COMMUNITY),
                  SPECIES.LANDED = levels(TOP_FIVE_COMMUNITIES$SPECIES.LANDED))

df2$FIT = predict(m8, newdata=df2, type="response", se.fit = FALSE)
df2$SE = predict(m8, newdata=df2, type="response", se.fit = TRUE)$se

#plot catch abundance over year

ggplot(df2, aes(x = as.numeric(as.character(fYEAR)), y=FIT)) +
  geom_jitter() +
  geom_smooth(col = "black") + 
  theme_bw() 

 ggplot(TOP_FIVE_COMMUNITIES, aes(x=YEAR,y=WEIGHT)) + 
  geom_point(aes(col = SPECIES.LANDED)) + 
  geom_smooth(aes(limits = col = SPECIES.LANDED))

ggplot(MARFIS_COMMUNITIES, aes(x=YEAR, y = WEIGHT)) +
  geom_jitter() + 
  geom_smooth()

####Summary table ####

MARFIS_LANDINGS <- MARFIS %>%
  group_by(TRIP.ID, DATE, YEAR) %>%
  summarise(TRIP.LAN = sum(WEIGHT)) %>%
  arrange(desc(TRIP.LAN))
  
#are total landings different every year?

summary(aov(MARFIS_LANDINGS$TRIP.LAN ~ MARFIS_LANDINGS$YEAR))
TukeyHSD(aov(MARFIS_LANDINGS$TRIP.LAN ~ MARFIS_LANDINGS$YEAR))

#yes

#plot trip landings by date

plot(MARFIS_LANDINGS$TRIP.LAN ~ MARFIS_LANDINGS$DATE, ylim = c(0,6000))
abline(lm(MARFIS_LANDINGS$TRIP.LAN ~ MARFIS_LANDINGS$DATE))
LANDINGS_TBL <- tidy(lm(MARFIS_LANDINGS$TRIP.LAN ~ MARFIS_LANDINGS$DATE))
write.csv(LANDINGS_TBL, file= "Tables/LANDINGS_TBL.csv")

# trip landings have increased from 2006 to 2018

#what are the most landed species?

MARFIS_SPECIES <- MARFIS %>%
  group_by(SPECIES.LANDED) %>%
  summarise(TOT.LAN = sum(WEIGHT)/981)%>%
  arrange(desc(TOT.LAN))

write.csv(MARFIS_SPECIES, file = "Tables/MARFIS_SPECIES.csv")

#plot number of trips per day

MARFIS_TRIPS <- MARFIS %>%
  group_by(DATE) %>%
  summarise(ntrips = length(unique(TRIP.ID))) %>%
  arrange(desc(ntrips))

plot(MARFIS_TRIPS$ntrips ~ MARFIS_TRIPS$DATE)
abline(lm(MARFIS_TRIPS$ntrips ~ MARFIS_TRIPS$DATE))
summary(lm(MARFIS_TRIPS$ntrips ~ MARFIS_TRIPS$DATE))

#what are the species landed the most in terms of frequency?

MARFIS_SPECIES_FREQUENCY <- MARFIS %>%
  group_by(SPECIES.LANDED) %>%
  summarise(ncatch = length(TRIP.ID)) %>%
  arrange(desc(ncatch))

#### Haddock ####

# create a table of landing for haddock 

MARFIS_HADDOCK <- MARFIS %>%
  filter(SPECIES.LANDED == "HADDOCK") %>%
  group_by(TRIP.ID, DATE) %>%
  summarise(TOT.LAN = sum(WEIGHT)) %>%
  arrange(desc(TOT.LAN))

# plot Haddock landings over time

plot(MARFIS_HADDOCK$TOT.LAN ~ MARFIS_HADDOCK$DATE, ylim = c(0,4000))
abline(lm(MARFIS_HADDOCK$TOT.LAN ~ MARFIS_HADDOCK$DATE))  
summary(lm(MARFIS_HADDOCK$TOT.LAN ~ MARFIS_HADDOCK$DATE))

#haddock landings are increasing

#### Winter founder ####

# create a table of landing for winter flounder 

MARFIS_WF <- MARFIS %>%
  filter(SPECIES.LANDED == "WINTER FLOUNDER") %>%
  group_by(TRIP.ID, DATE) %>%
  summarise(TOT.LAN = sum(WEIGHT)) %>%
  arrange(desc(TOT.LAN))

#plot landings over time

plot(MARFIS_WF$TOT.LAN ~ MARFIS_WF$DATE)
abline(lm(MARFIS_WF$TOT.LAN ~ MARFIS_WF$DATE))  
summary(lm(MARFIS_WF$TOT.LAN ~ MARFIS_WF$DATE))

#### Scallop ####

MARFIS_SCALLOP <- MARFIS %>%
  filter(SPECIES.LANDED == "SCALLOP, SEA") %>%
  group_by(TRIP.ID, DATE) %>%
  summarise(TOT.LAN = sum(WEIGHT)) %>%
  arrange(desc(TOT.LAN))

#plot landings over time

plot(MARFIS_SCALLOP$TOT.LAN ~ MARFIS_SCALLOP$DATE)
abline(lm(MARFIS_SCALLOP$TOT.LAN ~ MARFIS_SCALLOP$DATE))  
summary(lm(MARFIS_SCALLOP$TOT.LAN ~ MARFIS_SCALLOP$DATE))

#Scallop landings are increasing

####Sea urchins ####

MARFIS_Urchins <- MARFIS %>%
  filter(SPECIES.LANDED == "SEA URCHINS") %>%
  group_by(TRIP.ID, DATE) %>%
  summarise(TOT.LAN = sum(WEIGHT)) %>%
  arrange(desc(TOT.LAN))

#plot landings over time

plot(MARFIS_Urchins$TOT.LAN ~ MARFIS_Urchins$DATE)
abline(lm(MARFIS_Urchins$TOT.LAN ~ MARFIS_Urchins$DATE))  
summary(lm(MARFIS_Urchins$TOT.LAN ~ MARFIS_Urchins$DATE))

#### Herring ####

MARFIS_Herring <- MARFIS %>%
  filter(SPECIES.LANDED == "HERRING") %>%
  group_by(TRIP.ID, DATE) %>%
  summarise(TOT.LAN = sum(WEIGHT)) %>%
  arrange(desc(TOT.LAN))

#plot landings over time

plot(MARFIS_Herring$TOT.LAN ~ MARFIS_Herring$DATE)
abline(lm(MARFIS_Herring$TOT.LAN ~ MARFIS_Herring$DATE))  
summary(lm(MARFIS_Herring$TOT.LAN ~ MARFIS_Herring$DATE))

####Pollock####

MARFIS_Pollock <- MARFIS %>%
  filter(SPECIES.LANDED == "POLLOCK") %>%
  group_by(TRIP.ID, DATE) %>%
  summarise(TOT.LAN = sum(WEIGHT)) %>%
  arrange(desc(TOT.LAN))

plot(MARFIS_Pollock$TOT.LAN ~ MARFIS_Pollock$DATE, ylim = c(0,500))
abline(lm(MARFIS_Pollock$TOT.LAN ~ MARFIS_Pollock$DATE))
summary(lm(MARFIS_Pollock$TOT.LAN ~ MARFIS_Pollock$DATE))

#### Cod ####

MARFIS_Cod <- MARFIS %>%
  filter(SPECIES.LANDED == "COD") %>%
  group_by(TRIP.ID, DATE) %>%
  summarise(TOT.LAN = sum(WEIGHT)) %>%
  arrange(desc(TOT.LAN))

plot(MARFIS_Cod$TOT.LAN ~ MARFIS_Cod$DATE, ylim = c(0,500))
abline(lm(MARFIS_Cod$TOT.LAN ~ MARFIS_Cod$DATE))
summary(lm(MARFIS_Cod$TOT.LAN ~ MARFIS_Cod$DATE))

#### Sculpin ####

MARFIS_Sculpin <- MARFIS %>%
  filter(SPECIES.LANDED == "SCULPIN") %>%
  group_by(TRIP.ID, DATE) %>%
  summarise(TOT.LAN = sum(WEIGHT)) %>%
  arrange(desc(TOT.LAN))

plot(MARFIS_Sculpin$TOT.LAN ~ MARFIS_Sculpin$DATE)
abline(lm(MARFIS_Sculpin$TOT.LAN ~ MARFIS_Sculpin$DATE))
summary(lm(MARFIS_Sculpin$TOT.LAN ~ MARFIS_Sculpin$DATE))

#### White hake ####

MARFIS_Hake <- MARFIS %>%
  filter(SPECIES.LANDED == "WHITE HAKE") %>%
  group_by(TRIP.ID, DATE) %>%
  summarise(TOT.LAN = sum(WEIGHT)) %>%
  arrange(desc(TOT.LAN))

plot(MARFIS_Hake$TOT.LAN ~ MARFIS_Hake$DATE, ylim = c(0, 200))
abline(lm(MARFIS_Hake$TOT.LAN ~ MARFIS_Hake$DATE))
summary(lm(MARFIS_Hake$TOT.LAN ~ MARFIS_Hake$DATE))

### Halibut ####

MARFIS_Halibut <- MARFIS %>%
  filter(SPECIES.LANDED == "HALIBUT") %>%
  group_by(TRIP.ID, DATE) %>%
  summarise(TOT.LAN = sum(WEIGHT)) %>%
  arrange(desc(TOT.LAN))

plot(MARFIS_Halibut$TOT.LAN ~ MARFIS_Halibut$DATE, ylim = c(0,200))
abline(lm(MARFIS_Halibut$TOT.LAN ~ MARFIS_Halibut$DATE))
summary(lm(MARFIS_Halibut$TOT.LAN ~ MARFIS_Halibut$DATE))

#### Community Data ####

#tidy date

MARFIS_COMMUNITIES$DATE <- ymd(MARFIS_COMMUNITIES$DATE)

#make table of landings per recording

COM.TRIP <- MARFIS_COMMUNITIES %>%
  group_by(SPECIES.LANDED, DATE, YEAR, NROW) %>%
  summarise(TRIP.LAN = sum(WEIGHT)) %>%
  arrange(desc(TRIP.LAN))

#plot landings over time

plot(COM.TRIP$TRIP.LAN ~ COM.TRIP$DATE)
abline(lm(COM.TRIP$TRIP.LAN ~ COM.TRIP$DATE))  
COM_LANDINGS_TBL <- tidy(lm(COM.TRIP$TRIP.LAN ~ COM.TRIP$DATE))
write.csv(COM_LANDINGS_TBL, file = "Tables/COM_LANDINGS_TBL.csv")

#top species

MARFIS.COM.SPECIES <- MARFIS_COMMUNITIES %>%
  group_by(SPECIES.LANDED) %>%
  summarise(TOT.LAN = sum(WEIGHT)) %>%
  arrange(desc(TOT.LAN))

write.csv(MARFIS.COM.SPECIES, file = "Tables/MARFIS.COM.SPECIES.csv")
####Lobster####

MARFIS_LOBSTER <- MARFIS_COMMUNITIES %>%
  filter(SPECIES.LANDED == "LOBSTER") %>%
  group_by(NROW, DATE) %>%
  summarise(TOT.LAN = sum(WEIGHT)/2493) %>%
  arrange(desc(TOT.LAN))

plot(MARFIS_LOBSTER$TOT.LAN ~ MARFIS_LOBSTER$DATE, ylim = c(0,4))
abline(lm(MARFIS_LOBSTER$TOT.LAN ~ MARFIS_LOBSTER$DATE))  
summary(lm(MARFIS_LOBSTER$TOT.LAN ~ MARFIS_LOBSTER$DATE))

####Rockweed####

MARFIS_ROCKWEED <- MARFIS_COMMUNITIES %>%
  filter(SPECIES.LANDED == "ROCKWEED") %>%
  group_by(NROW, DATE) %>%
  summarise(TOT.LAN = sum(WEIGHT)) %>%
  arrange(desc(TOT.LAN))

plot(MARFIS_ROCKWEED$TOT.LAN ~ MARFIS_ROCKWEED$DATE)
abline(lm(MARFIS_ROCKWEED$TOT.LAN ~ MARFIS_ROCKWEED$DATE))
summary(lm(MARFIS_ROCKWEED$TOT.LAN ~ MARFIS_ROCKWEED$DATE))

#### Sea Urchins ####

MARFIS_COM_URCHINS <- MARFIS_COMMUNITIES %>%
  filter(SPECIES.LANDED == "SEA URCHINS") %>%
  group_by(NROW, DATE) %>%
  summarise(TOT.LAN = sum(WEIGHT)) %>%
  arrange(desc(TOT.LAN))

plot(MARFIS_COM_URCHINS$TOT.LAN ~ MARFIS_COM_URCHINS$DATE)
abline(lm(MARFIS_COM_URCHINS$TOT.LAN ~ MARFIS_COM_URCHINS$DATE))
summary(lm(MARFIS_COM_URCHINS$TOT.LAN ~ MARFIS_COM_URCHINS$DATE))

MARFIS %>%
  filter(SPECIES.LICENSE == "GROUNDFISH, UNSPECIFIED") %>%
  summarise(n_distinct(TRIP.ID))

INVERT <- MARFIS_COMMUNITIES %>%
  group_by(SPECIES.LANDED, YEAR) %>%
  summarise(WEIGHT = sum(WEIGHT))


#### Install packages ####

install.packages('lubridate')
install.packages("car")
install.packages("broom")

#### Load librarys####

library(dplyr)
library(lubridate)
library(broom)
library(car)
library(ggplot2)
library(data.table)
library(mgcv)
require(MASS)
library(glmm)

#### Load data ####

setwd("S:/Science/Shared/JonesO/Musquash/Working Directories/Musquash Benthic Baseline")
macroinfauna <- read.csv('Raw Data/MusquashMPA_Macroinfauna.csv')
MGS <- read.csv('Raw Data/Musquash_GS.csv')

#### Tidy data ####

# Uniform species names

macroinfauna$LOWEST_TAXON <- dplyr::recode(macroinfauna$LOWEST_TAXON, "Amathia spp" = "Amathia sp.",
                                    "Amphibalanus spp." = "Amphibalanus sp.",
                                    "Aphelocaeta spp" = "Aphelocaeta sp.",
                                    "Aphelochaeta spp"= "Aphelocaeta sp.",
                                    "Arctica islandica " ="Arctcica islandica",
                                    "Barentsia spp" = "Barentsia sp.",
                                    "Barentsia spp." = "Barentsia sp.",
                                    "Bipalponephtys cornuta " = "Bipalponephtys cornuta",
                                    "Cerebratulus spp" = "Cerebratulus sp.",
                                    "Cirratulidae " = "Cirratulidae",
                                    "Golfingia sp" = "Golfingia sp.",
                                    "Kirkegaardia spp" = "Kirkegaardia sp.",
                                    "Leitoscoloplos fragilis " = "Leitoscoloplos fragilis",
                                    "Lumbrineridae " = "Lumbrineridae",
                                    "Nereididae " = "Nereididae",
                                    "Phoronis sp" = "Phoronis sp.",
                                    "Phoronis spp" = "Phoronis sp.",
                                    "Pycnophyes spp" = "Pycnophyes spp.",
                                    "Tharyx spp" = "Tharyx sp.")

macroinfauna$DATE <- dmy(macroinfauna$DATE)
macroinfauna$LOWEST_TAXON <- as.character(macroinfauna$LOWEST_TAXON)
macroinfauna <- macroinfauna[-2585,]
macroinfauna <- mutate(macroinfauna, ind_per_kg = TOTAL_COUNT/(WEIGHT/1000))
macroinfauna$doy <- lubridate::yday(macroinfauna$DATE)
macroinfauna = data.table(macroinfauna)
macroinfauna$fYEAR = factor(macroinfauna$YEAR)
macroinfauna$LOWEST_TAXON <- as.factor(macroinfauna$LOWEST_TAXON)

#### explore data ####

ggplot(macroinfauna, (aes(x = DATE, y = TOTAL_COUNT))) + geom_point()
ggplot(macroinfauna, (aes(x = doy, y = TOTAL_COUNT))) + geom_point() + geom_smooth(method = "loess")

#plot 1

ggplot(macroinfauna, (aes(x = DATE, y = TOTAL_COUNT))) + geom_jitter(shape = 1) +
  geom_smooth(method = "loess", colour = "black") +
  coord_cartesian(ylim = c(0, 150)) + 
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) + 
  ylab("Abundance") + 
  xlab("Date")

#plot 2

t1 <- c(
  "channel" = "Channel",
  "intertidal" = "Intertidal",
  "subtidal" = "Subtidal"
)

ggplot(macroinfauna, (aes(x = DATE, y = TOTAL_COUNT))) + geom_jitter(shape = 1) +
  geom_smooth(method = "loess", colour = "black") +
  coord_cartesian(ylim = c(0, 275)) +
  theme_bw() + 
  facet_wrap(~STRATA, labeller = labeller(STRATA = t1)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) + 
  ylab("Abundance") + 
  xlab("Date")

#species richness

species_richness <- macroinfauna %>%
  group_by(DATE, YEAR, SEASON, STRATA, doy) %>%
  summarise(SR = n_distinct(LOWEST_TAXON))

ggplot(species_richness, aes(x = DATE, y = SR)) + geom_point(shape = 1) + geom_smooth(col = "black") + facet_wrap(~STRATA) + 
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) + 
  xlab("Date") + 
  ylab("Species Richness")
  

# Species abundance

ggplot(subset(macroinfauna, LOWEST_TAXON == "Cossura longocirrata" | LOWEST_TAXON == "Streblospio benedicti" | LOWEST_TAXON == "Levinsenia gracilis" | LOWEST_TAXON == "Nucula proxima" | LOWEST_TAXON == "Bipalponephtys cornuta" | LOWEST_TAXON == "Pygospio elegans"), aes(x = DATE, y = ind_per_kg)) +
  geom_jitter(shape = 1) + geom_smooth(method = "loess", colour = "black") + facet_wrap(~LOWEST_TAXON) + coord_cartesian(ylim = c(0,175)) +
  ylab("Abundance") + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

ggplot(subset(df1, LOWEST_TAXON == "Cossura longocirrata" | LOWEST_TAXON == "Streblospio benedicti" | LOWEST_TAXON == "Levinsenia gracilis" | LOWEST_TAXON == "Nucula proxima" | LOWEST_TAXON == "Bipalponephtys cornuta" | LOWEST_TAXON == "Pygospio elegans"), aes(x = DATE, y = FIT)) + geom_jitter() + facet_wrap(~LOWEST_TAXON) + coord_cartesian(ylim = c(0,200))

#### build GAM ####

m0 <- gam(TOTAL_COUNT ~ WEIGHT + fYEAR + STRATA + DATE, macroinfauna, family = poisson)

summary(m0)

#### plot residuals ####

macroinfauna$RES = residuals.glm(m0, type="pearson")
macroinfauna$FIT = predict.glm(m0, type="response")

ggplot(macroinfauna, aes(x=fYEAR, y=RES, group=1)) + geom_jitter() + geom_smooth(method="loess")
ggplot(macroinfauna, aes(x=STRATA, y=RES, group=1)) + geom_jitter() + geom_smooth(method="loess")
ggplot(macroinfauna, aes(x=SEASON, y=RES, group=1)) + geom_jitter() + geom_smooth(method="loess")
ggplot(macroinfauna, aes(x=WEIGHT, y=RES)) + geom_jitter() + geom_smooth(method="loess")
ggplot(macroinfauna, aes(x=TOTAL_COUNT, y=FIT, group=1)) + geom_jitter() + geom_smooth(method="lm")
ggplot(macroinfauna, aes(x=FIT, y=RES, group=1)) + geom_jitter() + geom_smooth(method="loess")

#create dataframe for model fit

df1 = expand.grid(fYEAR = levels(macroinfauna$fYEAR),
                  STRATA =  levels(macroinfauna$STRATA),
                  WEIGHT = mean(macroinfauna$WEIGHT),
                  DATE = macroinfauna$DATE)

df1$FIT = predict(m0, newdata=df1, type="response", se.fit = FALSE)
df1$SE = predict(m0, newdata=df1, type="response", se.fit = TRUE)$se

#### plot model ####

ggplot(df1, aes(x = as.numeric(as.character(fYEAR)), y = FIT)) + geom_point() + 
  geom_smooth() +
  facet_wrap(~ STRATA) +
  geom_linerange(aes(ymin = FIT - 1.96*SE, ymax = FIT + 1.96*SE)) + 
  theme_bw()

ggplot(df1, aes(x = as.numeric(as.character(fYEAR)), y = FIT)) + geom_point() + geom_smooth()

# add zero counts

df1 = data.table(expand.grid(SET_ID = unique(macroinfauna$SET_ID),
                             LOWEST_TAXON = unique(macroinfauna$LOWEST_TAXON),
                             TOTAL_COUNT = 0))

df2 = unique(data.table(subset(macroinfauna, 
                               select=c(SEASON,fYEAR,STRATA,SET_ID,WEIGHT,doy, DATE))))

df3 = unique(data.table(subset(macroinfauna, 
                               select=c(SET_ID,LOWEST_TAXON,TOTAL_COUNT))))


df2 = merge(df1, df2, all.x=T)

df3 = merge(df2, df3, all.x=T, by=c("SET_ID","LOWEST_TAXON"))
df3[, TOTAL_COUNT := ifelse(is.na(TOTAL_COUNT.y), 0, TOTAL_COUNT.y)]
df3[, ":=" (TOTAL_COUNT.x=NULL, TOTAL_COUNT.y=NULL)]
df3[,TAXA := factor(LOWEST_TAXON)]

#### explore data again

ggplot(df3, (aes(x = DATE, y = TOTAL_COUNT))) + geom_point()
ggplot(df3, (aes(x = doy, y = TOTAL_COUNT))) + geom_point() + geom_smooth(method = "loess")
ggplot(df3, (aes(x = DATE, y = TOTAL_COUNT))) + geom_jitter(aes(col = STRATA)) +
  geom_smooth(aes(col = STRATA), method = "loess") +
  coord_cartesian(ylim = c(0, 300))

ggplot(df3, (aes(x = DATE, y = ind_per_kg))) + geom_jitter() +
  geom_smooth(method = "loess") +
  coord_cartesian(ylim = c(0, 100)) +
  facet_wrap(~STRATA) +
  theme_bw()


m1 = glmm(TOTAL_COUNT ~ 0 + offset(WEIGHT) + STRATA +SEASON + YEAR, random = list(~ 0 + LOWEST_TAXON), 
          data = macroinfauna, family.glmm = "poisson.glmm", varcomps.names=c("TAXA"))

m2 = gam(TOTAL_COUNT ~ s(WEIGHT,k=4) + STRATA +SEASON + fYEAR, data = macroinfauna, 
         family = "poisson")

macroinfauna$RES = residuals.glm(m2, type="pearson")
macroinfauna$FIT = predict.glm(m2, type="response")

# check for patterns in the residuals

ggplot(macroinfauna, aes(x=fYEAR, y=RES, group=1)) + geom_jitter() + geom_smooth(method="loess")
ggplot(macroinfauna, aes(x=STRATA, y=RES, group=1)) + geom_jitter() + geom_smooth(method="loess")
ggplot(macroinfauna, aes(x=SEASON, y=RES, group=1)) + geom_jitter() + geom_smooth(method="loess")
ggplot(macroinfauna, aes(x=WEIGHT, y=RES)) + geom_jitter() + geom_smooth(method="loess")
ggplot(macroinfauna, aes(x=TOTAL_COUNT, y=FIT, group=1)) + geom_jitter() + geom_smooth(method="lm")
ggplot(macroinfauna, aes(x=FIT, y=RES, group=1)) + geom_jitter() + geom_smooth(method="loess")

# should check for dispersion

plot(m2)

# Plot of main effects: Season by strata

df4 = expand.grid(SEASON = levels(macroinfauna$SEASON),
                      fYEAR = levels(macroinfauna$fYEAR),
                      STRATA =  levels(macroinfauna$STRATA),
                      WEIGHT = mean(macroinfauna$WEIGHT),
                      doy = macroinfauna$doy)

df4$FIT = predict(m2, newdata=df4, type="response", se.fit = FALSE)
df4$SE = predict(m2, newdata=df4, type="response", se.fit = TRUE)$se

ggplot(df4, aes(x=STRATA, y=FIT, col = SEASON)) + geom_point()  +
  geom_linerange(aes(ymin = FIT - 1.96*SE, ymax = FIT + 1.96*SE))

# season, strata and year

df5 = expand.grid(SEASON = levels(macroinfauna$SEASON),
                      fYEAR = levels(macroinfauna$fYEAR),
                      STRATA =  levels(macroinfauna$STRATA),
                      WEIGHT = mean(macroinfauna$WEIGHT),
                      doy = macroinfauna$doy)

df5$FIT = predict(m2, newdata=df5, type="response", se.fit = FALSE)
df5$SE = predict(m2, newdata=df5, type="response", se.fit = TRUE)$se

# channel by season over time

# plot 3

ggplot(df5, aes(x = doy, y=FIT)) + geom_jitter(shape = 1) +
  theme_bw() + geom_smooth(se = F, col= "black") +
  facet_wrap(~STRATA, labeller = labeller(STRATA = t1)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) + 
  ylab("Abundance") + 
  xlab("Date")

#plot 4

ggplot(subset(df5, LOWEST_TAXON == "Cossura longocirrata" | LOWEST_TAXON == "Streblospio benedicti" | LOWEST_TAXON == "Levinsenia gracilis" | LOWEST_TAXON == "Nucula proxima" | LOWEST_TAXON == "Bipalponephtys cornuta" | LOWEST_TAXON == "Pygospio elegans"), aes(x = DATE, y = ind_per_kg)) +
  geom_jitter() + geom_smooth(method = "loess") + facet_wrap(~LOWEST_TAXON) + coord_cartesian(ylim = c(0,200))

# season by strata over time

ggplot(df2, aes(x=fYEAR, y=FIT)) + geom_point() + facet_wrap(~STRATA) +
  geom_linerange(aes(ymin = FIT - 1.96*SE, ymax = FIT + 1.96*SE))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme_bw()

#### Zero counts ####


# m3 = glmm(TOTAL_COUNT ~ offset(WEIGHT) + STRATA +SEASON + fYEAR, 
#          random = list(~ 0 + LOWEST_TAXON), data = newdata3, 
#          varcomps.names = "TAXA", family.glmm = "binomial.glmm")

m4 = gam(TOTAL_COUNT ~ s(WEIGHT) + STRATA + SEASON + fYEAR + s(doy), data = df5, family = nb(theta = NULL, link = "log"))

summary(m4)
plot(m4)

df5$RES = residuals.gam(m4, type="pearson")
df5$SE = predict.gam(m4, type="response")

# check for patterns in the residuals

ggplot(df5, aes(x=fYEAR, y=RES, group=1)) + geom_jitter() + geom_smooth(method="loess")
ggplot(df5, aes(x=STRATA, y=RES, group=1)) + geom_jitter() + geom_smooth(method="loess")
ggplot(df5, aes(x=SEASON, y=RES, group=1)) + geom_jitter() + geom_smooth(method="loess")
ggplot(df5, aes(x=WEIGHT, y=RES)) + geom_jitter() + geom_smooth(method="loess")
ggplot(df5, aes(x=TOTAL_COUNT, y=FIT, group=1)) + geom_jitter() + geom_smooth(method="lm")
ggplot(df5, aes(x=FIT, y=RES, group=1)) + geom_jitter() + geom_smooth(method="loess")

df5$FIT = predict(m4, newdata=df5, type="response", se.fit = FALSE)
df5$SE = predict(m4, newdata=df5, type="response", se.fit = TRUE)$se

ggplot(df5, aes(x= doy, y= FIT)) + geom_point(aes(col = STRATA)) + facet_grid(~SEASON) +
  geom_linerange(aes(ymin = FIT - 1.96*SE, ymax = FIT + 1.96*SE))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_bw() +
  geom_smooth()

##### Cossura longocirrata ####

COSS <- macroinfauna %>%
  filter(LOWEST_TAXON == "Cossura longocirrata")

hist(COSS$ind_per_kg)
qqnorm(COSS$ind_per_kg)
qqline(COSS$ind_per_kg)
leveneTest(COSS$ind_per_kg, COSS$STRATA)
leveneTest(COSS$ind_per_kg, COSS$SEASON)
leveneTest(COSS$ind_per_kg, COSS$YEAR)

summary(aov(COSS$ind_per_kg ~ COSS$SEASON))
summary(aov((COSS$ind_per_kg ~ COSS$STRATA)))
summary(aov((COSS$ind_per_kg ~ COSS$YEAR)))


##### Streblospio b. ####

# create summary table

streb_abun <- macroinfauna %>%
  filter(LOWEST_TAXON == "Streblospio benedicti")

#is data normal?

hist(streb_abun$ind_per_kg)
qqnorm(streb_abun$ind_per_kg)
qqline(streb_abun$ind_per_kg)
leveneTest(streb_abun$ind_per_kg, streb_abun$STRATA)
leveneTest(streb_abun$ind_per_kg, streb_abun$SEASON)
leveneTest(streb_abun$ind_per_kg, streb_abun$YEAR)

#ANOVA between seasons

summary(aov(streb_abun$ind_per_kg ~ streb_abun$SEASON))
summary(aov(streb_abun$ind_per_kg ~ streb_abun$STRATA))
summary(aov(streb_abun$ind_per_kg ~ streb_abun$YEAR))

#### LEVI ####

levi_abun <- macroinfauna %>%
  filter(LOWEST_TAXON == "Levinsenia gracilis")

hist(levi_abun$ind_per_kg)
shapiro.test(levi_abun$ind_per_kg)
qqnorm(levi_abun$ind_per_kg)
qqline(levi_abun$ind_per_kg)
leveneTest(levi_abun$ind_per_kg, levi_abun$STRATA)
leveneTest(levi_abun$ind_per_kg, levi_abun$SEASON)
leveneTest(levi_abun$ind_per_kg, levi_abun$YEAR)

summary(aov(levi_abun$ind_per_kg ~ levi_abun$SEASON))
summary(aov(levi_abun$ind_per_kg ~ levi_abun$STRATA))
summary(aov(levi_abun$ind_per_kg ~ levi_abun$YEAR))
kruskal.test(levi_abun$ind_per_kg ~ levi_abun$YEAR)

#### Nucula proxima ####

NUCU <- macroinfauna %>%
  filter(LOWEST_TAXON == "Nucula proxima")

hist(NUCU$ind_per_kg)
qqnorm(NUCU$ind_per_kg)
qqline(NUCU$ind_per_kg)
shapiro.test(NUCU$ind_per_kg)
leveneTest(NUCU$ind_per_kg, NUCU$STRATA)
leveneTest(NUCU$ind_per_kg, NUCU$SEASON)
leveneTest(NUCU$ind_per_kg, NUCU$YEAR)

#is data normal?

summary(aov(NUCU$ind_per_kg ~ NUCU$STRATA))
summary(aov((NUCU$ind_per_kg ~ NUCU$SEASON)))

#### BIPAL ####

bipal_abun <- macroinfauna %>%
  filter(LOWEST_TAXON == "Bipalponephtys cornuta")

hist(bipal_abun$ind_per_kg)
qqnorm(bipal_abun$ind_per_kg)
qqline(bipal_abun$ind_per_kg)
shapiro.test(bipal_abun$ind_per_kg)
leveneTest(bipal_abun$ind_per_kg, bipal_abun$STRATA)
leveneTest(bipal_abun$ind_per_kg, bipal_abun$SEASON)
leveneTest(bipal_abun$ind_per_kg, bipal_abun$YEAR)

plot(bipal_abun$ind_per_kg ~ bipal_abun$DATE)
summary(lm(bipal_abun$ind_per_kg ~ bipal_abun$DATE))


##### Pygbosio #### 

pyg_abun <- macroinfauna %>%
  filter(LOWEST_TAXON == "Pygospio elegans")

hist(pyg_abun$ind_per_kg)
qqnorm(pyg_abun$ind_per_kg)
qqline(pyg_abun$ind_per_kg)
shapiro.test(pyg_abun$ind_per_kg)
leveneTest(pyg_abun$ind_per_kg, pyg_abun$STRATA)
leveneTest(pyg_abun$ind_per_kg, pyg_abun$SEASON)
leveneTest(pyg_abun$ind_per_kg, pyg_abun$YEAR)

summary(aov(pyg_abun$ind_per_kg ~ pyg_abun$SEASON))
summary(aov(pyg_abun$ind_per_kg ~ pyg_abun$STRATA))
summary(aov(pyg_abun$ind_per_kg ~ pyg_abun$YEAR))

#### Aricidea catherinae ####

aric_abun <- macroinfauna %>%
  filter(LOWEST_TAXON == "Aricidea catherinae")

hist(aric_abun$ind_per_kg)
qqnorm(aric_abun$ind_per_kg)
qqline(aric_abun$ind_per_kg)
shapiro.test(aric_abun$ind_per_kg)
leveneTest(aric_abun$ind_per_kg, aric_abun$STRATA)
leveneTest(aric_abun$ind_per_kg, aric_abun$SEASON)
leveneTest(aric_abun$ind_per_kg, aric_abun$YEAR)

#### Nephtys ciliata ####

neph_abun <- macroinfauna %>%
  filter(LOWEST_TAXON == "Nephtys ciliata")

hist(neph_abun$ind_per_kg)
qqnorm(neph_abun$ind_per_kg)
qqline(neph_abun$ind_per_kg)
shapiro.test(neph_abun$ind_per_kg)
leveneTest(neph_abun$ind_per_kg, neph_abun$STRATA)
leveneTest(neph_abun$ind_per_kg, neph_abun$SEASON)
leveneTest(neph_abun$ind_per_kg, neph_abun$YEAR)

#### STERN ###

stern_abun <- macroinfauna %>%
  filter(LOWEST_TAXON == "Sternaspis scutata")

hist(stern_abun$ind_per_kg)
qqnorm(stern_abun$ind_per_kg)
qqline(stern_abun$ind_per_kg)
shapiro.test(stern_abun$ind_per_kg)
leveneTest(stern_abun$ind_per_kg, stern_abun$STRATA)
leveneTest(stern_abun$ind_per_kg, stern_abun$SEASON)
leveneTest(stern_abun$ind_per_kg, stern_abun$YEAR)

# summmary of infauna abundance

benthic_sum <- macroinfauna %>%
  select(DATE, )


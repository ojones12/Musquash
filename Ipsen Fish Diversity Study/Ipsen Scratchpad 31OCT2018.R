#### Load packages and data ####

library(dplyr)
library(lubridate)
library(broom)
library(car)
library(ggplot2)
library(data.table)
library(mgcv)
require(MASS)
library(glmm)
library(usdm)
library(scales)

#### load Ipsen raw data ####

setwd("S:/Science/Shared/JonesO/Musquash/Working Directories/Ipsen Fish Diversity Study")
Ipsen <- data.table(read.csv("Raw Data/Erinn Ipsen Copy of fish data 30JUL2018.csv"))

#### tidy data ####

Ipsen$Date <- dmy(Ipsen$Date)

Ipsen$Species <- dplyr::recode(Ipsen$Species, "3 spine sb" = "3 spine SB",
                      "9 spine sb" = "9 spine SB",
                      "american eel" = "American eel",
                      "Black spotted sb" = "Black spotted SB",
                      "Black Spotted SB" = "Black spotted SB",
                      "smelt" = "Smelt",
                      "white hake" = "White hake", 
                      "White Hake" = "White hake",
                      "winter flounder" = "Winter flounder",
                      "Winter Flounder" = "Winter flounder",
                      "mummichog" = "Mummichog",
                      "tomcod" = "Tomcod",
                      "cod" = "Cod",
                      "herring" = "Herring",
                      "pollock" = "Pollock",
                      "smelt" = "Smelt",
                      "alewife" = "Alewife",
                      "sea raven" = "Sea raven",
                      "Sea Raven" = "Sea raven")

Ipsen$Value = 1
Ipsen$doy <- lubridate::yday(Ipsen$Date)
Ipsen[,":=" (fYear=factor(Year), fLocation=factor(Location))]
Ipsen$Length <- dplyr::recode(Ipsen$Length, "nm" = "NA")
Ipsen$Length <- as.character(Ipsen$Length)
Ipsen$Length <- as.numeric(Ipsen$Length)
Ipsen$fMonth <- as.factor(Ipsen$Month)
Ipsen$ym <- paste(Ipsen$ear, Ipsen$Month, sep = "-")
Ipsen$ym <- as.factor(Ipsen$ym)
Ipsen <- Ipsen %>%
   filter(fLocation == "BB" | fLocation == "HB" | fLocation == "FFH") %>%
    filter(Gear == "Seine" | Gear == "Fyke Net") %>%
   droplevels()
Ipsen$nrow <- seq.int(nrow(Ipsen))

hist_table <- Ipsen %>%
  group_by(Sample, Location, Date, Gear) %>%
  summarise(species_richness = n_distinct(Species))

#species richness exploration

ggplot(hist_table, aes(x = Date, y = species_richness)) + geom_jitter(aes(shape = Gear)) + scale_shape_manual(values = c(1, 3)) + geom_smooth(aes(linetype = Gear), col = "black") + facet_wrap(~Location, labeller = labeller(Location = a2)) + 
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank()) + 
  ylab("Species Richness")

ggplot(hist_table, aes(x = Date, y = species_richness)) + geom_jitter() + geom_smooth() + facet_wrap(~Gear) + facet_wrap(~fLocation)

#### add zero values ####

df0 <- data.table(expand.grid(Sample = unique(Ipsen$Sample),
                              Species = unique(Ipsen$Species),
                              Value = 0))

df1 <- unique(data.table(subset(Ipsen,
                                select = c(fYear, Year, fLocation, Sample, Gear, Season, fMonth, doy, Day, Date))))

df2 <- hist_table

df1 <- merge(df0, df1, all.x =T)

df2 = merge(df1, df2, all.x=T, by = c("Sample", "Species"))
df2[, Value := ifelse(is.na(Value.y), 0, Value.y)]
df2[, ":=" (Value.x=NULL, Value.y=NULL)]
df2[,spcd := factor(Species)]

#### exploratory data ####

# plot 1 - total abundance

ggplot(hist_table, aes(x = Date, y = Value)) + 
  geom_jitter(col = "black", size = 1, shape = 1)+
  geom_smooth(method = "loess", col = "black", size = 1)+
  coord_cartesian(ylim = c(0,60)) +
  theme_bw() +
  ylab("Abundance") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b-%y") + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

#plot 2 - gear 

ggplot(hist_table, aes(x = Date, y = Value)) + 
  geom_jitter(col = "black", size = 1, shape = 1)+
  geom_smooth(method = "loess", col = "black", size = 1)+
  coord_cartesian(ylim = c(0,75)) +
  theme_bw() +
  ylab("Abundance") + 
  scale_x_date(date_breaks = "6 months", date_labels = "%b-%y") + 
  facet_wrap(~Gear) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

# plot 3 location


ggplot(hist_table, aes(x = Date, y = Value)) + 
  geom_jitter(col = "black", size = 1, shape = 1)+
  geom_smooth(method = "loess", col = "black", size = 1)+
  coord_cartesian(ylim = c(0,75)) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ylab("Abundance") + 
  scale_x_date(date_breaks = "6 months", date_labels = "%b-%y") + 
  facet_wrap(~Location, labeller = labeller(Location = a2))


ggplot(hist_table, aes(x = Date, y = Value)) + 
  geom_jitter(col = "black", size = 1, shape = 1)+
  geom_smooth(method = "loess", col = "black", size = 1)+
  coord_cartesian(ylim = c(0,75)) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ylab("Abundance") + 
  scale_x_date(date_breaks = "6 months", date_labels = "%b-%y") + 
  facet_wrap(~Location, labeller = labeller(Location = a2))


#### GLM ####

m0 <- gam(Value ~ fLocation + Year + Gear + s(doy) + Species + Sample + Season + fYear, data = df2, family = "poisson")
m1 <- gam(Value ~ fLocation + Gear + s(doy) + Species, data = df2, family = "poisson")

summary(m1)

plot(m1)

#check colinearity

cor(Ipsen[,c("doy", "Value")])
vif(lm(Ipsen$Value ~ Ipsen$fLocation + fMonth))

Ipsen_lm <- data,lm(Value ~ doy, data = Ipsen)

vif(Ipsen_lm)

#create residuals

df2$RES = residuals.glm(m1, type="pearson")
df2$FIT = predict.glm(m1, type="response")

#check for patterns in residuals

ggplot(df2, aes(x=fLocation, y=RES, group=1)) + geom_jitter() + geom_smooth(method="loess")
ggplot(df2, aes(x=Gear, y=RES, group=1)) + geom_jitter() + geom_smooth(method="loess")
ggplot(df2, aes(x=Season, y=RES, group=1)) + geom_jitter() + geom_smooth(method="loess")
ggplot(df2, aes(x=Sample, y=RES)) + geom_jitter() + geom_smooth(method="loess")
ggplot(df2, aes(x=doy, y=RES, group=1)) + geom_jitter() + geom_smooth(method="lm")
ggplot(df2, aes(x=FIT, y=RES, group=1)) + geom_jitter() + geom_smooth(method="loess")
ggplot(df2, aes(x=Sample, y=RES, group=1)) + geom_jitter() + geom_smooth(method="loess")
ggplot(df2, aes(x=FIT)) + geom_histogram(binwidth = 0.1) + coord_cartesian(xlim = c(0,5))

#plot model 

df3 = expand.grid(fLocation =  levels(df2$fLocation),
                  Gear = levels(df2$Gear),
                  doy = df2$doy,
                  Species = levels(df2$Species))

df3$FIT = predict(m1, newdata = df3, type="response", se.fit = FALSE)
df3$SE = predict(m1, newdata = df3, type="response", se.fit = TRUE)$se



#top five species

top_five <- df3 %>%
  filter(Species == "Atlantic silverside" | Species == "Winter flounder" | Species == "Smelt" | Species == "Tomcod" | Species == "Black spotted SB" | Species == "Shorthorn sculpin") %>%
  droplevels()

#plot 4

ggplot(top_five, aes(x=doy, y=FIT)) + 
  geom_point(stat = "summary", fun.y = "mean", shape = 1, size = 1.5) +
  facet_wrap(~Species, labeller = labeller(Species = a1)) + 
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank()) +
  geom_smooth(col= "black", se = F) +
  ylab("Abundance") + 
  xlab("Day of Year")

a1 <- c(
"Atlantic silverside" = "Atlantic silverside",
"Black spotted SB"  = "Black spotted stickleback",
"Shorthorn sculpin" = "Shorthorn sculpin",
"Smelt" = "Smelt",
"Tomcod" = "Tomcod",
"Winter flounder" = "Winter flounder"
)


# plot 5 - location

ggplot(df3, aes(x=doy, y=FIT)) +
  geom_point(stat = "summary", fun.y = "mean", shape = 1) + 
  theme_classic() +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  geom_smooth(col= "black", se = F) +
  ylab("Abundance") + 
  xlab("Day of Year") + 
  coord_cartesian(ylim = c(-1.5,12)) + 
  facet_wrap(~fLocation, labeller = labeller(fLocation = a2))

# plot 5 names 

a2 <- c(
  "BB" = "Black Beach",
  "FFH"  = "Five Fathom Hole",
  "HB" = "Hepburn Basin"
  )

# plot 6 gear

ggplot(df3, aes(x=doy, y=FIT)) +
  geom_point(stat = "summary", fun.y = "mean", shape = 1) + 
  theme_classic() +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  geom_smooth(col= "black", se = F) +
  ylab("Abundance") + 
  xlab("Day of Year") + 
  coord_cartesian(ylim = c(-1.5,14)) + 
  facet_wrap(~Gear, labeller = labeller(fLocation = a2))


ggplot(df3, aes(x=doy, y=FIT)) +
  geom_smooth() +
  theme_classic() +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  geom_smooth(col= "black", se = F) +
  ylab("Abundance") + 
  xlab("Day of Year") + 
  coord_cartesian(ylim = c(0,110)) + 
  facet_wrap(~fLocation)

ggplot(subset(top_five, Gear == "Seine"), aes(x=doy, y=FIT, col = Species)) + geom_point(stat = "summary", fun.y = "mean") + 
  geom_smooth(aes(col = Species))

ggplot(top_five, aes(x=doy, y = FIT)) +
  geom_point(stat = "summary", fun.y = "mean") +
  geom_smooth(method = "gam")

####



ggplot(df2, aes(x= Value)) + geom_histogram(binwidth = 1) + coord_cartesian(xlim = c(0,10)) 
qqnorm(df2$Value)            
qqline(df2$Value)

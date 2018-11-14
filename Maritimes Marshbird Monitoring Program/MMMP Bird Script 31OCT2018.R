#packages

library(dplyr)
library(lubridate)
library(ggplot2)
library(mgcv)
library(data.table)
library(tidyr)

#load new data

setwd("S:/Science/Shared/JonesO/Musquash/Working Directories/Musquash Marsh Bird Baseline")
MMMP <- data.table(read.csv("MMMP JUN292018.csv"))

# tidy

MMMP$pointID <- as.character(MMMP$pointID)
MMMP$Date <- lubridate::dmy(MMMP$Date)
MMMP[,doy:=yday(Date)]
MMMP[,":=" (fYear=factor(year), fPoint=factor(point))]
MMMP$Type <- dplyr::recode(MMMP$Type, "cryptic" = "Cryptic")
MMMP$ym <- paste(MMMP$year, MMMP$Month, sep = "-")
MMMP <- MMMP %>%
  filter(fPoint != "10") %>%
  filter(fPoint != "11") %>%
  droplevels()

#### exploratory plots ####

set_sum <- MMMP %>%
  group_by(Date, Sample.ID, fPoint, fYear, Type) %>%
  summarise(Total = sum(Value))

species_richness <- MMMP %>%
  group_by(Date, Sample.ID, fPoint, fYear, Type) %>%
  summarise(SR = n_distinct(spcd))

ggplot(species_richness, aes(x = fYear, y = SR)) + geom_bar(stat = "summary", fun.y = "mean", aes(fill = Type)) + theme_bw() +
      theme(axis.line = element_line(colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank()) +
      xlab("Year") +
      ylab("Species Richness") +



#plot 1

ggplot(set_sum, aes(x= fPoint, y = Total)) + geom_boxplot() + theme_bw() + coord_cartesian(ylim = c(0,25)) + facet_wrap(~Type) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) + 
  xlab("Sample Site") + 
  ylab("Abundance")

#plot 2

t1 <- c(
  "coye" = "Common yellowthroat",
  "rwbl" = "Redwinged blackbird",
  "sosp" = "Song sparrow",
  "swsp" = "Swamp sparrow"
)

t2 <- c(
  "ambi" = "American bittern",
  "mawr" = "Marsh wren",
  "nesp" = "Nelson's sparrow",
  "sora" = "Sora"
)

ggplot(set_sum, aes(x= Date, y = Total)) +
  geom_jitter(shape = 1) + coord_cartesian(ylim = c(0,50)) + theme_bw() + facet_wrap(~Type) +
  geom_smooth(method = "loess", col = "black") + 
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) + 
        ylab("Abundance") + 
  



ggplot(set_sum, aes(x = Date, y = Total)) + geom_jitter() + geom_smooth(method = "loess") + facet_wrap(~fPoint) + coord_cartesian(ylim = c(0,100)) + theme_bw()
  
ggplot(df4, aes(x = Date, y = Value)) + geom_jitter(aes(col = Type)) + coord_cartesian(ylim = c(0,3)) + geom_smooth(aes(col = Type), method = "loess") + facet_wrap(~fPoint) + theme_bw()

#plot 3 

ggplot(subset(df4, spcd == "ambi" | spcd == "nesp" | spcd == "mawr" | spcd == "sora"), aes(x = Date, y = Value)) +
  geom_jitter(shape = 1) + facet_wrap(~spcd, labeller = labeller(spcd = t2)) + geom_smooth(method = "loess", colour = "black") + coord_cartesian(ylim = c(0,5)) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) + 
  ylab("Abundance")

#plot 4

ggplot(subset(df4, spcd == "rwbl" |spcd == "sosp" | spcd == "coye" | spcd == "swsp"), aes(x = Date, y = Value)) +
  geom_jitter(shape = 1) + facet_wrap(~spcd, labeller = labeller(spcd = t1)) + geom_smooth(method = "loess", col = "black") + coord_cartesian(ylim = c(0,5)) + 
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) + 
  ylab("Abundance")

ggplot(subset(df4, Type == "Cryptic"), aes(x = Date, y = Value)) + geom_jitter() + geom_smooth(method = "loess") + facet_wrap(~spcd)
ggplot(subset(df4, Type == "Secondary"), aes(x = Date, y = Value)) + geom_jitter() + geom_smooth(method = "loess")

ggplot(set_sum, aes(x = Date, y = Total)) + geom_jitter() + geom_smooth(method = "loess") + facet_wrap(~fPoint) + coord_cartesian(ylim = c(0,5)) + theme_bw()


#### Abundance model ####

# check for correlation between doy and temperature

ggplot(MMMP, aes(x=doy, y=Temperature)) + geom_point() + geom_smooth(method="loess") 

#### add zero counts ####

df2 <- data.table(expand.grid(Sample.ID = unique(MMMP$Sample.ID),
                              spcd = unique(MMMP$spcd),
                              Value = 0))

df3 <- unique(data.table(subset(MMMP,
                                select = c(Temperature, fPoint, doy, fYear, Sample.ID, obs, Date))))

df4 <- unique(data.table(subset(MMMP,select=c(Sample.ID,spcd,Value))))

df3 <- merge (df2, df3, all.x =T)

df4 = merge(df3, df4, all.x=T, by = c("Sample.ID", "spcd"))
df4[, Value := ifelse(is.na(Value.y), 0, Value.y)]
df4[, ":=" (Value.x=NULL, Value.y=NULL)]
df4[,spcd := factor(spcd)]

df4$Type <- ifelse(df4$spcd  == "ambi" | df4$spcd == "sora" | df4$spcd == "nesp" | df4$spcd == "pbgr" | df4$spcd == "vira" | df4$spcd == "goot" | df4$spcd == "mawr",
                   "Cryptic",
                   "Secondary")

df4$Type <- as.factor(df4$Type)

#make new model

m1 = gam(Value ~ fYear + fPoint + s(doy) + s(Temperature) + Type + spcd, df4, family=tw())
summary(m1)

#create fit and standard error for new model

df5 =   expand.grid(fYear = levels(df4$fYear),
                    fPoint = levels(df4$fPoint),
                    doy = mean(df4$doy),
                    Temperature = mean(df4$Temperature),
                    Type = levels(df4$Type), 
                    spcd = levels(df4$spcd))


df5$FIT = predict(m1, newdata=df5, type="response", se.fit = FALSE)
df5$SE = predict(m1, newdata=df5, type="response", se.fit = TRUE)$se

df5_sum <- df5 %>%
  group_by(fYear, fPoint, doy, Type) %>%
  summarise(MF = mean(FIT)) %>%
  droplevels()

# plot of difference between types

ggplot(df5_sum, aes(x = as.numeric(as.character(fYear)), y = MF)) + geom_jitter() + theme_bw() + facet_wrap(~Type) + coord_cartesian(ylim = c(0,1)) + geom_smooth()

ggplot(subset(subset(df5, spcd == "ambi" | spcd == "nesp" | spcd == "mawr" | spcd == "sora")), aes(x = as.numeric(as.character(fYear)), y = FIT)) +
  geom_jitter(stat = "summary", fun.y = "mean", shape = 1) + theme_bw() + facet_wrap(~spcd, labeller = labeller(spcd = t2)) + coord_cartesian(ylim = c(0,5)) + geom_smooth(colour = "black", se = F) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) + 
  ylab("Abundance") + 
  xlab("Year")

ggplot(subset(subset(df5, spcd == "rwbl" |spcd == "sosp" | spcd == "coye" | spcd == "swsp")), aes(x = as.numeric(as.character(fYear)), y = FIT)) +
  geom_jitter(stat = "summary", fun.y = "mean", shape = 1) + theme_bw() + facet_wrap(~spcd, labeller = labeller(spcd = t1)) + coord_cartesian(ylim = c(0,2)) + geom_smooth(colour = "black", se = F) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) + 
  ylab("Abundance") + 
  xlab("Year")


ggplot(df5, aes(x = as.numeric(as.character(fYear)), y = FIT)) +
  geom_jitter() + theme_bw() + facet_wrap(~spcd) + coord_cartesian(ylim = c(0,10)) + geom_smooth ()


ggplot(df5, aes(x=fYear, y=FIT, col = Type)) + geom_point()  +
  geom_linerange(aes(ymin = FIT - 1.96*SE, ymax = FIT + 1.96*SE)) + facet_wrap(~fPoint) +
  xlab("Year") +
  ylab("Pearson residuals") +
  theme_bw()

#### Sora model ####

SORA <- df4 %>%
  filter(spcd == "sora")

#make new model

m7 = gam(Value~fYear + fPoint +s(Temperature) + obs + s(doy), SORA, family="poisson")

summary(m7)
plot(m7)
anova(m7)

# plot residuals

SORA$RES = residuals.glm(m7, type="pearson")
SORA$FIT = predict.glm(m7, type="response")

ggplot(SORA, aes(x=FIT, y=RES, group=1)) + geom_jitter() + geom_smooth(method="loess")

# create fit and SE for new model

df6 =   expand.grid(fYear = levels(SORA$fYear),
                    fPoint = levels(SORA$fPoint),
                    doy = mean(SORA$doy),
                    Temperature = mean(SORA$Temperature),
                    obs = levels(SORA$obs))

df6$FIT = predict(m7, newdata=df6, type="response", se.fit = FALSE)
df6$SE = predict(m7, newdata=df6, type="response", se.fit = TRUE)$se

df6 <- df6 %>%
  filter(fPoint == "1" | fPoint == "2" | fPoint == "3" | fPoint == "4"| fPoint == "5" | fPoint == "6" | fPoint == "7" | fPoint == "8")

ggplot(df6, aes(x=fYear, y=FIT)) + geom_point()  +
  geom_linerange(aes(ymin = FIT - 1.96*SE, ymax = FIT + 1.96*SE)) + facet_wrap(~fPoint) +
  xlab("Year") +
  ylab("Pearson residuals")

#### Marsh wren ####

MAWR <- df4 %>%
  filter(spcd == "mawr")

#make new model

m8 = gam(Value~fYear + fPoint +s(Temperature) + obs + s(doy), MAWR, family="poisson")

summary(m8)
plot(m8)

# plot residuals

MAWR$RES = residuals.glm(m8, type="pearson")
MAWR$FIT = predict.glm(m8, type="response")

ggplot(MAWR, aes(x=FIT, y=RES, group=1)) + geom_jitter() + geom_smooth(method="loess")

# create fit and SE for new model

df7 =   expand.grid(fYear = levels(MAWR$fYear),
                    fPoint = levels(MAWR$fPoint),
                    doy = mean(MAWR$doy),
                    Temperature = mean(MAWR$Temperature),
                    obs = levels(MAWR$obs))

df7$FIT = predict(m8, newdata=df7, type="response", se.fit = FALSE)
df7$SE = predict(m8, newdata=df7, type="response", se.fit = TRUE)$se

df7 <- df7 %>%
  filter(fPoint == "1" | fPoint == "2" | fPoint == "3" | fPoint == "4"| fPoint == "5" | fPoint == "6" | fPoint == "7" | fPoint == "8" | fPoint == "9")

ggplot(df7, aes(x=fYear, y=FIT)) + geom_point()  +
  geom_linerange(aes(ymin = FIT - 1.96*SE, ymax = FIT + 1.96*SE)) + facet_wrap(~fPoint) +
  xlab("Year") +
  ylab("Pearson residuals")

#### Red winged blackbird ####

RWBL <- df4 %>%
  filter(spcd == "rwbl")

#make new model

m9 = gam(Value~fYear + fPoint +s(Temperature) + obs + s(doy), RWBL, family="poisson")

summary(m9)
plot(m9)

# plot residuals

RWBL$RES = residuals.glm(m9, type="pearson")
RWBL$FIT = predict.glm(m9, type="response")

ggplot(RWBL, aes(x=FIT, y=RES, group=1)) + geom_jitter() + geom_smooth(method="loess")

# create fit and SE for new model

df8 =   expand.grid(fYear = levels(RWBL$fYear),
                    fPoint = levels(RWBL$fPoint),
                    doy = mean(RWBL$doy),
                    Temperature = mean(RWBL$Temperature),
                    obs = levels(RWBL$obs))

df8$FIT = predict(m9, newdata=df8, type="response", se.fit = FALSE)
df8$SE = predict(m9, newdata=df8, type="response", se.fit = TRUE)$se

df8 <- df8 %>%
  filter(fPoint == "1" | fPoint == "2" | fPoint == "3" | fPoint == "4"| fPoint == "5" | fPoint == "6" | fPoint == "7" | fPoint == "8" | fPoint == "9")

ggplot(df8, aes(x=fYear, y=FIT)) + geom_point()  +
  geom_linerange(aes(ymin = FIT - 1.96*SE, ymax = FIT + 1.96*SE)) + facet_wrap(~fPoint) +
  xlab("Year") +
  ylab("Pearson residuals")

ggplot(MMMP, aes(x = doy, y = Value)) +
  geom_point(aes(col = pointID)) +
  geom_smooth(aes(col = pointID), se = FALSE)

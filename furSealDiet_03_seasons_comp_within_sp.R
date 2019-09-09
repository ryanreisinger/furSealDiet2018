##Fur Seal Diet
##2016-11-30
##Ryan R Reisinger

#-----------------------------------------------------

library(plyr)
library(ggplot2)
library(tidyr)
library(vegan)
library(MASS)

#-----------------------------------------------------

# Seal colours
# trop
# light orange #f1a340
# dark orange #e66101
# gaz
# ligt purple #998ec3
# dark purple #5e3c99

#-----------------------------------------------------

setwd("C:/Users/Ryan Reisinger/Documents/Academic/PEI_Toppredators/Fur Seal Diet/Data/Working")
dat <- read.csv("AbundanceDat. 21-06_RR_01.csv", stringsAsFactors = FALSE)

names(dat) <- c("date", "year", "season", "group", "sample", "prey", "length", "biomass",
                "sl_tl", "u1", "u2", "seal")

dat$date <- as.POSIXct(dat$date, format = "%d-%b-%y")

#correct some errors
dat[dat$prey == "Crustacean?", "prey"] <- "Crustacean"
dat[dat$prey == "Symbolophorus boops ", "prey"] <- "Symbolophorus boops"
dat[dat$prey == "Protomytophum choriodon", "prey"] <- "Protomyctophum choriodon"
dat[dat$prey == "B. gracilis", "prey"] <- "Bathylagus gracilis"

#Remove unknown prey
dat <- dat[dat$prey != "Unknown", ]

#Sample ID
dat$sample.id <- paste0(dat$seal, "-", dat$sample)

#aggregate prey species:
dat[dat$prey == "Krill", "prey"] <- "Crustacean"
dat[dat$prey == "Amphipod", "prey"] <- "Crustacean"

dat[dat$prey == "Unknown 1", "prey"] <- "Sternoptychidae"
dat[dat$prey == "Unknown 2", "prey"] <- "Sternoptychidae"
dat[dat$prey == "Unknown 3", "prey"] <- "Sternoptychidae"
dat[dat$prey == "Unknown 4", "prey"] <- "Sternoptychidae"

#aggregate prey groups:
dat[dat$group == "Krill", "group"] <- "Crustacean"
dat[dat$group == "Amphipod", "group"] <- "Crustacean"


unique(dat$prey)
unique(dat$group)

#-----------------------------------------------------
#For stats on aggregated prey:
#dat$prey <- dat$group



#------------------------
##Sampling Effort

dat$month <- strftime(dat$date, format = "%m")
dat$year.month <- strftime(dat$date, format = "%Y-%m")

trop <- dat[dat$seal == "trop", ]
gaz <- dat[dat$seal == "gaz", ]


#-----------------------------------------------------
#For stats by season:
dat$season <- rep("summer", nrow(dat))
dat[dat$month == "04" |
              dat$month == "05" |
              dat$month == "06" |
              dat$month == "07" |
              dat$month == "08" |
              dat$month == "09", "season"] <- "winter"

#----------------------------------------------------


#dat <- dat[dat$seal == "gaz", ]
dat <- dat[dat$seal == "trop", ]


#------------------------------------------------------------------------------
##Multivariate analysis

dat.long <- ddply(dat, c("sample.id", "prey"), function(df)nrow(df))
dat.wide <- spread(dat.long, key = prey, value = V1, fill = 0)
dat.temp <- dat[ , c("year", "season", "seal", "month", "year.month", "sample.id")]
dat.temp <- dat.temp[!duplicated(dat.temp$sample.id), ]

dat.mult <- merge(dat.temp, dat.wide, by = "sample.id")
rm(dat.temp)

ad.mat <- as.matrix(dat.mult[ , c(7:length(dat.mult))])

#------------
#Adonis test
ad <- adonis(ad.mat ~ dat.mult$season, method = "bray", permutations = 9999)

#Compare with ANOSIM
an <- anosim(dat = ad.mat, grouping = dat.mult$season, distance = "bray")

#------------
#SIMPER - which prey contributes most difference between the two groups

simp <- simper(ad.mat, group = dat.mult$season, permutations = 0)

#------------------------------------------------------------------------------
## Diet overlap

#https://cran.r-project.org/web/packages/EcoSimR/vignettes/nicheOverlapVignette.html
#also pgirmess:piankabio, can boostrap with piankabioboot

library(EcoSimR)

gaz.pi <- gaz.prey.summary[ , c("prey", "na.number.all")]
names(gaz.pi) <- c("prey", "gaz")

trop.pi <- trop.prey.summary[ , c("prey", "na.number.all")]
names(trop.pi) <- c("prey", "trop")

pi <- merge(gaz.pi, trop.pi, by = "prey", all = T)
pi[is.na(pi$gaz), "gaz"] <- 0
pi[is.na(pi$trop), "trop"] <- 0
pi <- pi[ , -1]

pi <- t(pi)
pi <- as.data.frame(pi)

myRandomModel <- niche_null_model(speciesData=pi,
                                  algo="ra3", metric="czekanowski", 
                                  suppressProg=FALSE,nReps=5000)
summary(myRandomModel)

#try spaa library
library(spaa)

overlap.pianka <- niche.overlap.boot(mat = t(pi), method = "pianka", times = 9999)
overlap.pianka

overlap.schoener <- niche.overlap.boot(mat = t(pi), method = "schoener", times = 9999)
overlap.schoener

#------------------------------------------------------------------------------
##Assign new seasons

# dat$season <- rep("summer", nrow(dat))
# dat[scats.month$month == "04" |
#       dat$month == "05" |
#       dat$month == "06" |
#       dat$month == "07" |
#       dat$month == "08" |
#       dat$month == "09", "season"] <- "winter"

#------------------------------------------------------------------------------
##Subset data as neccessary for these analyses.
##See separate R-scripts


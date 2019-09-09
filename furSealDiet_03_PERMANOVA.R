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


##Sampling Effort

dat$month <- strftime(dat$date, format = "%m")
dat$year.month <- strftime(dat$date, format = "%Y-%m")



#------------------------------------------------------------------------------

##Repeat Multivariate analysis with year

#Add season
dat$season <- rep("summer", nrow(dat))
dat[dat$month == "04" |
      dat$month == "05" |
      dat$month == "06" |
      dat$month == "07" |
      dat$month == "08" |
      dat$month == "09", "season"] <- "winter"

#Select only the summer data
#dat <- dat[dat$season == "summer", ]

#Assign time period to each summer
dat$time <- rep(NA, nrow(dat))

dat[dat$date > strptime("2006-09-30", format = "%Y-%m-%d") & dat$date < strptime("2007-04-01", format = "%Y-%m-%d"), "time"] <- "2006/2007"
dat[dat$date > strptime("2007-09-30", format = "%Y-%m-%d") & dat$date < strptime("2008-04-01", format = "%Y-%m-%d"), "time"] <- "2007/2008"
dat[dat$date > strptime("2008-09-30", format = "%Y-%m-%d") & dat$date < strptime("2009-04-01", format = "%Y-%m-%d"), "time"] <- "2008/2009"
dat[dat$date > strptime("2009-09-30", format = "%Y-%m-%d") & dat$date < strptime("2010-04-01", format = "%Y-%m-%d"), "time"] <- "2009/2010"

#Test one species at a time
#dat <- dat[dat$seal == "trop", ]
#dat <- dat[dat$season == "winter", ]

dat.long <- ddply(dat, c("sample.id", "prey"), function(df)nrow(df))
dat.wide <- spread(dat.long, key = prey, value = V1, fill = 0)
dat.temp <- dat[ , c("year", "season", "seal", "month", "year.month", "sample.id", "time")]
dat.temp <- dat.temp[!duplicated(dat.temp$sample.id), ]

dat.mult <- merge(dat.temp, dat.wide, by = "sample.id")
rm(dat.temp)

ad.mat <- as.matrix(dat.mult[ , c(8:length(dat.mult))])

#------------
#Adonis test
ad <- adonis(ad.mat ~ dat.mult$seal, method = "bray", permutations = 9999)

#Compare with ANOSIM
an <- anosim(dat = ad.mat, grouping = dat.mult$seal, distance = "bray")

#------------
#SIMPER - which prey contributes most difference between the two groups

simp <- simper(ad.mat, group = dat.mult$seal, permutations = 0)

#------------
#Random Forest for classification

library(randomForest)


dat.f <- dat.mult

#Select only complete cases
#dat.f <- dat.f[complete.cases(dat.f), ]

#Select species if neccessary
dat.f <- dat.f[dat.f$seal == "trop", ]
#Build RF
rf <- randomForest(data = dat.f, y = as.factor(dat.f$season), x = dat.f[ , 8:ncol(dat.f)], ntree = 1000,
                   na.action = na.omit, proximity = T, importance = T)
rf
varImpPlot(rf, scale = T, col = "red")
importance(rf, type = 1)


#Plots
par(ps = 9) #set font size
varImpPlot(rf, pch = 16)
plot(dat.f$SST, dat.f$CHL, col=as.factor(dat.f$Group)) #plot two most important vars
MDSplot(rf, fac = as.factor(dat.f$Group), k=5)

#MDS with rfPermute
library(rfPermute)
proximityPlot(rf, legend.loc = "right", circle.size = NULL)


#Make own plot - proximity as MDS
rf.mds <- cmdscale(1- rf$proximity, k = 2)
mdsframe <- cbind.data.frame(dat.f$season, rf.mds[ , 1], rf.mds[ ,2])
names(mdsframe) <- c("season", "Dim1", "Dim2")

p.mds <- ggplot(mdsframe, aes(x = Dim1, y = Dim2, colour = season))
p.mds <- p.mds + geom_point(size = 1.5, alpha = 1) + scale_colour_manual(values = c("#377eb8", "#e41a1c", "#984ea3", "#4daf4a"))
p.mds <- p.mds + theme_bw() + theme(panel.grid.minor=element_blank(),
                                    panel.grid.major=element_blank())
p.mds <- p.mds + labs(x = "Dimension 2", y = "Dimension 1")
p.mds

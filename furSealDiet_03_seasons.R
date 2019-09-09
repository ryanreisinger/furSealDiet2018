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

#-----------------------------------------------------



#------------------------------------------------------------------------------
## Wrap the calculations up in a function

scatR <- function(dat, species){
  #dat = dataframe containing the data, formatted as above
  #species = character string with species code ("gaz" or "trop")
  
  SPEC.prey <- unique(dat[dat$seal == paste(species), "prey"])
  
  SPEC.prey.fo <- list()
  SPEC.prey.na.all <- list()
  
  for(i in 1:length(SPEC.prey)){
    a <- dat[dat$seal == paste(species), ]
    a <- a[a$prey == SPEC.prey[i], ]
    b <- nrow(a)
    SPEC.prey.na.all[i] <- b
    c <- length(unique(a$sample))
    SPEC.prey.fo[i] <- c
  }
  
  SPEC.prey.fo <- unlist(SPEC.prey.fo, use.names = FALSE)
  SPEC.prey.na.all <- unlist(SPEC.prey.na.all, use.names = FALSE)
  
  SPEC.prey.table <-cbind.data.frame(SPEC.prey, SPEC.prey.fo, SPEC.prey.na.all)
  SPEC.prey.table <- as.data.frame(SPEC.prey.table)
  names(SPEC.prey.table) <- c("prey", "fo.number", "na.number.all")
  SPEC.prey.table$fo.number <- as.numeric(SPEC.prey.table$fo.number)
  SPEC.prey.table$na.number.all <- as.numeric(SPEC.prey.table$na.number.all)
  
  SPEC.prey.table$samples<- rep(length(unique(dat[dat$seal == paste(species), "sample"])), nrow(SPEC.prey.table))
  SPEC.prey.table$fo.percent <- (SPEC.prey.table$fo.number/SPEC.prey.table$samples)*100
  
  SPEC.prey.table$prey.items.all<- rep(sum(SPEC.prey.table$na.number.all), nrow(SPEC.prey.table))
  SPEC.prey.table$na.percent.all <- (SPEC.prey.table$na.number.all/SPEC.prey.table$prey.items.all)*100
  
  SPEC.prey.table <- SPEC.prey.table[ , c("prey", "fo.number", "samples", "fo.percent", "na.number.all", "prey.items.all", "na.percent.all")]
  
  SPEC.pps <- ddply(dat[dat$seal == paste(species), ], c("sample", "prey"), function(df)sum(df$sample))
  SPEC.s <- ddply(SPEC.pps, c("sample"), function(df)sum(df$V1))
  SPEC.na.long <- merge(x = SPEC.pps,y = SPEC.s, by = "sample", all = T)
  names(SPEC.na.long) <- c("sample", "prey", "number.prey", "number.prey.sample")
  SPEC.na.long$na.percent <- (SPEC.na.long$number.prey/SPEC.na.long$number.prey.sample)*100
  SPEC.na.long <- SPEC.na.long[ , c("sample", "prey", "na.percent")]
  
  SPEC.na.wide <- spread(SPEC.na.long, key = prey, value = na.percent, fill = 0)
  SPEC.means <- apply(X = SPEC.na.wide[ ,-1], MARGIN = 2, FUN = mean)
  SPEC.means <- as.data.frame(SPEC.means)
  names(SPEC.means) <- "mean"
  SPEC.means$prey <- row.names(SPEC.means)
  SPEC.sds <- apply(X = SPEC.na.wide[ ,-1], MARGIN = 2, FUN = sd)
  SPEC.sds <- as.data.frame(SPEC.sds)
  names(SPEC.sds) <- "sd"
  SPEC.sds$prey <- row.names(SPEC.sds)
  
  SPEC.na <- merge(SPEC.means, SPEC.sds, by = "prey")
  names(SPEC.na) <- c("prey", "na.percent.mean", "na.percent.sd")
  
  SPEC.prey.summary <- merge(x = SPEC.prey.table, y = SPEC.na, by = "prey")
  
  return(SPEC.prey.summary)
  
}
#------------------------------------------------------------------------------


dat <- dat[dat$season == "winter", ]



#Run scatR to produce the summary tables
gaz.prey.summary <- scatR(dat = dat, species = "gaz")
trop.prey.summary <- scatR(dat = dat, species = "trop")

#run on aggregated prey to get fish contribution
#see lines near beginning

dat.temp <- dat
dat.temp[dat.temp$group == "Fish", "prey"] <- "Fish"
t1 <- scatR(dat = dat.temp, species = "gaz")
t2 <- scatR(dat = dat.temp, species = "trop")

#write.csv(t1, file = "summary_alltime_gaz_summer.csv", row.names = F)
#write.csv(t2, file = "summary_alltime_trop_summer.csv", row.names = F)

#------------------------------------------------------------------------------

#------------------------
#Rank Correlation
cor(trop.prey.summary$fo.percent, trop.prey.summary$na.percent.mean, method = "spearman")
cor(gaz.prey.summary$fo.percent, gaz.prey.summary$na.percent.mean, method = "spearman")

#Plot rank correlation
trop.prey.summary$fo.rank <- rank(trop.prey.summary$fo.percent, ties.method = "random")
trop.prey.summary$na.rank <- rank(trop.prey.summary$na.percent.all, ties.method = "random")

gaz.prey.summary$fo.rank <- rank(gaz.prey.summary$fo.percent, ties.method = "random")
gaz.prey.summary$na.rank <- rank(gaz.prey.summary$na.percent.all, ties.method = "random")

ggplot(trop.prey.summary, aes(x = na.rank, y = fo.rank)) + geom_point()
ggplot(gaz.prey.summary, aes(x = na.rank, y = fo.rank)) + geom_point()

#------------------------------------------------------------------------------
#Bootstrap of diet measures.

#apply mean over an array:
#A.mean <- apply(A, c(1,2), mean)

#------------------------
#gaz
dat.gaz <- dat[dat$seal == "gaz", ]

#create dataframe of species to ensure subsampled matrices have the same dimensions
#(species may be left out in each bootstrap sample)
spec.gaz <- gaz.prey.summary[ , 1:2]
spec.gaz[ , 2] <- rep(NA, nrow(spec.gaz))
names(spec.gaz) <- c("prey", "HOLDER")

boot.hold <- list()
samples <- unique(dat.gaz$sample)

system.time(
  for(i in 1:500){ #1000 times
    da <- dat.gaz[0, ]
    samp <- sample(x = samples, size = length(samples), replace = T)
    for(j in 1:length(samp)){
      su <- dat.gaz[dat.gaz$sample == samp[j], ]
      su$sample <- paste0(j, su$sample) #need to make the samples unique, otherwise resamples are combined
      su$sample <- as.numeric(su$sample) #coerce to numeric
      da <- rbind(da, su)
    }
    #problem - the next step was just drawing random prey types within scats, not keeping scats together
    #hence loop above
    #da <- dat.gaz[sample(x = nrow(dat.gaz), size = nrow(dat.gaz), replace = T), ] #sample same sample size
    da.summary <- scatR(dat = da, species = "gaz")
    da.m <- merge(x = spec.gaz, y = da.summary, by = "prey", all.x = TRUE)
    da.m$HOLDER <- NULL
    da.m[is.na(da.m)] <- 0
    boot.hold[[i]] <- as.matrix(da.m[ 2:9])
    print(i)
  }
)

boot.hold <- array(data = unlist(boot.hold), dim = c(nrow(spec.gaz), 8, 500)) #array 31 rows, 8 cols, 10 deep

#stats
#mean - not neccessary, use actual estimate instead
gaz.boot.mean <- apply(boot.hold, c(1,2), mean)
#sd can be used to work out confint - see
#http://influentialpoints.com/Training/bootstrap_confidence_intervals-principles-properties-assumptions.htm
gaz.boot.sd <- apply(boot.hold, c(1,2), sd)
#but rather using percentile rank
gaz.boot.LCL <- apply(boot.hold, c(1,2), quantile, probs = .025)
gaz.boot.UCL <- apply(boot.hold, c(1,2), quantile, probs = .975)

#turn into a dataframe and add names
#LCL
gaz.boot.LCL <- as.data.frame(gaz.boot.LCL)
names(gaz.boot.LCL) <- names(gaz.prey.summary[ ,c(2:9)])
gaz.boot.LCL$prey <- spec.gaz$prey
#UCL
gaz.boot.UCL <- as.data.frame(gaz.boot.UCL)
names(gaz.boot.UCL) <- names(gaz.prey.summary[ ,c(2:9)])
gaz.boot.UCL$prey <- spec.gaz$prey

gaz.sum <- data.frame("prey" = gaz.prey.summary$prey,
                      "n.samples" = gaz.prey.summary$samples,
                      "fo.number" = gaz.prey.summary$fo.number,
                      "fo.number.LCL" = gaz.boot.LCL$fo.number,
                      "fo.number.UCL" = gaz.boot.UCL$fo.number,
                      "fo.percent" = gaz.prey.summary$fo.percent,
                      "fo.percent.LCL" = gaz.boot.LCL$fo.percent,
                      "fo.percent.UCL" = gaz.boot.UCL$fo.percent,
                      "n.preyitems" = gaz.prey.summary$prey.items.all,
                      "na.number" = gaz.prey.summary$na.number.all,
                      "na.number.LCL" = gaz.boot.LCL$na.number.all,
                      "na.number.UCL" = gaz.boot.UCL$na.number.all,
                      "na.percent" = gaz.prey.summary$na.percent.all,
                      "na.percent.LCL" = gaz.boot.LCL$na.percent.all,
                      "na.percent.UCL" = gaz.boot.UCL$na.percent.all,
                      "na.weighted" = gaz.prey.summary$na.percent.mean,
                      "na.weighted.LCL" = gaz.boot.LCL$na.percent.mean,
                      "na.weighted.UCL" = gaz.boot.UCL$na.percent.mean,
                      "rank.fo" = gaz.prey.summary$fo.rank,
                      "rank.na" = gaz.prey.summary$na.rank)

saveRDS(gaz.sum, "gaz_sum_winter.rds")
gaz.sum <- readRDS("gaz_sum_winter.rds")

#------------------------
#trop
dat.trop <- dat[dat$seal == "trop", ]

#create dataframe of species to ensure subsampled matrices have the same dimensions
#(species may be left out in each bootstrap sample)
spec.trop <- trop.prey.summary[ , 1:2]
spec.trop[ , 2] <- rep(NA, nrow(spec.trop))
names(spec.trop) <- c("prey", "HOLDER")

boot.hold <- list()
samples <- unique(dat.trop$sample)

system.time(
  for(i in 1:500){ #1000 times
    da <- dat.trop[0, ]
    samp <- sample(x = samples, size = length(samples), replace = T)
    for(j in 1:length(samp)){
      su <- dat.trop[dat.trop$sample == samp[j], ]
      su$sample <- paste0(j, su$sample) #need to make the samples unique, otherwise resamples are combined
      su$sample <- as.numeric(su$sample) #coerce to numeric
      da <- rbind(da, su)
    }
    #problem - the next step was just drawing random prey types within scats, not keeping scats together
    #hence loop above
    #da <- dat.trop[sample(x = nrow(dat.trop), size = nrow(dat.trop), replace = T), ] #sample same sample size
    da.summary <- scatR(dat = da, species = "trop")
    da.m <- merge(x = spec.trop, y = da.summary, by = "prey", all.x = TRUE)
    da.m$HOLDER <- NULL
    da.m[is.na(da.m)] <- 0
    boot.hold[[i]] <- as.matrix(da.m[ 2:9])
    print(i)
  }
)

boot.hold <- array(data = unlist(boot.hold), dim = c(nrow(spec.trop), 8, 500)) #array 31 rows, 8 cols, 10 deep

#stats
#mean - not neccessary, use actual estimate instead
trop.boot.mean <- apply(boot.hold, c(1,2), mean)
#sd can be used to work out confint - see
#http://influentialpoints.com/Training/bootstrap_confidence_intervals-principles-properties-assumptions.htm
trop.boot.sd <- apply(boot.hold, c(1,2), sd)
#but rather using percentile rank
trop.boot.LCL <- apply(boot.hold, c(1,2), quantile, probs = .025)
trop.boot.UCL <- apply(boot.hold, c(1,2), quantile, probs = .975)

#turn into a dataframe and add names
#LCL
trop.boot.LCL <- as.data.frame(trop.boot.LCL)
names(trop.boot.LCL) <- names(trop.prey.summary[ ,c(2:9)])
trop.boot.LCL$prey <- spec.trop$prey
#UCL
trop.boot.UCL <- as.data.frame(trop.boot.UCL)
names(trop.boot.UCL) <- names(trop.prey.summary[ ,c(2:9)])
trop.boot.UCL$prey <- spec.trop$prey

trop.sum <- data.frame("prey" = trop.prey.summary$prey,
                      "n.samples" = trop.prey.summary$samples,
                      "fo.number" = trop.prey.summary$fo.number,
                      "fo.number.LCL" = trop.boot.LCL$fo.number,
                      "fo.number.UCL" = trop.boot.UCL$fo.number,
                      "fo.percent" = trop.prey.summary$fo.percent,
                      "fo.percent.LCL" = trop.boot.LCL$fo.percent,
                      "fo.percent.UCL" = trop.boot.UCL$fo.percent,
                      "n.preyitems" = trop.prey.summary$prey.items.all,
                      "na.number" = trop.prey.summary$na.number.all,
                      "na.number.LCL" = trop.boot.LCL$na.number.all,
                      "na.number.UCL" = trop.boot.UCL$na.number.all,
                      "na.percent" = trop.prey.summary$na.percent.all,
                      "na.percent.LCL" = trop.boot.LCL$na.percent.all,
                      "na.percent.UCL" = trop.boot.UCL$na.percent.all,
                      "na.weighted" = trop.prey.summary$na.percent.mean,
                      "na.weighted.LCL" = trop.boot.LCL$na.percent.mean,
                      "na.weighted.UCL" = trop.boot.UCL$na.percent.mean,
                      "rank.fo" = trop.prey.summary$fo.rank,
                      "rank.na" = trop.prey.summary$na.rank)

saveRDS(trop.sum, "trop_sum_winter.rds")
trop.sum <- readRDS("trop_sum_winter.rds")

#------------------------------------------------------------------------------
#Piecewise regression to identify important prey
library(segmented)

#---------------
#gaz
#invert the ranks
gaz.sum$rank.fo.inv <- rank(-gaz.sum$fo.percent, ties.method = "random")
gaz.sum$rank.na.inv <- rank(-gaz.sum$na.percent, ties.method = "random")

#fo percent
gaz.sum.ord <- gaz.sum[order(gaz.sum$rank.fo.inv), ]
gaz.sum.ord$cum.fo <- cumsum(gaz.sum.ord$fo.percent)
plot(gaz.sum.ord$rank.fo.inv, gaz.sum.ord$cum.fo)

gaz.fo.seg <- segmented(obj =lm(cum.fo ~ rank.fo.inv, data=gaz.sum.ord),
                        seg.Z = ~rank.fo.inv)
gaz.fo.seg
plot(gaz.fo.seg)
plot(gaz.sum.ord$rank.fo.inv, gaz.sum.ord$cum.fo)
abline(v = gaz.fo.seg$psi[1, 2])
plot(gaz.fo.seg, add=T)

#na percent
gaz.sum.ord <- gaz.sum.ord[order(gaz.sum.ord$rank.na.inv), ]
gaz.sum.ord$cum.na <- cumsum(gaz.sum.ord$na.percent)
plot(gaz.sum.ord$rank.na.inv, gaz.sum.ord$cum.na)

gaz.na.seg <- segmented(obj =lm(cum.na ~ rank.na.inv, data=gaz.sum.ord),
                        seg.Z = ~rank.na.inv)
gaz.na.seg
plot(gaz.na.seg)
plot(gaz.sum.ord$rank.na.inv, gaz.sum.ord$cum.na)
abline(v = gaz.na.seg$psi[1, 2])
plot(gaz.na.seg, add=T)

#add the information
gaz.sum.ord$fo.prim <- rep("primary", nrow(gaz.sum.ord))
gaz.sum.ord[gaz.sum.ord$rank.fo.inv > gaz.fo.seg$psi[1, 2], "fo.prim"] <- "non-primary"

gaz.sum.ord$na.prim <- rep("primary", nrow(gaz.sum.ord))
gaz.sum.ord[gaz.sum.ord$rank.na.inv > gaz.na.seg$psi[1, 2], "na.prim"] <- "non-primary"

#---------------
#trop
#invert the ranks
trop.sum$rank.fo.inv <- rank(-trop.sum$fo.percent, ties.method = "random")
trop.sum$rank.na.inv <- rank(-trop.sum$na.percent, ties.method = "random")

#fo percent
trop.sum.ord <- trop.sum[order(trop.sum$rank.fo.inv), ]
trop.sum.ord$cum.fo <- cumsum(trop.sum.ord$fo.percent)
plot(trop.sum.ord$rank.fo.inv, trop.sum.ord$cum.fo)

trop.fo.seg <- segmented(obj =lm(cum.fo ~ rank.fo.inv, data=trop.sum.ord),
                         seg.Z = ~rank.fo.inv)
trop.fo.seg
plot(trop.fo.seg)
plot(trop.sum.ord$rank.fo.inv, trop.sum.ord$cum.fo)
abline(v = trop.fo.seg$psi[1, 2])
plot(trop.fo.seg, add=T)

#na percent
trop.sum.ord <- trop.sum.ord[order(trop.sum.ord$rank.na.inv), ]
trop.sum.ord$cum.na <- cumsum(trop.sum.ord$na.percent)
plot(trop.sum.ord$rank.na.inv, trop.sum.ord$cum.na)

trop.na.seg <- segmented(obj =lm(cum.na ~ rank.na.inv, data=trop.sum.ord),
                         seg.Z = ~rank.na.inv)
trop.na.seg
plot(trop.na.seg)
plot(trop.sum.ord$rank.na.inv, trop.sum.ord$cum.na)
abline(v = trop.na.seg$psi[1, 2])
plot(trop.na.seg, add=T)

#add the information
trop.sum.ord$fo.prim <- rep("primary", nrow(trop.sum.ord))
trop.sum.ord[trop.sum.ord$rank.fo.inv > trop.fo.seg$psi[1, 2], "fo.prim"] <- "non-primary"

trop.sum.ord$na.prim <- rep("primary", nrow(trop.sum.ord))
trop.sum.ord[trop.sum.ord$rank.na.inv > trop.na.seg$psi[1, 2], "na.prim"] <- "non-primary"

#----------------------
#Save for table

write.csv(gaz.sum.ord, "summary_alltime_gaz_winter.csv", row.names = F)
write.csv(trop.sum.ord, "summary_alltime_trop_winter.csv", row.names = F)
temp <- merge(trop.sum.ord, gaz.sum.ord, by = "prey", all = T)
write.csv(temp, "summary_alltime_bothsp_winter.csv", row.names = F)

saveRDS(gaz.sum.ord, "summary_alltime_gaz_winter.rds")
gaz.sum.ord <- readRDS("summary_alltime_gaz_winter.rds")
saveRDS(trop.sum.ord, "summary_alltime_trop_winter.rds")
trop.sum.ord <- readRDS("summary_alltime_trop_winter.rds")



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
ad <- adonis(ad.mat ~ dat.mult$seal, method = "bray", permutations = 9999)

#Compare with ANOSIM
an <- anosim(dat = ad.mat, grouping = dat.mult$seal, distance = "bray")

#------------
#SIMPER - which prey contributes most difference between the two groups

simp <- simper(ad.mat, group = dat.mult$seal, permutations = 0)

#------------
#NMDS

#see https://jonlefcheck.net/2012/10/24/nmds-tutorial-in-r/ and
#http://chrischizinski.github.io/rstats/2014/04/13/vegan-ggplot2


dimnames(ad.mat) <- list(dat.mult$sample.id, dimnames(ad.mat)[[2]])
nmds <- metaMDS(ad.mat, distance = "bray", k = 2, autotransform = FALSE,
                engine = "monoMDS")

stressplot(nmds)

ordiplot(nmds, type = "points", display = "sites")
#ordiellipse(nmds, groups = dat.mult$seal, display = "sites", kind = "se", conf = 0.95, label = T, col = c("red", "blue"))
ordihull(nmds, groups = dat.mult$seal, display = "sites", draw = "polygon", col = c("red", "blue"), alpha = 50)
ordispider(nmds, groups = dat.mult$seal, col = c("red", "blue"))


#Plot with ggplot
#from http://stackoverflow.com/questions/13794419/plotting-ordiellipse-function-from-vegan-package-onto-nmds-plot-created-in-ggplo

NMDS = data.frame(MDS1 = nmds$points[,1], MDS2 = nmds$points[,2], group=as.factor(dat.mult$seal))
ord<-ordiellipse(nmds, dat.mult$seal, display = "sites", 
                 kind = "se", conf = 0.95, label = T)

veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100) 
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}


df_ell <- data.frame()
for(g in levels(NMDS$group)){
  df_ell <- rbind(df_ell, cbind(as.data.frame(with(NMDS[NMDS$group==g,],
                                                   veganCovEllipse(ord[[g]]$cov,ord[[g]]$center,ord[[g]]$scale))),
                                group=g))
}

ggplot(data = NMDS, aes(MDS1, MDS2)) +
  geom_point(aes(color = group)) +
  geom_path(data=df_ell, aes(x=NMDS1, y=NMDS2,colour=group), size=1, linetype=2)

#------------
#Rarefaction curve?

ad.mat.gaz <- as.matrix(dat.mult[dat.mult$seal == "gaz", c(7:length(dat.mult))])
ad.mat.trop <- as.matrix(dat.mult[dat.mult$seal == "trop", c(7:length(dat.mult))])

#Rarefaction to find number of species... working on it.
rr.gaz <- rarefy(ad.mat, sample = 600, MARGIN = 1)



#Species accumulation curve
ad.mat.gaz <- as.matrix(dat.mult[dat.mult$seal == "gaz", c(7:length(dat.mult))])
ad.mat.trop <- as.matrix(dat.mult[dat.mult$seal == "trop", c(7:length(dat.mult))])

sac.gaz <- specaccum(ad.mat.gaz, method = "rarefaction", permutations = 100,
                     conditioned =TRUE, gamma = "jack1")
sac.gaz.col <- specaccum(ad.mat.gaz, method = "collector", permutations = 100,
                         conditioned =TRUE, gamma = "jack1")

sac.trop <- specaccum(ad.mat.trop, method = "rarefaction", permutations = 100,
                      conditioned =TRUE, gamma = "jack1")
sac.trop.col <- specaccum(ad.mat.trop, method = "collector", permutations = 100,
                          conditioned =TRUE, gamma = "jack1")


#Plots
op <- par(mfrow = c(2,1),
          cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1 , font.lab = 1, cex.axis = 1, bty = "n")

#gaz
plot(sac.gaz, lty = 2, lwd = 1, ci.type = "polygon", col = "#5e3c99", ci.col = "#998ec3", ci.lty = 0,
     xlim = c(0, 800),
     ylim = c(0, 35))
plot(sac.gaz.col, col = "black", lty = 1, lwd = 1, add = T)
text(400, 10, "Antarctic", cex = 1, col = "#998ec3")

#trop
plot(sac.trop, lty = 2, lwd = 1, ci.type = "polygon", col = "#e66101", ci.col = "#f1a340", ci.lty = 0,
     xlim = c(0, 800),
     ylim = c(0, 35))
plot(sac.trop.col, col = "black", lty = 1, lwd = 1, add = T)
text(400, 10, "Subantarctic", cex = 1, col = "#f1a340")

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


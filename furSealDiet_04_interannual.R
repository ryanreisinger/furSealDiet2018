##Fur Seal Diet
##2016-11-30
##Ryan R Reisinger

## Summer split for annual comparisons

#-----------------------------------------------------

library(plyr)
library(ggplot2)
library(tidyr)
library(vegan)
library(MASS)

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
dat[dat$prey == "Protomytophum bolini", "prey"] <- "Protomyctophum bolini"
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

#Analyse the aggregated groups:
#dat$prey <- dat$group


##Add seasons
dat$month <- strftime(dat$date, format = "%m")
dat$year.month <- strftime(dat$date, format = "%Y-%m")

dat$season <- rep("summer", nrow(dat))
dat[dat$month == "04" |
              dat$month == "05" |
              dat$month == "06" |
              dat$month == "07" |
              dat$month == "08" |
              dat$month == "09", "season"] <- "winter"

#Select only the summer data
dat <- dat[dat$season == "summer", ]

#Assign time perido to each summer
dat$time <- rep(NA, nrow(dat))

dat[dat$date > strptime("2006-09-30", format = "%Y-%m-%d") & dat$date < strptime("2007-04-01", format = "%Y-%m-%d"), "time"] <- "2006/2007"
dat[dat$date > strptime("2007-09-30", format = "%Y-%m-%d") & dat$date < strptime("2008-04-01", format = "%Y-%m-%d"), "time"] <- "2007/2008"
dat[dat$date > strptime("2008-09-30", format = "%Y-%m-%d") & dat$date < strptime("2009-04-01", format = "%Y-%m-%d"), "time"] <- "2008/2009"
dat[dat$date > strptime("2009-09-30", format = "%Y-%m-%d") & dat$date < strptime("2010-04-01", format = "%Y-%m-%d"), "time"] <- "2009/2010"

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
#Separate data
dat.gaz.0607 <- dat[dat$seal == "gaz" & dat$time == "2006/2007", ]
dat.gaz.0708 <- dat[dat$seal == "gaz" & dat$time == "2007/2008", ]
dat.gaz.0809 <- dat[dat$seal == "gaz" & dat$time == "2008/2009", ]
dat.gaz.0910 <- dat[dat$seal == "gaz" & dat$time == "2009/2010", ]

#dat.trop.0607 <- dat[dat$seal == "trop" & dat$time == "2006/2007", ]
dat.trop.0708 <- dat[dat$seal == "trop" & dat$time == "2007/2008", ]
dat.trop.0809 <- dat[dat$seal == "trop" & dat$time == "2008/2009", ]
dat.trop.0910 <- dat[dat$seal == "trop" & dat$time == "2009/2010", ]

#Run scatR to produce the summary tables
gaz.prey.summary.0607 <- scatR(dat = dat.gaz.0607, species = "gaz")
gaz.prey.summary.0708 <- scatR(dat = dat.gaz.0708, species = "gaz")
gaz.prey.summary.0809 <- scatR(dat = dat.gaz.0809, species = "gaz")
gaz.prey.summary.0910 <- scatR(dat = dat.gaz.0910, species = "gaz")

#Tropicalis contain only fish, so scatR doesn't run in curent form
#trop.prey.summary.0607 <- scatR(dat = dat.trop.0607, species = "trop")
trop.prey.summary.0708 <- scatR(dat = dat.trop.0708, species = "trop")
trop.prey.summary.0809 <- scatR(dat = dat.trop.0809, species = "trop")
trop.prey.summary.0910 <- scatR(dat = dat.trop.0910, species = "trop")


#------------------------------------------------------------------------------

#Bootstrap of diet measures.

#------------------------
#gaz
dat.gaz <- dat.gaz.0910
gaz.prey.summary <- gaz.prey.summary.0910


spec.gaz <- gaz.prey.summary[ , 1:2]
spec.gaz[ , 2] <- rep(NA, nrow(spec.gaz))
names(spec.gaz) <- c("prey", "HOLDER")

boot.hold <- list()
samples <- unique(dat.gaz$sample)

system.time(
  for(i in 1:5){ #1000 times
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
gaz.boot.mean <- apply(boot.hold, c(1,2), mean)
gaz.boot.sd <- apply(boot.hold, c(1,2), sd)
gaz.boot.LCL <- apply(boot.hold, c(1,2), quantile, probs = .025)
gaz.boot.UCL <- apply(boot.hold, c(1,2), quantile, probs = .975)

gaz.boot.LCL <- as.data.frame(gaz.boot.LCL)
names(gaz.boot.LCL) <- names(gaz.prey.summary[ ,c(2:9)])
gaz.boot.LCL$prey <- spec.gaz$prey
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
                      "na.weighted.UCL" = gaz.boot.UCL$na.percent.mean)

saveRDS(gaz.sum, "gaz_sum_0910.rds")





#------------------------
#trop
dat.trop <- dat.trop.0910
trop.prey.summary <- trop.prey.summary.0910


spec.trop <- trop.prey.summary[ , 1:2]
spec.trop[ , 2] <- rep(NA, nrow(spec.trop))
names(spec.trop) <- c("prey", "HOLDER")

boot.hold <- list()
samples <- unique(dat.trop$sample)

system.time(
  for(i in 1:5){ #1000 times
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
trop.boot.mean <- apply(boot.hold, c(1,2), mean)
trop.boot.sd <- apply(boot.hold, c(1,2), sd)
trop.boot.LCL <- apply(boot.hold, c(1,2), quantile, probs = .025)
trop.boot.UCL <- apply(boot.hold, c(1,2), quantile, probs = .975)

trop.boot.LCL <- as.data.frame(trop.boot.LCL)
names(trop.boot.LCL) <- names(trop.prey.summary[ ,c(2:9)])
trop.boot.LCL$prey <- spec.trop$prey
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
                      "na.weighted.UCL" = trop.boot.UCL$na.percent.mean)

saveRDS(trop.sum, "trop_sum_0910.rds")


#------------------------------------------------------------------------------
## Multiple regression

#get overall gaz diet for prey spec

gaz.over <- readRDS("gaz_sum_summer.rds")
gaz.over <- gaz.over[ , c("prey", "rank.na")]

gaz.0607 <- readRDS("gaz_sum_0607.rds")
gaz.0708 <- readRDS("gaz_sum_0708.rds")
gaz.0809 <- readRDS("gaz_sum_0809.rds")
gaz.0910 <- readRDS("gaz_sum_0910.rds")

#merge with overall diet to ensure all prey is present each year
gaz.0607 <- merge(gaz.0607, gaz.over, by = "prey", all = T)
gaz.0708 <- merge(gaz.0708, gaz.over, by = "prey", all = T)
gaz.0809 <- merge(gaz.0809, gaz.over, by = "prey", all = T)
gaz.0910 <- merge(gaz.0910, gaz.over, by = "prey", all = T)

#add year
gaz.0607$year <- rep("2006/2007", nrow(gaz.0607))
gaz.0708$year <- rep("2007/2008", nrow(gaz.0708))
gaz.0809$year <- rep("2008/2009", nrow(gaz.0809))
gaz.0910$year <- rep("2009/2010", nrow(gaz.0910))

#bind all
gaz <- rbind(gaz.0607, gaz.0708, gaz.0809, gaz.0910)
gaz <- gaz[ , c("year", "prey", "na.percent", "rank.na")]

#fill in NA with 0
gaz[is.na(gaz$na.percent), "na.percent"] <- 0

#order
#gaz<-gaz[order(gaz$rank.na, decreasing = F), ]
#gaz$prey <- factor(gaz$prey, levels = gaz$prey)


##PLOT
#get colours
# library(RColorBrewer)
# library(viridis)
# 
# #Make virids scale
# #https://gist.github.com/hrbrmstr/f31899d067300d621baa
# 
# viridis_pal <- function(alpha=1) {
#   function(n) {
#     viridis(n, alpha)
#   }
# }
# 
# scale_fill_viridis <- function (..., alpha=1, discrete=TRUE) {
#   if (discrete) {
#     discrete_scale("fill", "viridis", viridis_pal(alpha), ...)
#   } else {
#     scale_fill_gradientn(colours = viridis(256, alpha), ...)
#   }
# }
# 
# ggplot(data=gaz, aes(x=year, y=na.percent, group=prey, fill = prey)) +
#   geom_area(position = "stack", colour = "black")
# 
# ggplot(gaz, aes(x=year, y=na.percent, fill=prey, group = prey)) +
#   geom_area(colour="black", size=.2) +
#   scale_fill_viridis()
# 
# ggplot(gaz, aes(x=year, y=na.percent, fill=prey, group = prey)) +
#   geom_area(colour="black", size=.2) +
#   scale_fill_brewer(palette = "PuBuGn")
# 
# ggplot(gaz, aes(x=year, y=na.percent, fill=prey, group = prey)) +
#   geom_area(colour="black", size=.2) +
#   scale_fill_grey()


#create palette, purples

purp.pal <- rep(c("#c3bcdc", "#998ec3"), 13)
or.pal <- rep(c("#f8d1a0", "#f1a340"), 13)

p1 <- ggplot(gaz, aes(x=year, y=na.percent, fill=reorder(prey, rank.na), group = reorder(prey, rank.na))) +
  geom_area(colour="black", size=.2) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  labs(x = "Year", y = "Numerical abundance (%)") +
  scale_fill_manual(values = paste(purp.pal)) +
  theme_bw() +
  guides(fill = "none") +
  theme(axis.text = element_text(colour = "black"),
        panel.grid = element_blank())

p1

##Trop
#get overall gaz diet for prey spec

trop.over <- readRDS("trop_sum_summer.rds")
trop.over <- trop.over[ , c("prey", "rank.na")]

#trop.0607 <- readRDS("trop_sum_0607.rds")
trop.0708 <- readRDS("trop_sum_0708.rds")
trop.0809 <- readRDS("trop_sum_0809.rds")
trop.0910 <- readRDS("trop_sum_0910.rds")

#merge with overall diet to ensure all prey is present each year
#trop.0607 <- merge(trop.0607, trop.over, by = "prey", all = T)
trop.0708 <- merge(trop.0708, trop.over, by = "prey", all = T)
trop.0809 <- merge(trop.0809, trop.over, by = "prey", all = T)
trop.0910 <- merge(trop.0910, trop.over, by = "prey", all = T)

#create dummy 2006/2007 for trop
trop.0607 <- trop.0708
trop.0607$na.percent <- rep(NA, nrow(trop.0607))

#add year
trop.0607$year <- rep("2006/2007", nrow(trop.0607))
trop.0708$year <- rep("2007/2008", nrow(trop.0708))
trop.0809$year <- rep("2008/2009", nrow(trop.0809))
trop.0910$year <- rep("2009/2010", nrow(trop.0910))

#bind all
trop <- rbind(trop.0607, trop.0708, trop.0809, trop.0910)
trop <- trop[ , c("year", "prey", "na.percent", "rank.na")]

#fill in NA with 0, but keep 2006/2007 NA
trop[is.na(trop$na.percent), "na.percent"] <- 0
trop[trop$year == "2006/2007", "na.percent"] <- NA

#order
#trop<-trop[order(trop$rank.na, decreasing = F), ]
#trop$prey <- factor(trop$prey, levels = trop$prey)


##PLOT

purp.pal <- rep(c("#c3bcdc", "#998ec3"), 13)
or.pal <- rep(c("#f8d1a0", "#f1a340"), 14)

p2 <- ggplot(trop, aes(x=year, y=na.percent, fill=reorder(prey, rank.na), group = reorder(prey, rank.na))) +
  geom_area(colour="black", size=.2) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  labs(x = "Year", y = "Numerical abundance (%)") +
  scale_fill_manual(values = paste(or.pal)) +
  theme_bw() +
  guides(fill = "none") +
  theme(axis.text = element_text(colour = "black"),
        panel.grid = element_blank())

p2


library(gridExtra)
grid.arrange(p1, p2, ncol = 2)
#------------------------------------------------------------------------------

#regression
library(car)
library(reshape2)

#gaz
gaz.cop <- gaz
gaz.cop$rank.na <- NULL
gaz.cop$year <- 1:4
gaz.cop <- melt(gaz.cop, id.vars = c("year", "prey"), measure.vars = "na.percent")
gaz.cop <- dcast(gaz.cop, year ~ prey)
names(gaz.cop) <- make.names(names(gaz.cop), unique=TRUE)

gaz.lm <- lm(formula = cbind(gaz.cop$Maurolicus.muelleri,
                             gaz.cop$Lampichthys.procerus,
                             gaz.cop$Electrona.sp.,
                             gaz.cop$Lepidonotothen.larseni,
                             gaz.cop$Paralepis.atlantica,
                             gaz.cop$Krefftichthys.anderssoni,
                             gaz.cop$Protomyctophum.bolini,
                             gaz.cop$Scopelosaurus.ahlstromi,
                             gaz.cop$Dissostichus.eleginoides,
                             gaz.cop$Gymnoscopelus.sp.,
                             gaz.cop$Notothenia.squamifron,
                             gaz.cop$Cephalopod,
                             gaz.cop$Crustacean,
                             gaz.cop$Electrona.subaspera,
                             gaz.cop$Penguin,
                             gaz.cop$Symbolophorus.boops,
                             gaz.cop$Electrona.antarctica,
                             gaz.cop$Protomyctophum.choriodon,
                             gaz.cop$Gymnoscopelus.bolini,
                             gaz.cop$Metelectrona.ventralis,
                             gaz.cop$Electrona.carlsbergi,
                             gaz.cop$Gymnoscopelus.nicholsi,
                             gaz.cop$Gymnoscopelus.fraseri,
                             gaz.cop$Protomyctophum.tenisoni,
                             gaz.cop$Gymnoscopelus.piabilis) ~ gaz.cop$year)

Manova(gaz.lm)
summary(manova(gaz.lm), tol = 0) #NB - manova is rank deficient since responses are correlated

gaz.lm$effects
gaz.lm.sum <- summary(gaz.lm)
gaz.prey <- names(gaz.cop[ ,-1])
s <- list()
for(i in 1:length(gaz.prey)){
t <- gaz.lm.sum[[i]]$coefficients[2, ]
s[i] <- list(t)
}
s <- as.data.frame(do.call(rbind, s))
s$prey <- gaz.prey
s.gaz <- s
write.csv(s.gaz, "gaz_preychange_reg.csv", row.names = F)

#trop
trop.cop <- trop
trop.cop$rank.na <- NULL
trop.cop$year <- 1:4
trop.cop <- melt(trop.cop, id.vars = c("year", "prey"), measure.vars = "na.percent")
trop.cop <- dcast(trop.cop, year ~ prey)
names(trop.cop) <- make.names(names(trop.cop), unique=TRUE)

trop.lm <- lm(formula = cbind(trop.cop$Bathylagus.gracilis,
                              trop.cop$Photichthys.argenteus,
                              trop.cop$Cephalopod,
                              trop.cop$Krefftichthys.anderssoni,
                              trop.cop$Bathylagus.sp.,
                              trop.cop$Maurolicus.muelleri,
                              trop.cop$Paralepis.sp.,
                              trop.cop$Dissostichus.eleginoides,
                              trop.cop$Sternoptychidae,
                              trop.cop$Notothenia.squamifrons,
                              trop.cop$Protomyctophum.bolini,
                              trop.cop$Lampichthys.procerus,
                              trop.cop$Gymnoscopelus.sp.,
                              trop.cop$Gymnoscopelus.braueri,
                              trop.cop$Paralepis.atlantica,
                              trop.cop$Electrona.antarctica,
                              trop.cop$Symbolophorus.boops,
                              trop.cop$Electrona.subaspera,
                              trop.cop$Scopelosaurus.ahlstromi,
                              trop.cop$Protomyctophum.choriodon,
                              trop.cop$Gymnoscopelus.bolini,
                              trop.cop$Electrona.carlsbergi,
                              trop.cop$Metelectrona.ventralis,
                              trop.cop$Gymnoscopelus.nicholsi,
                              trop.cop$Gymnoscopelus.fraseri,
                              trop.cop$Gymnoscopelus.piabilis,
                              trop.cop$Protomyctophum.tenisoni)
              ~ trop.cop$year)

Manova(trop.lm)
summary(manova(trop.lm), tol = 0) #NB - manova is rank deficient since responses are correlated

trop.lm$effects
trop.lm.sum <- summary(trop.lm)
trop.prey <- names(trop.cop[ ,-1])
s <- list()
for(i in 1:length(trop.prey)){
  t <- trop.lm.sum[[i]]$coefficients[2, ]
  s[i] <- list(t)
}
s <- as.data.frame(do.call(rbind, s))
s$prey <- trop.prey
s.trop <- s
write.csv(s.trop, "trop_preychange_reg.csv", row.names = F)


s.sum <- merge(s.gaz, s.trop, by = "prey", all = T)
write.csv(s.sum, "preychange_reg.csv", row.names = F)

#------------------------------------------------------------------------------
## Correlations between the two species

#remove 2006-2007 data from trop
trop <- trop[trop$year != "2006/2007", ]
gaz <- gaz[gaz$year != "2006/2007", ]

cor(as.numeric(gaz[gaz$prey == "Cephalopod", "na.percent"]),
    as.numeric(trop[trop$prey == "Cephalopod", "na.percent"]))

cor(as.numeric(gaz[gaz$prey == "Dissostichus eleginoides", "na.percent"]),
    as.numeric(trop[trop$prey == "Dissostichus eleginoides", "na.percent"]))

cor(as.numeric(gaz[gaz$prey == "Electrona antarctica", "na.percent"]),
    as.numeric(trop[trop$prey == "Electrona antarctica", "na.percent"]))

cor(as.numeric(gaz[gaz$prey == "Gymnoscopelus bolini", "na.percent"]),
    as.numeric(trop[trop$prey == "Gymnoscopelus bolini", "na.percent"]))

cor(as.numeric(gaz[gaz$prey == "Gymnoscopelus fraseri", "na.percent"]),
    as.numeric(trop[trop$prey == "Gymnoscopelus fraseri", "na.percent"]))

cor(as.numeric(gaz[gaz$prey == "Gymnoscopelus nicholsi", "na.percent"]),
    as.numeric(trop[trop$prey == "Gymnoscopelus nicholsi", "na.percent"]))

cor(as.numeric(gaz[gaz$prey == "Gymnoscopelus piabilis", "na.percent"]),
    as.numeric(trop[trop$prey == "Gymnoscopelus piabilis", "na.percent"]))

cor(as.numeric(gaz[gaz$prey == "Gymnoscopelus sp.", "na.percent"]),
    as.numeric(trop[trop$prey == "Gymnoscopelus sp.", "na.percent"]))

cor(as.numeric(gaz[gaz$prey == "Lampichthys procerus", "na.percent"]),
    as.numeric(trop[trop$prey == "Lampichthys procerus", "na.percent"]))

cor(as.numeric(gaz[gaz$prey == "Maurolicus muelleri", "na.percent"]),
    as.numeric(trop[trop$prey == "Maurolicus muelleri", "na.percent"]))

cor(as.numeric(gaz[gaz$prey == "Metelectrona ventralis", "na.percent"]),
    as.numeric(trop[trop$prey == "Metelectrona ventralis", "na.percent"]))

cor(as.numeric(gaz[gaz$prey == "Notothenia squamifrons", "na.percent"]),
    as.numeric(trop[trop$prey == "Notothenia squamifrons", "na.percent"]))

cor(as.numeric(gaz[gaz$prey == "Krefftichthys anderssoni", "na.percent"]),
    as.numeric(trop[trop$prey == "Krefftichthys anderssoni", "na.percent"]))

cor(as.numeric(gaz[gaz$prey == "Paralepis atlantica", "na.percent"]),
    as.numeric(trop[trop$prey == "Paralepis atlantica", "na.percent"]))

cor(as.numeric(gaz[gaz$prey == "Protomyctophum bolini", "na.percent"]),
    as.numeric(trop[trop$prey == "Protomyctophum bolini", "na.percent"]))

cor(as.numeric(gaz[gaz$prey == "Protomyctophum choriodon", "na.percent"]),
    as.numeric(trop[trop$prey == "Protomyctophum choriodon", "na.percent"]))

cor(as.numeric(gaz[gaz$prey == "Protomyctophum tenisoni", "na.percent"]),
    as.numeric(trop[trop$prey == "Protomyctophum tenisoni", "na.percent"]))

cor(as.numeric(gaz[gaz$prey == "Scopelosaurus ahlstromi", "na.percent"]),
    as.numeric(trop[trop$prey == "Scopelosaurus ahlstromi", "na.percent"]))

cor(as.numeric(gaz[gaz$prey == "Symbolophorus boops", "na.percent"]),
    as.numeric(trop[trop$prey == "Symbolophorus boops", "na.percent"]))





#------------------------------------------------------------------------------
## Diet overlap
library(spaa)

#run for each year:

#2007/2008
gaz.pi <- gaz.prey.summary.0708[ , c("prey", "na.number.all")]
names(gaz.pi) <- c("prey", "gaz")

trop.pi <- trop.prey.summary.0708[ , c("prey", "na.number.all")]
names(trop.pi) <- c("prey", "trop")

pi <- merge(gaz.pi, trop.pi, by = "prey", all = T)
pi[is.na(pi$gaz), "gaz"] <- 0
pi[is.na(pi$trop), "trop"] <- 0
pi <- pi[ , -1]

pi <- t(pi)
pi <- as.data.frame(pi)

overlap.pianka.0708 <- niche.overlap.boot(mat = t(pi), method = "pianka", times = 9999)

overlap.schoener.0708 <- niche.overlap.boot(mat = t(pi), method = "schoener", times = 9999)

#2008/2009
gaz.pi <- gaz.prey.summary.0809[ , c("prey", "na.number.all")]
names(gaz.pi) <- c("prey", "gaz")

trop.pi <- trop.prey.summary.0809[ , c("prey", "na.number.all")]
names(trop.pi) <- c("prey", "trop")

pi <- merge(gaz.pi, trop.pi, by = "prey", all = T)
pi[is.na(pi$gaz), "gaz"] <- 0
pi[is.na(pi$trop), "trop"] <- 0
pi <- pi[ , -1]

pi <- t(pi)
pi <- as.data.frame(pi)

overlap.pianka.0809 <- niche.overlap.boot(mat = t(pi), method = "pianka", times = 9999)

overlap.schoener.0809 <- niche.overlap.boot(mat = t(pi), method = "schoener", times = 9999)


#2009/2010
gaz.pi <- gaz.prey.summary.0910[ , c("prey", "na.number.all")]
names(gaz.pi) <- c("prey", "gaz")

trop.pi <- trop.prey.summary.0910[ , c("prey", "na.number.all")]
names(trop.pi) <- c("prey", "trop")

pi <- merge(gaz.pi, trop.pi, by = "prey", all = T)
pi[is.na(pi$gaz), "gaz"] <- 0
pi[is.na(pi$trop), "trop"] <- 0
pi <- pi[ , -1]

pi <- t(pi)
pi <- as.data.frame(pi)

overlap.pianka.0910 <- niche.overlap.boot(mat = t(pi), method = "pianka", times = 9999)

overlap.schoener.0910 <- niche.overlap.boot(mat = t(pi), method = "schoener", times = 9999)


#put together

overlap.pianka <- rbind.data.frame(overlap.pianka.0708, overlap.pianka.0809, overlap.pianka.0910)
names(overlap.pianka) <- c("Obs", "Boot mean", "Boot SD", "Boot LCL", "Boot UCL", "Times")
overlap.pianka$year <- c("2007/2008", "2008/2009", "2009/2010")
saveRDS(overlap.pianka, "overlap_pianka_summer.RDS")

overlap.schoener <- rbind.data.frame(overlap.schoener.0708, overlap.schoener.0809, overlap.schoener.0910)
names(overlap.schoener) <- c("Obs", "Boot mean", "Boot SD", "Boot LCL", "Boot UCL", "Times")
overlap.schoener$year <- c("2007/2008", "2008/2009", "2009/2010")
saveRDS(overlap.schoener, "overlap_schoener_summer.RDS")



#Plot

names(overlap.pianka) <- make.names(names(overlap.pianka))

ggplot(overlap.pianka, aes(x = year, y = Obs)) + geom_point() +
  geom_errorbar(aes(x = year, ymin = Boot.LCL, ymax = Boot.UCL), width = 0.3, size = 0.5) +
  theme_bw() +
  theme(axis.text = element_text(colour = "black"),
        panel.grid = element_blank())
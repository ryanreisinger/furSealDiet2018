test <- group_by(dat, sample.id, prey)
test <- filter(test, group == "Fish")
test2 <- summarize(test, nprey = n())
testf <- filter(test2, nprey == 1)

is.odd <- function(x) x %% 2 != 0
test2$odd <- is.odd(test2$nprey)

test.odd <- filter(test2, odd == TRUE)
test.even <- filter(test2, odd == FALSE)

# Let 'prey occurrence' be an instance of given prey species in given sample (scat)
# total fish prey occurrences = 4084 (22,549 otoliths)
# single fish prey occurences = 1247 / 4084 (30.53 %) (single otoliths of a given prey in a given scat)
# odd numbered fish prey occurrences = 2341 / 4084 (57.32 %) (odd number of otoliths of a given prey in a given scat)
# even numbered fish prey occurences = 1743 / 4084 (42.68 %) (even number of otoliths of a given prey in a given scat)
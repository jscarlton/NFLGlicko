# Project:      NFL Glicko Analysis
# File:         nflGlicko2014.r
# Date created: 2015-06-30
# Task:         Initial Glicko ratings for 2014 NFL data
library(PlayerRatings)

# Import the 2014 NFL data. Will do other data later.
nfl2014 <- read.csv("./data/nfl 2014.csv", header = TRUE)

# PlayerRatings asks for a dataframe with:
#   (1) a numeric vector denoting the time period in which the game took place
#   (2) a numeric or character identifier for player one
#   (3) a numeric or character identifier for player two and
#   (4) the result of the game expressed as a number, typically equal to one for a player one win, zero for a player two win and one half for a draw.

# Calculate the results column.
nfl2014$result <- ifelse(nfl2014$home_score>nfl2014$visitors_score, 1, ifelse(nfl2014$home_score<nfl2014$visitors_score,0,0.5))

# Convert the home team and away team to characters
nfl2014$home_team <- as.character(nfl2014$home_team)
nfl2014$visiting_team <- as.character(nfl2014$visiting_team)

# Create the dataframe for the glicko analysis
nfl2014Glicko <- nfl2014[,c(2,4,7,8)]

# Glicko analysis for the full 2014 season
nflGlicko <- glicko(nfl2014Glicko)

# Week-by-week Glicko analysis for 2014 using the prior week's ratings as the status. No gamma setting for now: will need to optimize that later. When I do prior seasons, can also use it for the status input for week 1. Or maybe that + some regression to the mean factor.
glicko20141 <- glicko(nfl2014Glicko[nfl2014Glicko$week == 1, ])
glicko20142 <- glicko(nfl2014Glicko[nfl2014Glicko$week == 2, ], status = glicko20141$ratings)
glicko20143 <- glicko(nfl2014Glicko[nfl2014Glicko$week == 3, ], status = glicko20142$ratings)
glicko20144 <- glicko(nfl2014Glicko[nfl2014Glicko$week == 4, ], status = glicko20143$ratings)
glicko20145 <- glicko(nfl2014Glicko[nfl2014Glicko$week == 5, ], status = glicko20144$ratings)
glicko20146 <- glicko(nfl2014Glicko[nfl2014Glicko$week == 6, ], status = glicko20145$ratings)
glicko20147 <- glicko(nfl2014Glicko[nfl2014Glicko$week == 7, ], status = glicko20146$ratings)
glicko20148 <- glicko(nfl2014Glicko[nfl2014Glicko$week == 8, ], status = glicko20147$ratings)
glicko20149 <- glicko(nfl2014Glicko[nfl2014Glicko$week == 9, ], status = glicko20148$ratings)
glicko201410 <- glicko(nfl2014Glicko[nfl2014Glicko$week == 10, ], status = glicko20149$ratings)
glicko201411 <- glicko(nfl2014Glicko[nfl2014Glicko$week == 11, ], status = glicko201410$ratings)
glicko201412 <- glicko(nfl2014Glicko[nfl2014Glicko$week == 12, ], status = glicko201411$ratings)
glicko201413 <- glicko(nfl2014Glicko[nfl2014Glicko$week == 13, ], status = glicko201412$ratings)
glicko201414 <- glicko(nfl2014Glicko[nfl2014Glicko$week == 14, ], status = glicko201413$ratings)
glicko201415 <- glicko(nfl2014Glicko[nfl2014Glicko$week == 15, ], status = glicko201414$ratings)
glicko201416 <- glicko(nfl2014Glicko[nfl2014Glicko$week == 16, ], status = glicko201415$ratings)
glicko201417 <- glicko(nfl2014Glicko[nfl2014Glicko$week == 17, ], status = glicko201416$ratings)

# Merge into a grand 2014 dataset

## First: rename the rating variable to incorporate the week
colnames(glicko20141$ratings)[2] <- "week1Rating"
colnames(glicko20142$ratings)[2] <- "week2Rating"
colnames(glicko20143$ratings)[2] <- "week3Rating"
colnames(glicko20144$ratings)[2] <- "week4Rating"
colnames(glicko20145$ratings)[2] <- "week5Rating"
colnames(glicko20146$ratings)[2] <- "week6Rating"
colnames(glicko20147$ratings)[2] <- "week7Rating"
colnames(glicko20148$ratings)[2] <- "week8Rating"
colnames(glicko20149$ratings)[2] <- "week9Rating"
colnames(glicko201410$ratings)[2] <- "week10Rating"
colnames(glicko201411$ratings)[2] <- "week11Rating"
colnames(glicko201412$ratings)[2] <- "week12Rating"
colnames(glicko201413$ratings)[2] <- "week13Rating"
colnames(glicko201414$ratings)[2] <- "week14Rating"
colnames(glicko201415$ratings)[2] <- "week15Rating"
colnames(glicko201416$ratings)[2] <- "week16Rating"
colnames(glicko201417$ratings)[2] <- "week17Rating"

## Same with Deviation
## First: rename the rating variable to incorporate the week,
colnames(glicko20141$ratings)[3] <- "week1Deviation"
colnames(glicko20142$ratings)[3] <- "week2Deviation"
colnames(glicko20143$ratings)[3] <- "week3Deviation"
colnames(glicko20144$ratings)[3] <- "week4Deviation"
colnames(glicko20145$ratings)[3] <- "week5Deviation"
colnames(glicko20146$ratings)[3] <- "week6Deviation"
colnames(glicko20147$ratings)[3] <- "week7Deviation"
colnames(glicko20148$ratings)[3] <- "week8Deviation"
colnames(glicko20149$ratings)[3] <- "week9Deviation"
colnames(glicko201410$ratings)[3] <- "week10Deviation"
colnames(glicko201411$ratings)[3] <- "week11Deviation"
colnames(glicko201412$ratings)[3] <- "week12Deviation"
colnames(glicko201413$ratings)[3] <- "week13Deviation"
colnames(glicko201414$ratings)[3] <- "week14Deviation"
colnames(glicko201415$ratings)[3] <- "week15Deviation"
colnames(glicko201416$ratings)[3] <- "week16Deviation"
colnames(glicko201417$ratings)[3] <- "week17Deviation"

# For now, a manual merge, week-by-week. There has to be a better way to do this.
merge2 <- merge(glicko20141$ratings, glicko20142$ratings, "Player")
merge3 <- merge(merge2, glicko20143$ratings, "Player")
merge4 <- merge(merge3, glicko20144$ratings, "Player")
merge5 <- merge(merge4, glicko20145$ratings, "Player")
merge6 <- merge(merge5, glicko20146$ratings, "Player")
merge7 <- merge(merge6, glicko20147$ratings, "Player")
merge8 <- merge(merge7, glicko20148$ratings, "Player")
merge9 <- merge(merge8, glicko20149$ratings, "Player")
merge10 <- merge(merge9, glicko201410$ratings, "Player")
merge11 <- merge(merge10, glicko201411$ratings, "Player")
merge12 <- merge(merge11, glicko201412$ratings, "Player")
merge13 <- merge(merge12, glicko201413$ratings, "Player")
merge14 <- merge(merge13, glicko201414$ratings, "Player")
merge15 <- merge(merge14, glicko201415$ratings, "Player")
merge16 <- merge(merge15, glicko201416$ratings, "Player")
merge17 <- merge(merge16, glicko201417$ratings, "Player")
merge17

## drop extraneous variables, copying the pattern from http://stackoverflow.com/a/4609568
merged2014Ratings <- merge17[, -grep("\\.x", names(merge17))]
merged2014Ratings <- merged2014Ratings[, -grep("\\.y", names(merged2014Ratings))]
merged2014Ratings

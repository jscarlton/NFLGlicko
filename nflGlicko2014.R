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
nflGlicko

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

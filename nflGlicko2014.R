# Project:      NFL Glicko Analysis
# File:         nflGlicko2014.r
# Date created: 2015-06-30
# Task:         Initial Glicko ratings for 2014 NFL data
library(PlayerRatings)

# Import the 2014 NFL data.
nfl2014 <- read.csv("./data/nfl 2014.csv", header = TRUE)

# Import the 2013 NFL data to use as a seed for 2014.
nfl2013 <- read.csv("./data/nfl 2013.csv", header = TRUE)

# PlayerRatings asks for a dataframe with:
#   (1) a numeric vector denoting the time period in which the game took place
#   (2) a numeric or character identifier for player one
#   (3) a numeric or character identifier for player two and
#   (4) the result of the game expressed as a number, typically equal to one for a player one win, zero for a player two win and one half for a draw.

# Calculate the results column.
nfl2013$result <- ifelse(nfl2013$home_score>nfl2013$visitors_score, 1, ifelse(nfl2013$home_score<nfl2013$visitors_score,0,0.5))
nfl2014$result <- ifelse(nfl2014$home_score>nfl2014$visitors_score, 1, ifelse(nfl2014$home_score<nfl2014$visitors_score,0,0.5))

# Convert the home team and away team to characters
nfl2013$home_team <- as.character(nfl2013$home_team)
nfl2013$visiting_team <- as.character(nfl2013$visiting_team)

nfl2014$home_team <- as.character(nfl2014$home_team)
nfl2014$visiting_team <- as.character(nfl2014$visiting_team)

# Glicko analysis for the full 2013 season. Note that only 4 columns from nfl2014 are used.
nflGlicko2013 <- glicko(nfl2013[,c(2,4,7,8)], history = TRUE)

# Since some regression to the mean would be expected, let's mean-revert
# the 2013 ratings and deviations before using them as the status for the initial 2014 ratings.

# Pull out just the Player, Rating, and Deviation
seed2013 <- nflGlicko2013$ratings[,c(1,2,3)]

# Calculate the mean Rating and Deviation
seed2013RatingMean <- mean(seed2013$Rating)
seed2013DeviationMean <- mean (seed2013$Deviation)

# Subtract 25% of the difference between good teams and the mean ratings and deviations and add 25% of the difference for bad teams

seed2013Reverted <- seed2013

seed2013Reverted$ReversionAmount <- seed2013Reverted$Rating - seed2013RatingMean
seed2013Reverted$Rating <- seed2013Reverted$Rating - (.25*seed2013Reverted$ReversionAmount)

# Same for deviations
seed2013Reverted$ReversionAmountD <- seed2013Reverted$Deviation - seed2013DeviationMean
seed2013Reverted$Deviation <- seed2013Reverted$Deviation - (.25*seed2013Reverted$ReversionAmountD)

# Glicko analysis for 2014 season
nflGlicko2014 <- glicko(nfl2014[,c(2,4,7,8)], history = TRUE, status = seed2013Reverted)
# Plot of the NFC South as a proof of concept. Colors from teamcolorcodes.com, except Tampa which was pulled from a picture of the creamsicle uniforms.
plot(nflGlicko2014, players = c("Saints", "Falcons", "Buccaneers", "Panthers"), lty=1, col = c("Black", "#BD0D18", "#E47924", "#0088CE"), main = "2014 NFC South Glicko Ratings", xlab = "Week", xaxt = "n")
abline(h = 2222.673, lty=2)
axis(1,at = seq(1,21, by = 1), label = seq(1,21, by = 1), cex.axis = 0.8)
text(20,2213, "Panthers", cex = 0.8)
text(20,2154, "Saints", cex = 0.8)
text(20,2119, "Falcons", cex = 0.8)
text(20,1867, "Buccaneers :(", cex = 0.8)
text(15,2242, "League average", cex = 0.8) # hand-calculated

# Plot of point estimates ± the deviation

## set numbers for x-axis categories
x <- 1:32

## set the confidence intervals:
upperCI <-nflGlicko2014$ratings$Rating + nflGlicko2014$ratings$Deviation
lowerCI <-nflGlicko2014$ratings$Rating - nflGlicko2014$ratings$Deviation
## Create the labels
xLabels <- c(nflGlicko2014$ratings$Player)
## now the plot
plot(nflGlicko2014$ratings$Rating~x,,cex=1.5,xaxt='n',xlim=c(1,32), ylim = c(1650,2650), xlab='',ylab='Glicko rating', main='Final 2014 Glicko ratings',col='gray',pch=16)
axis(1, at=x, cex.axis = 0.5, labels = FALSE)
text (1:32, par("usr")[3] - 0.25, srt = 90, adj = 1.3,
      labels = xLabels, xpd = TRUE, cex = 0.8)
arrows(x,lowerCI,x,upperCI,code=3,length=0.2,angle=90,col='black')
abline(h = 2222.673, lty=2) #hand-calcuated league average
text(5, 1660, "whodatreport.com", cex = 0.6)

# Vertical plot of point estimates ± the deviation.
## set numbers for y-axis categories. Go in reverse order so the best team is on top.
y <- 32:1

## Create the labels
yLabels <- c(nflGlicko2014$ratings$Player)

## now the plot. Notice the use of rev() to reverse the order so that the best team is on top.
plot(x~rev(nflGlicko2014$ratings$Rating),,cex=1.5,yaxt='n',ylim=c(1,32), xlim = c(1650,2650), ylab='',xlab='Glicko rating', main='Final 2014 Glicko ratings',col='gray',pch=16)
axis(2, at=y, labels = yLabels, las = 2, cex.axis = 1)
arrows(lowerCI,y,upperCI,y, code=3,length=0.2,angle=90,col='black')
abline(v = 2222.673, lty=2) #hand-calcuated league average
text(2560,0.5, "whodatreport.com", cex = 0.6)
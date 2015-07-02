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

# Glicko analysis for the full 2014 season. Note that only 4 columns from nfl2014 are used.
nflGlicko2014 <- glicko(nfl2014[,c(2,4,7,8)], history = TRUE)

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
## set numbers for y-axis categories
y <- 1:32

## Create the labels
yLabels <- c(nflGlicko2014$ratings$Player)
## now the plot
plot(x~nflGlicko2014$ratings$Rating,,cex=1.5,yaxt='n',ylim=c(1,32), xlim = c(1650,2650), ylab='',xlab='Glicko rating', main='Final 2014 Glicko ratings',col='gray',pch=16)
axis(2, at=y, cex.axis = 0.5, labels = FALSE)
text (par("usr")[3] - 0.25,1:32, srt = 90, adj = 1.3,
      labels = yLabels, xpd = TRUE, cex = 0.8)
arrows(x,lowerCI,x,upperCI,code=3,length=0.2,angle=90,col='black')
abline(v = 2222.673, lty=2) #hand-calcuated league average
text(5, 1660, "whodatreport.com", cex = 0.6)
# Samuel Scherl
# 12/26/17
# Calculates the team rankings of a squash season with the input being a CSV file of matches that have been played
# so far in the season. Uses self-consistent ELO for the ranking system

# Import data from CSV file
data <- read.csv("Women'sPreNationals16-17.csv", header=TRUE, stringsAsFactors=FALSE)

# This value will affect the spread of the ELO rankings but not the actual order
PointScale <- 6.67

# Minimum number of matches that need to be played in order for the team to be ranked
CutOff <- 1

# The margin of error that will be tolerated for the point values to converge
convergenceValue <- .01

# Keeps track of time taken to run the program
ptm <- proc.time()

# Only includes fields from input CSV which are relevant
data <- subset(data, select = -c(winnersTeamName, losersTeamName, winner, loser, matchdate, scorecrdid, descr, positionplayed, score, hteamid, vteamid))

# Creates the data frame "competitions" which is made of the results of matches and the probability of that result
competitions <- data.frame(Winner=character(), Loser=character(), Odds=double(), stringsAsFactors=FALSE) 
for (i in 1:nrow(data))
{
  if (i %% 10 == 1)
  {
    if (data[i, 3] > data[i, 4])
    {
      competitions[nrow(competitions) + 1, "Winner"] <- data[i, 1]
      competitions[nrow(competitions), "Loser"] <- data[i, 2]
    }
    else
    {
      competitions[nrow(competitions) + 1, "Winner"] <- data[i, 2]
      competitions[nrow(competitions), "Loser"] <- data[i, 1]
    }
  }
}

# Populates the dataframe "OverAllRecord" with the number of wins and losses for each college, 
helper <- unique(competitions["Loser"])
colnames(helper)[1] <- "Winner"
OverAllRecord <- unique(rbind(competitions["Winner"], helper))
NumColleges <- length(OverAllRecord[,1])
rm(helper)
OverAllRecord$Wins <- 0
OverAllRecord$Losses <- 0
OverAllRecord$Total.Matches <- 0
for (i in 1:NumColleges)
{
  OverAllRecord[i, 2] <- nrow(subset(competitions, Winner == OverAllRecord[i, 1]))
  OverAllRecord[i, 3] <- nrow(subset(competitions, Loser == OverAllRecord[i, 1]))
  OverAllRecord[i, 4] <- OverAllRecord[i, 2] + OverAllRecord[i, 3]
}
OverAllRecord <- OverAllRecord[order(OverAllRecord[,"Winner"]), ]
colnames(OverAllRecord)[1]<-"College"

# Create and populates collegeMatrix of all teams to hold their records against each other
# CollegeMatrix[i, j] holds how many times team i has beaten team j
NamesColleges <- OverAllRecord[,1]
collegeMatrix<-matrix(0, nrow = length(NamesColleges), ncol = length(NamesColleges))
for (i in 1: NumColleges)
{
  for (j in 1:NumColleges)
  {
    collegeMatrix[i, j] <- nrow(subset(competitions, Winner == NamesColleges[i] & Loser == NamesColleges[j]))
  }
}

# A vector that will hold each teams (WinSum - Expected Win Sum)
DiffWinSum <- c(rep(1, NumColleges))

# A vector that will hold the current points for each team, this will be updated with each iteration
Points<-c(rep(1000, NumColleges))

# Formats the output, which is Ratings so it is easier to read
Ratings <- data.frame(NamesColleges, Points, stringsAsFactors=FALSE)
colnames(Ratings)[1]<-"College"

# Checks that all values in DiffWinSum are within the given convergance value range
# This range is from (-convergenceValue to +convergenceValue)
convergence <- function()
{
  return (max(abs(DiffWinSum)) <= convergenceValue)
}

# Returns the probability that team i beats team j
winProb <- function(i, j)
{
  return (1/(1+exp((-(i - j)/PointScale))))
}

#  Populates TotalMatch Matrix so that at spot TotalMatchMatrix[i, j] holds the total amount of times team i has played j
TotalMatchMatrix<-matrix(0, nrow = length(NamesColleges), ncol = length(NamesColleges))
for (i in 1: NumColleges)
{
  for (j in 1:NumColleges)
  {
    TotalMatchMatrix[i, j] <- collegeMatrix[i,j] + collegeMatrix[j,i]
  }
}

# Creates a Win Matrix where WinMatrix[i,j] holds the probability that team I beats team J
WinMatrix<-matrix(0, nrow = length(NamesColleges), ncol = length(NamesColleges))

EditDistance <- data.frame(Iterations=double(), EditDist=double(), stringsAsFactors=FALSE) 

NamesToSymbols <- Ratings
colnames(NamesToSymbols)[2]<-"Symbol"
for (i in 1:NumColleges)
{
  NamesToSymbols[i,2] <- intToUtf8((i + 39))
}

# Loops through and iterates Point's values until convergence is achieved
k <- 0
while (convergence() == FALSE)
{
  # Populates WinMatrix with the win probabilities of team i beating team j
  WinMatrix <- outer(Ratings[,2],Ratings[,2],winProb)
  
  # Populates DiffWinSum with (Wins - ExpectedWins)
  DiffWinSum <- OverAllRecord[,2] - rowSums(TotalMatchMatrix * WinMatrix)
  
  OldRatings <- Ratings
  
  # Updates each team's point values by their respective DiffWinSum
  Ratings[,2] <- Ratings[,2] + DiffWinSum
  
  NewRatings <- Ratings
  
  OldRatings <- merge(OldRatings, NamesToSymbols, by="College")
  NewRatings <- merge(NewRatings, NamesToSymbols, by="College")
  
  OldRatings <- OldRatings[with(OldRatings, order(-Points)),]
  NewRatings <- NewRatings[with(NewRatings, order(-Points)), ]
  
  StringOld <- ""
  for (i in 1:NumColleges)
  {
    StringOld <- paste(StringOld, OldRatings[i,3], sep="")
  }
  StringNew <- ""
  for (i in 1:NumColleges)
  {
    StringNew <- paste(StringNew, NewRatings[i,3], sep="")
  }
  
  numEdits <- adist(StringOld, StringNew)
  EditDistance[nrow(EditDistance) + 1, "Iterations"] <- k
  EditDistance[nrow(EditDistance), "EditDist"] <- numEdits
  if (numEdits > 0 & k > 2500)
  {
    cat(paste("k: ", k, "\n",sep = ""))
    for (i in 1:NumColleges)
    {
      if (substr(StringNew,i,i) != substr(StringOld,i,i))
      {
        cat(paste("Old:", subset(NamesToSymbols, Symbol == substr(StringOld,i,i))[1,1], "--- Index:", i,sep = " " ))
        cat("\n")
        cat(paste("New:", subset(NamesToSymbols, Symbol == substr(StringNew,i,i))[1,1], "--- Index:", i,sep = " " ))
        cat("\n")
      }
    }
    cat("------------------------\n")
  }
  k <- k + 1
}
print(paste0("Iterations for Convergance: ", k))

# Penalizes teams that haven't played enough matches by dropping them to the bottom of the rankings
BelowCutoff <- vector()
for (i in 1:NumColleges)
{
  if (OverAllRecord[i, 4] < CutOff)
  {
    Points[i] <- Points[i] - 5000
  }
}


Ratings <- merge(Ratings, OverAllRecord, by="College")
Ratings <- Ratings[with(Ratings, order(-Points)), ]
Ratings$Rank <- NA
for (i in 1:NumColleges)
{
  Ratings[i, 6] <- i
}

# Goes through the competitions tables and updates each match with the probability of the result
for (i in 1:nrow(competitions))
{
  pointsWinner <- which(Ratings$College == toString(competitions[i, 1]))
  pointsLoser <- which(Ratings$College == toString(competitions[i, 2]))
  competitions[i, 3] <- winProb(Ratings[pointsWinner, 2], Ratings[pointsLoser, 2])
}

# Orders the Ratings file so it's easier to read and adding best win and worst losses
Ratings <- Ratings[c(6,1,2,3,4)]
Ratings$BestWin_1 <- NA
Ratings$BestWin_2 <- NA
Ratings$BestWin_3 <- NA
Ratings$WorstLoss_1 <- NA
Ratings$WorstLoss_2 <- NA
Ratings$WorstLoss_3 <- NA
for (i in 1:NumColleges)
{
  # Adding Best Wins
  helper <- subset(competitions, Winner == Ratings[i, 2])
  helper <- helper[with(helper, order(Odds)),]
  for (j in 1:3)
  {
    if (j <= nrow(helper))
    {
      Ratings[i, 5 + j] <- helper[j, 2]
    }
  }
  # Adding Worst Losses
  helper <- subset(competitions, Loser == Ratings[i, 2])
  helper <- helper[with(helper, order(-Odds)),]
  for (j in 1:3)
  {
    if (j <= nrow(helper))
    {
      Ratings[i, 8 + j] <- helper[j, 1]
    }
  }
}


# Prints out time taked to run the program
cat("\n")
print(proc.time() - ptm)

# Writes the output to a CSV file
write.csv(EditDistance, file = "output.csv", row.names=FALSE, na="")

# Removes varaibles from memory which are unnecessary
rm(BelowCutoff, DiffWinSum, i, convergenceValue, OverAllRecord, helper)
rm(pointsLoser, pointsWinner)
rm(convergence, winProb, Points, TotalMatchMatrix, WinMatrix)
rm(ptm, CutOff, k, NamesColleges, j, NumColleges, collegeMatrix, PointScale)
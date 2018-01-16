# Samuel Scherl
# 1/12/18
# Calculates the team rankings of a squash season with the input being a CSV file of matches that have been played
# so far in the season. Uses self-consistent ELO for the ranking system

#Get from user whether diagnostics will be printed
cat("\n")
cat("Would you like diagnostic information to be printed: ")
diagnostic <- readLines("stdin", n=1)
if (diagnostic == "Yes" | diagnostic == "yes" | diagnostic == "Y" | diagnostic == "y")
{
  diagnostic <- TRUE
}

# Get name of file from user and imports data from the given file
cat("\n")
cat("Input the name of the file to be read: ")
FileName <- readLines("stdin", n=1)
FileName <- paste(FileName, ".csv", sep="")
data <- read.csv(FileName, header=TRUE, stringsAsFactors=FALSE)

# Minimum number of matches that need to be played in order for the team to be ranked
cat("\n")
cat("Input the required minimum number of played matches: ")
CutOff <- strtoi(readLines("stdin", n=1))
check.integer <- function(N)
{
  !grepl("[^[:digit:]]", format(N,  digits = 20, scientific = FALSE))
}
if (check.integer(CutOff) == FALSE)
{
  stop("Invalid value for match cutoff")
}

# This value will affect the spread of the ELO rankings but not the actual order
PointScale <- 6.67
cat("\n")
cat(paste("The point scale value is: ", PointScale, "\n", sep=""))

# The margin of error that will be tolerated for the point values to converge
convergenceValue <- .01
cat("\n")
cat(paste("The convergence value is: ", convergenceValue,"\n", sep=""))
cat("\n")
cat("Press return to continue: ")
Continue <- readLines("stdin", n=1)
if (Continue != "")
{
  stop("The user chose to end the program")
}

# Only includes fields from input CSV which are relevant and removes trailing lines from input file
data <- subset(data, select = -c(winnersTeamName, losersTeamName, winner, loser, matchdate, scorecrdid, descr, positionplayed, score, hteamid, vteamid))
while (nrow(data) %% 10 != 0)
{
  data <- data[-nrow(data),] 
}

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

# Only prints the below diagnostic information if the user has selected it
if (diagnostic == TRUE)
{
  # Prints a list of all teams in the data
  cat("\n")
  print(OverAllRecord[,1])
  cat("\n")
  cat("Above are all the team that are in the given data\n")
  cat("\n")
  cat(paste("Total number of teams: ", nrow(OverAllRecord),"\n",sep=""))
  cat("\n")
  cat("Press return to continue: ")
  Continue <- readLines("stdin", n=1)
  if (Continue != "")
  {
    stop("The user chose to end the program")
  }

  # Prints all the matchups in the data
  cat("\n")
  options(width=120)
  print(competitions[c(1,2)], right = FALSE)
  options(width=75)
  cat("\n")
  cat("Above are all the matches that are in the given data\n")
  cat("\n")
  cat(paste("Total number of matches: ", nrow(competitions),"\n",sep=""))
  cat("\n")
  cat("Press return to continue: ")
  Continue <- readLines("stdin", n=1)
  if (Continue != "")
  {
    stop("The user chose to end the program")
  }
  
  # Prints each team's record for the season
  cat("\n")
  print(OverAllRecord[c(1,2,3)], right = FALSE, row.names = FALSE)
  cat("\n")
  cat("Above are all the overall records for each team\n")
  cat("\n")
  cat("Press return to continue to calculating rankings: ")
  Continue <- readLines("stdin", n=1)
  if (Continue != "")
  {
    stop("The user chose to end the program")
  }
  cat("\n")
}

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

# Checks that all values in DiffWinSum are within the given convergence value range
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

# Loops through and iterates Point's values until convergence is achieved
k <- 0
while (convergence() == FALSE)
{
  if (k %% 1000 == 0)
  {
    cat(paste("Iteration #", k, "    Max Diff Win: ",max(abs(DiffWinSum)),"\n", sep = "" ))
  }
  
  # Populates WinMatrix with the win probabilities of team i beating team j
  WinMatrix <- outer(Points,Points,winProb)
  
  # Populates DiffWinSum with (Wins - ExpectedWins)
  DiffWinSum <- OverAllRecord[,2] - rowSums(TotalMatchMatrix * WinMatrix)
  
  # Updates each team's point values by their respective DiffWinSum
  Points <- Points + DiffWinSum
  k <- k + 1
}

# Penalizes teams that haven't played enough matches by dropping them to the bottom of the rankings
for (i in 1:NumColleges)
{
  if (OverAllRecord[i, 4] < CutOff)
  {
    Points[i] <- Points[i] - 5000
  }
}

# Formats the output, which is Ratings so it is easier to read
Ratings <- data.frame(NamesColleges, Points, stringsAsFactors=FALSE)
colnames(Ratings)[1]<-"College"
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
      Ratings[i, 5 + j] <- paste("(", which(helper[j,2] == Ratings$College), ") ",helper[j, 2],sep = "")
    }
  }
  
  # Adding Worst Losses
  helper <- subset(competitions, Loser == Ratings[i, 2])
  helper <- helper[with(helper, order(-Odds)),]
  for (j in 1:3)
  {
    if (j <= nrow(helper))
    {
      Ratings[i, 8 + j] <- paste("(", which(helper[j,1] == Ratings$College), ") ",helper[j, 1],sep = "")
    }
  }
}

# Prints out any teams that are tied in ranking points
cat("\n------------------------------------------------\n")
for (i in 1:(NumColleges - 1))
{
  for (j in (i+1):NumColleges)
  {
    if (abs(Ratings[i,3] - Ratings[j,3]) < convergenceValue)
    {
        cat(paste("'",Ratings[i,2],"'", " and ","'", Ratings[j,2],"'"," are tied in ranking points. Please manually adjust", "\n", sep=""))
        cat("\n")
    }
  }
}
cat("-----------------------------------------------\n")

# Prints out final information for user
cat("\n")
cat(paste("Iterations for Convergence: ", k, "\n", sep = ""))
cat("\n")
cat(paste("The resulting Rankings have been written to a csv file called: ", "output_", FileName,"\n", sep = ""))
cat("\n")
cat("You should open and save this file as an excel file in order to read it\n")
cat("\n")

# Writes the output to a CSV file
output <- paste("output_", FileName, sep="")
write.csv(Ratings, file = output, row.names=FALSE, na="")
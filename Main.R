source("./Config.R")

source("./FantasyPros.R")
source("./FanDuel.R")
source("./CBSSports.R")
source("./VegasOdds.R")


salary <- merge(salary, all, by=c("First.Name", "Last.Name", "Position"), suffixes=c('', ''), all.x = TRUE)
salary$expectedPoints <- salary$FPTS

failedToMatch <- salary[is.na(salary$expectedPoints), ]
apply(failedToMatch, 1, function(player){
  print(paste("Problem matching", player[["Position"]], player[["First.Name"]], player[["Last.Name"]], "Expected points->", player[["FPPG"]]))
})



salary$expectedPoints[is.na(salary$expectedPoints)] <- salary$FPPG[is.na(salary$expectedPoints)]
salary$StdDev[is.na(salary$StdDev)] <- 0

salary <- mergeDefenseRank(salary);

odds <- getOddsData('nfl')
odds <- replaceTeamNamesWithAbbr(odds)
odds <- odds[odds$Source == 'SportsBetting.ag',]
odds <- cbind(odds, Game=paste(odds$Away, odds$Home, sep="@"))
salary <- merge(salary, odds, by.x="Game", by.y="Game", suffixes=c('', ''), all.x = TRUE)


if(PROJECTION_NOISE_AMOUNT > 0)
{
  salary$expectedPoints <- jitter(salary$expectedPoints, amount=PROJECTION_NOISE_AMOUNT)
}


#salary <- salary[order(salary$expectedPoints, decreasing = TRUE),]

#not sure why this is na
salary <- salary[!is.na(salary$Injury.Indicator),]

salary <- salary[salary$Injury.Indicator != 'IR',]
salary <- salary[salary$Injury.Indicator != 'O',]
salary <- salary[salary$Injury.Indicator != 'D',]

if(IGNORE_QUESTIONABLE == TRUE)
{
  salary <- salary[salary$Injury.Indicator != 'Q',]
}

getPlayers <- function(position, num)
{
  salary[salary$Position == position,][1:num,]$Id
}



# number of variables
num.players <- length(salary$Id)
# objective:
obj <- salary$expectedPoints
# the vars are represented as booleans
var.types <- rep("B", num.players)
# the constraints
matrix <- rbind(as.numeric(salary$Position == "QB"), # num QB
                as.numeric(salary$Position == "RB"), # num RB
                as.numeric(salary$Position == "WR"), # num WR
                as.numeric(salary$Position == "TE"), # num TE
                as.numeric(salary$Position == "K"),# num K
                as.numeric(salary$Position == "D"),# num DEF
                salary$StdDev,
                salary$Salary)                       # total cost

direction <- c("==", # QB
               "==", # RB
               "==", # WR
               "==", # TE
               "==", # K
               "==", # D
               ">=", # StdDev
               "<=" # Salary
               )
rhs <- c(NUM_QB, # Q
         NUM_RB, # RB
         NUM_WR, # WR
         NUM_TE, # TE
         NUM_K, # K
         NUM_D, # D
         0, #std dev
         MAX_SALARY)

for(player in USE_PLAYER)
{
  matrix <- rbind(matrix, as.numeric(salary$Id == player))
  direction <- c(direction, "==")
  rhs <- c(rhs, 1)
}

for(player in REJECT_PLAYER)
{
  matrix <- rbind(matrix, as.numeric(salary$Id == player))
  direction <- c(direction, "==")
  rhs <- c(rhs, 0)
}


sol <- Rglpk_solve_LP(obj = obj, mat = matrix, dir = direction, rhs = rhs,
                      types = var.types, max = TRUE)

print(salary[sol$solution == 1,])
print(paste("Total Salary =", sum(salary[sol$solution == 1,]$Salary)))
print(paste("Expected Points =", sum(salary[sol$solution == 1,]$expectedPoints)))
print(paste("Stdev =", sum(salary[sol$solution == 1,]$StdDev)))
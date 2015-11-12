options(stringsAsFactors = FALSE)

week <- 9

PPR <- .5

MAX_SALARY <- 60000
NUM_QB <- 1
NUM_RB <- 2
NUM_WR <- 3
NUM_TE <- 1
NUM_D <- 1
NUM_K <- 1

IGNORE_QUESTIONABLE <- TRUE

PROJECTION_NOISE_AMOUNT <- 2

#jets 12544
#denver 12531
#pats 12541
USE_PLAYER <- c(12544)
REJECT_PLAYER <- c(9392) # crabtree


TEAM_NAMES <- read.csv("./data/TeamNames.csv", stringsAsFactors=FALSE)
NAME_SUBSTITUTIONS <- read.csv("./data/NameSubstitutions.csv", stringsAsFactors=FALSE)

replaceTeamNamesWithAbbr <- function(data)
{
  for(i in 1:dim(TEAM_NAMES)[1])
  {
    data <- as.data.frame(sapply(data, function(x) { gsub(TEAM_NAMES$NAME[i], TEAM_NAMES$TEAM[i], x) } ))
  }
  data
}

substituteNames <- function(data, colName)
{
  for(i in 1:dim(NAME_SUBSTITUTIONS)[1])
  {
    #data[colName] <- gsub(TEAM_NAMES$NAME[i], TEAM_NAMES$TEAM[i], data[colName])
    data[data[colName] == NAME_SUBSTITUTIONS$NAME[i], colName] <- NAME_SUBSTITUTIONS$REPLACEMENT[i]
  }
  data
}


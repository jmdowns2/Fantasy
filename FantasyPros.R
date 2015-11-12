
#  position - qb, rb, te, wr, k
#  week - int
getUrl <- function(position, week, type="projections")
{
  paste('http://www.fantasypros.com/nfl/', type,'/', position, '.php?week=', week, sep='')
}

parseName <- function(val)
{
  val$Player <- gsub('Griffin III', 'Griffin', val$Player)
  val$Player <- gsub('Beckham Jr.', 'Beckham', val$Player)
  val$Player <- gsub('Ginn Jr.', 'Ginn', val$Player)
  
  
  splitNames <- strsplit(val$Player, ' ')
  
  firstNames <- NULL
  lastNames <- NULL
  teams <- NULL
  
  for (i in 1:length(splitNames))
  {
    firstNames <- rbind(firstNames, splitNames[[i]][1]) 
    lastNames <- rbind(lastNames, splitNames[[i]][2]) 
    teams <- rbind(teams, splitNames[[i]][3]) 
  }
  
  val$First.Name <- firstNames
  val$Last.Name <- lastNames
  val$Team <- teams
  
  val
}

normalizePlayers <- function(val)
{
  names(val)[names(val) == "Std Dev"] <- "StdDev"
    
  newFrame <- data.frame(First.Name=val$First.Name, Last.Name=val$Last.Name, Position=val$Position, stringsAsFactors=FALSE)
  
  cols <- c('FPTS', 'StdDev', 'Best', 'Worst')
  for(col in cols)
  {
    if(!is.null(val[[col]]))
    {
      newFrame <- cbind(newFrame, as.numeric(val[[col]]))
      names(newFrame)[dim(newFrame)[2]] <- col
    }    
  }

  newFrame
}

addPPR <- function(val)
{
  if(!is.null(val$REC) && PPR > 0)
  {
    val$REC <- as.numeric(val$REC)
    val$FPTS <- as.numeric(val$FPTS)
    val$FPTS <- val$FPTS + (val$REC * PPR)
  }
  val
}

getStatsForWeek <- function(week, type="projections")
{
  qbs <- readHTMLTable(getUrl("qb", week, type), stringsAsFactors = FALSE)$data
  wrs <- readHTMLTable(getUrl("wr", week, type), stringsAsFactors = FALSE)$data
  rbs <- readHTMLTable(getUrl("rb", week, type), stringsAsFactors = FALSE)$data
  tes <- readHTMLTable(getUrl("te", week, type), stringsAsFactors = FALSE)$data
  ks <- readHTMLTable(getUrl("k", week, type), stringsAsFactors = FALSE)$data
  
  qbs <- parseName(qbs)
  wrs <- parseName(wrs)
  rbs <- parseName(rbs)
  tes <- parseName(tes)
  ks <- parseName(ks)
  
  qbs$Position <- "QB"
  wrs$Position <- "WR"
  rbs$Position <- "RB"
  tes$Position <- "TE"
  ks$Position <- "K"
  
  qbs <- addPPR(qbs)
  wrs <- addPPR(wrs)
  rbs <- addPPR(rbs)
  tes <- addPPR(tes)
  ks <- addPPR(ks)
  
  all <- rbind(normalizePlayers(qbs), normalizePlayers(wrs), normalizePlayers(rbs), normalizePlayers(tes), normalizePlayers(ks))
}

all <- getStatsForWeek(week)
rankings <- getStatsForWeek(week, "rankings")

all <- merge(all, rankings, by=c("First.Name", "Last.Name", "Position"), suffixes=c('', ''), all.x = TRUE)


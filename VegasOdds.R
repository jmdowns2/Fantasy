
stripRank <- function(name)
{
  if(substr(name, 1, 1) != '#')
  {
    return(name)
  }
  pos <- regexpr(" ", name);
  substr(name, pos+1, nchar(name))
}

processOddsHtml <- function(data)
{
  odds <- NULL
  game <- ''
  Home <- ''
  Away <- ''
  for(i in 1:length(data))
  {
    row <- data[[i]]
    classes <- xmlGetAttr(row, "class")  
    
    if(grepl(classes, "stathead"))
    {
      # New game
      game <- xmlValue(row)
      # strip ' - ...'
      game <- substr(game, 0, regexpr(" - ", game)-1)[[1]]
      Home <- unlist(strsplit(game, ' at '))[2]
      Home <- stripRank(Home)
      Away <- unlist(strsplit(game, ' at '))[1]
      Away <- stripRank(Away)
    }
    else if(grepl(classes, "colhead"))
    {
      # Contains labels
    }
    else
    {
      vals <- xmlChildren(row)
      if(length(vals) == 4)
      {
        Source <- xmlValue(vals[[1]])
        #Spread <- xmlValue(vals[[2]])
        Spread <- xpathSApply(vals[[2]],".//text()", xmlValue)[1]
        #OU <- xmlValue(vals[[3]])
        OU <- xpathSApply(vals[[3]],".//text()", xmlValue)[1]
        OU <- gsub(" O/U", "", OU)
        odds <- rbind(odds, data.frame(Source=Source, Home=Home, Away=Away, Spread=Spread, OU=OU))
      }
    }
  }
  
  odds$Spread[odds$Spread == "EVEN"] <- 0
  odds$Spread <- as.numeric(odds$Spread)
  
  odds <- substituteNames(odds, "Home")
  odds <- substituteNames(odds, "Away")
  odds
}

# type = 'nfl' 'ncf'
getOddsData <- function(type)
{
  url <- paste('http://espn.go.com/', type, '/lines', sep='')
  html <- htmlTreeParse(getURL(url), useInternalNodes=T)
  data <- xpathApply(html, '//*[@id="my-teams-table"]/div/div[1]/table/tr')
  processOddsHtml(data)
}

test1 <- getOddsData('nfl')
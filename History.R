

history <- read.csv('./Data/history.csv', stringsAsFactors=FALSE, fill=TRUE, row.names=NULL)
history <- history[history$Position != "Cancelled",]

historyRowNames <- strsplit('Entry Id,Sport,Date,Title,SalaryCap,Score,Opp Score,Position,Entries,Opponent,Entry ($),Winnings ($),Link,', ',')
historyRowNames <- as.vector(historyRowNames)
colnames(history) <- historyRowNames[[1]]

history <- history[!is.na(history$Score),]
history$Percentile <- 1 - (history$Position-1) / history$Entries
history$Date <- as.Date(history$Date)

nflHistory <- history[history$Sport == "nfl",]

plot(nflHistory$Date, nflHistory$Score, type='p')
abline(h=100)

plot(nflHistory$Date, nflHistory$Percentile, type='p')
abline(h=.5)

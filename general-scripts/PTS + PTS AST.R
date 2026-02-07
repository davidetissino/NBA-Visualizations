### HISTORIC PTS + AST GENERATED ####

library(tidyverse)
library(jsonlite)
library(httr)

# headers Ryan Davis (?)
headers <- c(
  `Connection` = 'keep-alive',
  `Accept` = 'application/json, text/plain, */*',
  `x-nba-stats-token` = 'true',
  `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
  `x-nba-stats-origin` = 'stats',
  `Sec-Fetch-Site` = 'same-origin',
  `Sec-Fetch-Mode` = 'cors',
  `Referer` = 'https://stats.nba.com/players/shooting/',
  `Accept-Encoding` = 'gzip, deflate, br',
  `Accept-Language` = 'en-US,en;q=0.9'
)


ast <- function(seas) {
  
  url <- paste0('https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&PlayerExperience=&PlayerOrTeam=Player&PlayerPosition=&PtMeasureType=Passing&Season=',seas,'&SeasonSegment=&SeasonType=Regular%20Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight=')
  
  
  res <- GET(url = url, add_headers(.headers = headers))
  resp <- fromJSON(content(res, 'text'))
  
  astd_x <- data.frame(resp$resultSets$rowSet)
  
  colnames(astd_x) <- resp[['resultSets']][['headers']][[1]]
  
  return(astd_x)
  
}

ast(2023)



res <- GET(url = url, add_headers(.headers = headers))
resp <- fromJSON(content(res, 'text'))

astd_x <- data.frame(resp$resultSets$rowSet)

colnames(astd_x) <- resp[['resultSets']][['headers']][[1]]





drives <- function(season) {
  
  url <- paste0('https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&PlayerExperience=&PlayerOrTeam=Player&PlayerPosition=&PtMeasureType=Drives&Season=', season, '&SeasonSegment=&SeasonType=Regular%20Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight=')
  
  res <- GET(url = url, add_headers(.headers = headers))
  
  resp <- fromJSON(content(res, 'text'))
  
  df <- data.frame(resp$resultSets$rowSet)
  
  colnames(df) <- resp[['resultSets']][['headers']][[1]]
  
  return(df)
  
}

miao <- drives(2023)





paste0('https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&PlayerExperience=&PlayerOrTeam=Player&PlayerPosition=&PtMeasureType=Passing&Season=', 
       season, '&SeasonSegment=&SeasonType=Regular%20Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight=')


gamelogs <- function(season) {
  
  url <- paste0('https://stats.nba.com/stats/leaguegamelog?Counter=1000&DateFrom=&DateTo=&Direction=DESC&ISTRound=&LeagueID=00&PlayerOrTeam=P&Season=', 
                season, '&SeasonType=Regular%20Season&Sorter=DATE')
  
  res <- GET(url = url, add_headers(.headers = headers))
  resp <- fromJSON(content(res, 'text'))
  
  logs <- data.frame(resp$resultSets$rowSet)
  colnames(logs) <- resp[['resultSets']][['headers']][[1]]
  
  return(logs)
  
}




pts24 <- assisted(2023)



ast <- assisted(2023)

df <- assisted(2023)




url <- 'https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&PlayerExperience=&PlayerOrTeam=Player&PlayerPosition=&PtMeasureType=Passing&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight='

  
res <- GET(url = url, add_headers(.headers = headers))
resp <- fromJSON(content(res, 'text'))

astd <- data.frame(resp$resultSets$rowSet)

colnames(astd) <- resp[['resultSets']][['headers']][[1]]


url <- 'https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&PlayerExperience=&PlayerOrTeam=Player&PlayerPosition=&PtMeasureType=Passing&Season=2013-14&SeasonSegment=&SeasonType=Regular%20Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight='


res <- GET(url = url, add_headers(.headers = headers))
resp <- fromJSON(content(res, 'text'))

astd_x <- data.frame(resp$resultSets$rowSet)

colnames(astd_x) <- resp[['resultSets']][['headers']][[1]]




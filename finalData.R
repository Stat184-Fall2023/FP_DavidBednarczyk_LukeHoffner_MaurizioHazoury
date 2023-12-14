library(tidyverse)
library(rvest)
library(writexl)
library(data.table)
library(stringi)
library(kableExtra)

#Creating Totals table

#Scraping 22-23 season totals from basketball reference
nbaTopScorersTotals <- read_html(
  x = "https://www.basketball-reference.com/leagues/NBA_2023_totals.html#totals_stats::pts"
) %>%
  html_elements(css = "table") %>%
  html_table()
nbaTopScorersTotals23 <- nbaTopScorersTotals[[1]]

#Eliminating duplicates from the table
tot_rows <- nbaTopScorersTotals23$Tm == "TOT"
duplicates <- duplicated(nbaTopScorersTotals23$Player)
removed_rows <- duplicates & !tot_rows
nbaTopScorersTotals23 <- nbaTopScorersTotals23[!removed_rows, ]

#removing any accents from the players name
remove_accents <- function(x) {
  stri_trans_general(x, "Latin-ASCII")
}
nbaTopScorersTotals23$Player <- remove_accents(nbaTopScorersTotals23$Player)
remove_accents <- function(x) {
  stri_trans_general(x, "Latin-ASCII")
}

#Creating Averages table

#Scraping top scorer's by ppg data table from ESPN
nbaTopScorers <- read_html(
  x = "https://www.espn.com/nba/stats/player/_/season/2023/seasontype/2"
) %>%
  html_elements(css = "table") %>%
  html_table()
nbaTopScorers23 <- bind_cols(nbaTopScorers[[1]], nbaTopScorers[[2]])

#Removing unecessary columns and rows
nbaTopScorers23 <- nbaTopScorers23[-c(26:50), -c(16:22)]

#Removing team abbreviation from end of player name
nbaTopScorers23$Name[nbaTopScorers23$Name == "Joel EmbiidPHI"] <- "Joel Embiid"
nbaTopScorers23$Name[nbaTopScorers23$Name == "Luka DoncicDAL"] <- "Luka Doncic"
nbaTopScorers23$Name[nbaTopScorers23$Name == "Damian LillardPOR"] <- "Damian Lillard"
nbaTopScorers23$Name[nbaTopScorers23$Name == "Shai Gilgeous-AlexanderOKC"] <- "Shai Gilgeous-Alexander"
nbaTopScorers23$Name[nbaTopScorers23$Name == "Giannis AntetokounmpoMIL"] <- "Giannis Antetokounmpo"
nbaTopScorers23$Name[nbaTopScorers23$Name == "Jayson TatumBOS"] <- "Jayson Tatum"
nbaTopScorers23$Name[nbaTopScorers23$Name == "Stephen CurryGS"] <- "Stephen Curry"
nbaTopScorers23$Name[nbaTopScorers23$Name == "Kevin DurantBKN/PHX"] <- "Kevin Durant"
nbaTopScorers23$Name[nbaTopScorers23$Name == "LeBron JamesLAL"] <- "LeBron James"
nbaTopScorers23$Name[nbaTopScorers23$Name == "Donovan MitchellCLE"] <- "Donovan Mitchell"
nbaTopScorers23$Name[nbaTopScorers23$Name == "Devin BookerPHX"] <- "Devin Booker"
nbaTopScorers23$Name[nbaTopScorers23$Name == "Kyrie IrvingBKN/DAL"] <- "Kyrie Irving"
nbaTopScorers23$Name[nbaTopScorers23$Name == "Jaylen BrownBOS"] <- "Jaylen Brown"
nbaTopScorers23$Name[nbaTopScorers23$Name == "Trae YoungATL"] <- "Trae Young"
nbaTopScorers23$Name[nbaTopScorers23$Name == "Ja MorantMEM"] <- "Ja Morant"
nbaTopScorers23$Name[nbaTopScorers23$Name == "Zion WilliamsonNO"] <- "Zion Williamson"
nbaTopScorers23$Name[nbaTopScorers23$Name == "Anthony DavisLAL"] <- "Anthony Davis"
nbaTopScorers23$Name[nbaTopScorers23$Name == "Lauri MarkkanenUTAH"] <- "Lauri Markkanen"
nbaTopScorers23$Name[nbaTopScorers23$Name == "Julius RandleNY"] <- "Julius Randle"
nbaTopScorers23$Name[nbaTopScorers23$Name == "De'Aaron FoxSAC"] <- "De'Aaron Fox"
nbaTopScorers23$Name[nbaTopScorers23$Name == "Zach LaVineCHI"] <- "Zach LaVine"
nbaTopScorers23$Name[nbaTopScorers23$Name == "Brandon IngramNO"] <- "Brandon Ingram"
nbaTopScorers23$Name[nbaTopScorers23$Name == "Anthony EdwardsMIN"] <- "Anthony Edwards"
nbaTopScorers23$Name[nbaTopScorers23$Name == "DeMar DeRozanCHI"] <- "DeMar DeRozan"
nbaTopScorers23$Name[nbaTopScorers23$Name == "Nikola JokicDEN"] <- "Nikola Jokic"

#Changing first column name to Player
colnames(nbaTopScorers23)[2] <- "Player"

#Extracting name column of nbaTopScorers23 into a vector
top25 <- nbaTopScorers23$Player

#Creating a data frame of all players in the league
nbaTop25 <- data.frame(Player = nbaTopScorersTotals23$Player)

#Creating empty data frame
top25ScoringTotals <- data.frame()

#Creating a for loop to extract the players from the nbaTopScorers23 from the Totals table
for (string in top25) {
  #Taking the row from the totals table that has the same player as the nbaTopScorers23 table
  top25Rows <- nbaTopScorersTotals23$Player == string
  top25Scorers <- nbaTopScorersTotals23[top25Rows, ]
  #Combines selected columns into a new table
  if (nrow(top25Scorers) > 0) {
    top25ScoringTotals <- rbind(top25ScoringTotals, top25Scorers)
  }
}

#Changing column 5 to Team
colnames(top25ScoringTotals)[5] <- "Team"

#Changing a player's team names
top25ScoringTotals[8, 5] <- "BKN/PHX"
top25ScoringTotals[12, 5] <- "BKN/DAL"

#Removing unecessary columns
top25ScoringTotals <- top25ScoringTotals[, -c(1, 3:7, 11, 14, 21:29)]

#Merging the data sets
nba23Data <- inner_join(nbaTopScorers23, top25ScoringTotals, by = "Player")

#Fixing column names
colnames(nba23Data)[6] <- "PPG"
colnames(nba23Data)[7] <- "FGM"
colnames(nba23Data)[8] <- "FGA"
colnames(nba23Data)[9] <- "FG%"
colnames(nba23Data)[11] <- "3PA"
colnames(nba23Data)[12] <- "3P%"
colnames(nba23Data)[14] <- "FTA"
colnames(nba23Data)[15] <- "FT%"
colnames(nba23Data)[17] <- "Total FGM"
colnames(nba23Data)[18] <- "Total FGA"
colnames(nba23Data)[19] <- "Total 3PM"
colnames(nba23Data)[20] <- "Total 3PA"
colnames(nba23Data)[21] <- "Total 2PM"
colnames(nba23Data)[22] <- "Total 2PA"
colnames(nba23Data)[23] <- "2P%"
colnames(nba23Data)[25] <- "Total FT"
colnames(nba23Data)[26] <- "Total FTA"
colnames(nba23Data)[27] <- "PTS"

#Turning Columns 4-27 into numeric values and adding True Shooting %
nba23Data[, 4:27] <- lapply(nba23Data[, 4:27], as.numeric)
nba23Data$`TS%` <- (nba23Data$PTS / (2 * (nba23Data$`Total FGA` + 0.44 * nba23Data$`Total FTA`))) * 100
nba23Data$`TS%` <- round(nba23Data$`TS%`, 2)

#Subsetting the data for naming purposes on the scatterplot
subsetNba23Data <- nba23Data[nba23Data$PPG * nba23Data$`TS%` > 1900, ]
Jokic <- nba23Data[nba23Data$`TS%` > 70, ]

firstHalfData <- nba23Data[, 1:15]
secondHalfData <- nba23Data[, c(1:3, 16:28)]
firstHalfData <- firstHalfData[, -c(4, 13:15)]

firstHalfDataTable <- firstHalfData %>%
  kable(
    caption = "Top 25 NBA Players by Points per Game in the 2022-2023 Season",
    booktabs = TRUE,
    align = c("l", rep("c", 6))
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 6
  )

secondHalfDataTable <- secondHalfData %>%
  kable(
    booktabs = TRUE,
    align = c("l", rep("c", 6))
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 6
  )

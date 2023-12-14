library(tidyverse)
library(rvest)


nbaTopScorers <- read_html(
  x = "https://www.espn.com/nba/stats/player/_/season/2023/seasontype/2"
) %>%
  html_elements(css = "table") %>%
  html_table()

nbaTopScorers23 <- bind_cols(nbaTopScorers[[1]], nbaTopScorers[[2]])
View(nbaTopScorers23)



nbaTopScorersTotals <- read_html(
  x = "https://www.basketball-reference.com/leagues/NBA_2023_totals.html#totals_stats::pts"
) %>%
  html_elements(css = "table") %>%
  html_table()

nbaTopScorersTotals23 <- nbaTopScorersTotals[[1]]
View(nbaTopScorersTotals23)


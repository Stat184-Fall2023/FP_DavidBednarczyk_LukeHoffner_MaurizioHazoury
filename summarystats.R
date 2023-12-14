nbaPositionData <- nba23Data %>%
  group_by(POS) %>%
  summarize(
    mean_PPG = mean(`PPG`),
    mean_TS = mean(`TS%`),
    sd_PPG = sd(`PPG`),
    sd_TS = sd(`TS%`),
    max_PPG = max(`PPG`),
    max_TS = max(`TS%`)
  )

nbaPositionData
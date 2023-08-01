# scrape websites for data
ff_raw <- ffanalytics::scrape_data(
  src = c("CBS", "ESPN", "Fantasy Pros", "Fantasy Sharks", 
  "FFToday", "NumberFire", "FantasyFootballNerd", 
  "NFL", "RTSports", "Walterfootball"),
  pos = c("QB", "RB", "WR", "TE"),
  season = 2023, 
  week = NULL
  )

readr::write_rds(ff_raw, file = here::here("data", "ff_raw.Rds"))

#This function returns the stats to be displayed 
Backendfx <- function(sum_name_input,api_key){
  
  require(curl)
  require(jsonlite)
  require(RJSONIO)
  require(data.table)
  
  #champion info
  queueinfo <- fromJSON("http://static.developer.riotgames.com/docs/lol/queues.json")
  champinfo <- fromJSON("http://ddragon.leagueoflegends.com/cdn/10.21.1/data/en_US/champion.json")
  base_url <- "https://euw1.api.riotgames.com"
  sumname_url <- "/lol/summoner/v4/summoners/by-name/"
  
  #fetch data from summoner name
  
  name_data <- fromJSON(GetRiot(name = sum_name_input, base_url, property_url = sumname_url, api_key))
  
  #fetch mastery
  
  mastery_url <- "/lol/champion-mastery/v4/champion-masteries/by-summoner/"
  mastery_data <- fromJSON(GetRiot(name = name_data$id, base_url, property_url = mastery_url, api_key))
  
  #isolate champion name and id
  champ_name_id <- rbindlist(lapply(champinfo$data, "[", c(2,3)))
  
  #favorite champ is #1 from mastery
  
  #rename champid column to champname
  fav_data <- rbindlist(mastery_data[1:3])
  fav_ids <- fav_data$championId
  fav_data$championId <- champ_name_id$id[match(fav_data$championId, champ_name_id$key)]
  
  #winrate fav champ
  
  match_url <- "/lol/match/v4/matchlists/by-account/"
  #get id of fav champ
  fav_champ_id <- fav_ids[1]
  #make url for json with champ and queue selection 400 = draft nm 420 = solo ranked 440 = flex ranked
  queues <- "&queue=400&queue=420&queue=440"
  fav_name <- paste0("champion=", fav_champ_id,queues)
  
  match_data <- fromJSON(GetRiot(name = name_data$accountId, base_url, match_url, api_key, props = fav_name))
  
  #get match ids
  match_id <- unlist(lapply(match_data$matches, "[", 2))
  
  match_id_url <- "/lol/match/v4/matches/"
  match_data_fav <-  lapply(match_id, function(x){GetRiot(x, base_url, match_id_url, api_key)})
  
  #only recent 10 games on the champ because R is fucking slow
  match_data_ten <- match_data_fav[c(1:10)]
  match_data_recent <-  lapply(match_data_ten, function(x){temp = fromJSON(x)})
  
  #winrate/kda function
  gamestats_fav <- KDA(match_data_recent,input_name = sum_name_input)
  
  #most recent stats
  #convert current time to unix timestamp in ms
  time_current <-as.numeric(Sys.time())*1000 
  #set begin time to 30 days prior
  time_begin <- round(time_current - 2592000000)
  vars <- paste0(queues,"&beginTime=",time_begin,"&endIndex=10")
  
  
  recent_match_data <- tryCatch(
    expr = {
      fromJSON(GetRiot(name = name_data$accountId, base_url, match_url, api_key, props = vars))
    },
    error = function(e){
      message("kenkerwarning")
      recent_match_data <- fromJSON(GetRiot(name = name_data$accountId, base_url, match_url, api_key, props = paste0(queues,"&endIndex=10")))
    },
    warning = function(w){
      message("kenkerwarning")
    }
    
  )
  
  recent_match_id<- unlist(lapply(recent_match_data$matches, "[", 2))
  match_data_month <- lapply(recent_match_id, function(x){fromJSON(GetRiot(x, base_url, match_id_url, api_key))})
  gamestats_recent <- KDA(match_data_month,sum_name_input)
  
  #calculate winrate and kda of the 3 most recent champs
  gamestat_df <- rbindlist(gamestats_recent$player)
  champ_count <- as.data.frame(table(gamestat_df$champID))
  sorted <- champ_count[order(champ_count$Freq,decreasing = TRUE),]
  recent_3_champ <- sorted[1:3,1]
  r3_names <- champ_name_id$id[match(recent_3_champ,champ_name_id$key)]
  recent_3_played <- sorted[1:3,2]
  recent_3c <- gamestat_df[gamestat_df$champID %in% recent_3_champ]
  
  kda_recent <- NULL
  win_rate_recent <- NULL
  for(i in 1:length(recent_3_champ)){
    kda_recent[i] <- sum(recent_3c$kda[recent_3c$champID == recent_3_champ[i]])/recent_3_played[i]
    win_logic <- !is.na(match(recent_3c$outcome[recent_3c$champID == recent_3_champ[i]],"Win"))
    win_rate_recent[i] <- sum(ifelse(win_logic,1,0))/recent_3_played[i]*100
  }
  
  #ranking
  
  queue_url <- "/lol/league/v4/entries/by-summoner/"
  queue_data <- fromJSON(GetRiot(name = name_data$id, base_url, queue_url, api_key))
  # queue_data <- tryCatch(
  #   expr = {
  #     fromJSON(GetRiot(name = name_data$id, base_url, queue_url, api_key))
  #   },
  #   error = function(e){
  #     message("meer games spelen nerd")
  #     queue_data <- 0
  #     
  #   },
  #   warning = function(w){
  #     message("kenkerwarning")
  #   }
  # )
  
  
  rank <- GetRank(queue_data)
  
  
  player <- list("name" = sum_name_input, 
                 "rank" = rank$tier,
                 "rank_tier" = rank$division, 
                 "fav_champ" = fav_data$championId[1],
                 "mastery_fav" = fav_data$championPoints[1],
                 "winrate" = gamestats_fav$winrate,
                 "games_played_fav" = length(match_data_ten),
                 "kda_fav" = gamestats_fav$KDA,
                 "top_3_champs_recent" = r3_names,
                 "top_3_champs_games_recent" = recent_3_played,
                 "top_3_champs_winrate_recent" = win_rate_recent,
                 "top_3_champs_kda_recent" = kda_recent
  )
  
  return(player)
}
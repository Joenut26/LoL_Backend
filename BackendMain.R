#Backend API#

#source functions
lapply(paste0("fun/",list.files("fun")),source)

library(curl)
library(jsonlite)
library(RJSONIO)
library(data.table)





source("authentication.R")


#inputname

sum_name_input <- "Zwieee" 

#Backendfx <- function(name,api_key){
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
fav_data$championId <- champ_name_id$id[match(fav_data$championId, champ_name_id$key)]

#winrate fav champ

match_url <- "/lol/match/v4/matchlists/by-account/"
#get id of fav champ
fav_champ_id <- fav_data$championId[1]
#make url for json with champ and queue selection 400 = draft nm 420 = solo ranked 440 = flex ranked
queues <- "&queue=400&queue=420&queue=440"
fav_name <- paste0("champion=", fav_champ_id,queues)

match_data <- fromJSON(GetRiot(name = name_data$accountId, base_url, match_url, api_key, props = fav_name ))

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
vars <- paste0(queues,"&beginTime=",time_begin,"&endIndex=100")

recent_match_data <- fromJSON(GetRiot(name = name_data$accountId, base_url, match_url, api_key, props = vars))
recent_match_id<- unlist(lapply(recent_match_data$matches, "[", 2))
match_data_month <- lapply(recent_match_id, function(x){fromJSON(GetRiot(x, base_url, match_id_url, api_key))})

gamestats_recent <- KDA(match_data_month,sum_name_input)

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
queue_data <- fromJSON(GetRiot(name = name_data$id, base_url, account_url, api_key))

tier_vector <- c("iron","bronze","silver","gold","platinum","diamond","master","grandmaster","challenger")

if(length(queue_data) > 0){
  
  #get soloq rank and convert to lowercase
  rank_tier_solo <- tolower(queue_data[[1]]$tier)
  #get division in numeral
  rank_div_solo <- as.numeric(as.roman(queue_data[[1]]$rank))
  
  div <- toupper(substr(rank_tier_solo,start = 0,stop = 1 ))
  rank_tier_div_solo <- paste0(div,rank_div_solo)
  
  if(length(queue_data) == 2){
    #flex queue
    
    rank_tier_flex <- tolower(queue_data[[2]]$tier)
    rank_div_flex <- as.numeric(as.roman(queue_data[[2]]$rank))
    
    div_flex <- toupper(substr(rank_tier_flex,start = 0,stop = 1 ))
    rank_tier_div_flex <- paste0(div,rank_div_flex)
    
    if(match(rank_tier_solo,tier_vector) == match(rank_tier_flex,tier_vector)){
      if(rank_div_solo <= rank_div_flex){
        rank_tier <- rank_tier_solo
        rank_tier_div <- rank_tier_div_solo
      }else{
        rank_tier <- rank_tier_flex
        rank_tier_div <- rank_tier_div_flex
      }
    }else if(match(rank_tier_solo,tier_vector)>match(rank_tier_flex,tier_vector)){
      rank_tier <- rank_tier_solo
      rank_tier_div <- rank_tier_div_solo
    }else{
      rank_tier <- rank_tier_flex
      rank_tier_div <- rank_tier_div_flex
    }
  }
  
  
}else{
  rank_tier <- "unranked"
  rank_tier_div <- "unranked"
}







players <- list("name" = sum_name_input, 
                "rank" = rank_tier,
                "rank_tier" = rank_tier_div,
                "fav_champ" = fav_data$championId[1],
                "mastery_fav" = fav_data$championPoints[1],
                "winrate" = gamestats_fav$winrate,
                "games_played_fav" = lenght(match_data_ten),
                "kda_fav" = gamestats_fav$KDA,
                "top_3_champs_recent" = r3_names,
                "top_3_champs_games_recent" = recent_3_played,
                "top_3_champs_winrate_recent" = win_rate_recent,
                "top_3_champs_kda_recent" = kda_recent
)

#}


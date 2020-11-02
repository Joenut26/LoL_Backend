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
match_id_list <- unlist(lapply(match_data$matches, "[", 2))

match_id_url <- "/lol/match/v4/matches/"
match_data_fav <-  lapply(match_id_list, function(x){GetRiot(x, base_url, match_id_url, api_key)})





#only recent 10 games on the champ because R is fucking slow
match_data_ten <- match_data_fav[c(1:10)]
match_data_recent <-  lapply(match_data_ten, function(x){temp = fromJSON(x)})

player_names <- lapply(match_data_recent,"[", "participantIdentities")


participant_df_rows <- data.frame()
participant_list <- list()

for (i in 1:length(player_names)){
  int <- which(lapply(lapply(player_names[[i]]$participantIdentities, "[[",2), "[[",3) == sum_name_input)
  #combine name and participant id in dataframe per game
  bind_df <- data.frame("participantID" = player_names[[i]]$participantIdentities[[int]]$participantId,
                        "participantName" = player_names[[i]]$participantIdentities[[int]]$player$summonerName,
                        "teamID" = match_data_recent[[i]]$participants[[int]]$teamId,
                        "kda" = (match_data_recent[[i]]$participants[[int]]$stats$kills + 
                                 match_data_recent[[1]]$participants[[int]]$stats$assists)/
                                 match_data_recent[[1]]$participants[[int]]$stats$deaths)
  
  participant_df_rows <- rbind(participant_df_rows,bind_df)
  
  #win or loss
  outcome <- !is.na(match(participant_df_rows$teamID,match_data_recent[[i]]$teams[[1]]$teamId))
  outcome <- ifelse(outcome,match_data_recent[[i]]$teams[[1]]$win,match_data_recent[[i]]$teams[[2]]$win)
  
  participant_df_rows <- cbind(participant_df_rows,outcome)
  
  temp <- list("data" = participant_df_rows)
  participant_list[[i]] <- temp$data
  names(participant_list)[i] <- paste0("gamenumber",i)
  #reset participant_df_rows
  participant_df_rows <- data.frame()
}

#which(...) = row c(2,4) = colums
results <- lapply(participant_list, function(x){x[which(x$participantName == sum_name_input),c(2,5)]})

results_logic <- unlist(lapply(results,function(x){x$outcome == "Win"}))
#winrate
winrate <- sum(results_logic, na.rm = TRUE)/length(results_logic)*100
#kda
kda_avg <-sum(unlist(lapply(participant_list,"[[",4)))/length(results_logic)
  
  
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
                "winrate" = winrate,
                "games_played_fav" = lenght(match_data_ten),
                "kda_fav" = kda_avg,
                "top_3_champs_recent" = 
                  
                  
)

#}


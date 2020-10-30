#Backend API#

#source functions
lapply(paste0("fun/",list.files("fun")),source)

library(curl)
library(jsonlite)
library(RJSONIO)





source("authentication.R")


#inputname

name <- "Zwieee" 

#Backendfx <- function(name,api_key){
#champion info
queueinfo <- fromJSON("http://static.developer.riotgames.com/docs/lol/queues.json")
champinfo <- fromJSON("http://ddragon.leagueoflegends.com/cdn/10.21.1/data/en_US/champion.json")
base_url <- "https://euw1.api.riotgames.com"
sumname_url <- "/lol/summoner/v4/summoners/by-name/"

#fetch data from summoner name
tic()
name_data <- fromJSON(GetRiot(name = name, base_url, property_url = sumname_url, api_key))
toc()



#fetch mastery

mastery_url <- "/lol/champion-mastery/v4/champion-masteries/by-summoner/"
tic()
mastery_data <- fromJSON(GetRiot(name = name_data$id, base_url, property_url = mastery_url, api_key))
toc()
#isolate champion name and id
champ_name_id <- lapply(champinfo$data, "[", c(2,3))

#favorite champ is #1 from mastery

#get name of the champ from mastery id
fav_data <- mastery_data[[1]]
#returns TRUE for champion id matches
fav_logic <- !is.na(match(lapply(champ_name_id,"[[",2),fav_data$championId))
champ_names <- names(champ_name_id)
fav_champ <- champ_names[fav_logic]
fav_champ_mastery <- fav_data$championPoints


#winrate fav champ

match_url <- "/lol/match/v4/matchlists/by-account/"
#get id of fav champ
fav_champ_id <- fav_data$championId
#make url for json with champ and queue selection 400 = draft nm 420 = solo ranked 440 = flex ranked
fav_name <- paste0("champion=", fav_champ_id,"&queue=400&queue=420&queue=440")
tic()
match_data <- fromJSON(GetRiot(name = name_data$accountId, base_url, match_url, api_key, props = fav_name ))
toc()
#get match ids
match_id_list <- lapply(match_data$matches, "[", 2)
match_ids <- unlist(lapply(match_id_list, "[[", 1))

match_id_url <- "/lol/match/v4/matches/"
match_data_fav <-  lapply(match_ids, function(x){GetRiot(x, base_url, match_id_url, api_key)})
#only recent 10 games on the champ because R is fucking slow
match_data_ten <- match_data_fav[c(1:10)]
match_data_recent <-  lapply(match_data_ten, function(x){temp = fromJSON(x)})

kekw <- lapply(match_data_recent,"[", "participantIdentities")






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







players <- list("name" = name, 
                "rank" = rank_tier,
                "rank_tier" = rank_tier_div,
                "fav_champ" = fav_champ,
                "mastery_fav" = fav_champ_mastery
)

#}


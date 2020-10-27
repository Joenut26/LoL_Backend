#Backend API#

#source functions
lapply(paste0("fun/",list.files("fun")),source)

library(curl)
library(jsonlite)
library(RJSONIO)
library(RColorBrewer)

#champion info

champinfo <- fromJSON("http://ddragon.leagueoflegends.com/cdn/10.21.1/data/en_US/champion.json")


api_key <- "RGAPI-9aebb144-01f2-4983-82aa-304727fd8cee"
name <- "Zwieee"
base_url <- "https://euw1.api.riotgames.com"
sumname_url <- "/lol/summoner/v4/summoners/by-name/"

#fetch data from summoner name
name_data <- fromJSON(GetRiot(name = "Zwieee", base_url, property_url = sumname_url, api_key))
 


#fetch mastery

mastery_url <- "/lol/champion-mastery/v4/champion-masteries/by-summoner/"
mastery_data <- fromJSON(GetRiot(name = name_data$id, base_url, property_url = mastery_url, api_key))

#isolate champion name and id
champ_name_id <- lapply(champinfo$data, "[", c(2,3))

#favorite champ is #1 from mastery

#get name of the champ from mastery id
fav_data <- mastery_data[[1]]
#returns TRUE for id matches
fav_logic <- !is.na(match(lapply(champ_name_id,"[[",2),fav_data$championId))
champ_names <- names(champ_name_id)
fav_champ <- champ_names[fav_logic]
fav_champ_mastery <- fav_data$championPoints


#matches played

match_url <- "/lol/match/v4/matchlists/by-account/"
match_data <- fromJSON(GetRiot(name = name_data$accountId, base_url, match_url, api_key))

  











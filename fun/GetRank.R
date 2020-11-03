#data input needs to be a list of game information from gameId's of RiotAPI with a "/lol/league/v4/entries/by-summoner/" 
#query and wrapped in a fromJSON

GetRank <- function(data){
  
  tier_vector <- c("iron","bronze","silver","gold","platinum","diamond","master","grandmaster","challenger")
  rank_tier <- "unranked"
  rank_tier_div <- "unranked"
  
  if(length(data) > 0){
    
    #get soloq rank and convert to lowercase
    rank_tier_solo <- tolower(data[[1]]$tier)
    #get division in numeral
    rank_div_solo <- as.numeric(as.roman(data[[1]]$rank))
    
    div <- toupper(substr(rank_tier_solo,start = 0,stop = 1 ))
    rank_tier_div_solo <- paste0(div,rank_div_solo)
    
    rank_tier <- rank_tier_solo
    rank_tier_div <- rank_tier_div_solo
    
    if(length(data) == 2){
      #flex queue
      
      rank_tier_flex <- tolower(data[[2]]$tier)
      rank_div_flex <- as.numeric(as.roman(data[[2]]$rank))
      
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
  }
  
  return(list("tier" = rank_tier,"division" = rank_tier_div))
}
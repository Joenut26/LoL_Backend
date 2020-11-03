#data input needs to be a list of game information from gameId's of RiotAPI with a "/lol/match/v4/matchlists/by-account/" 
#query and wrapped in a fromJSON
#input_name is the summonername


KDA <- function(data,input_name){
  
  participant_df_rows <- data.frame()
  participant_list <- list()
  for (i in 1:length(match_data_month)){
    #get names
    player_names <- lapply(data,"[", "participantIdentities")
    #find entries for inputname only

    int <- which(lapply(lapply(player_names[[i]]$participantIdentities, "[[",2), "[[",3) == input_name)
    #combine name and participant id in dataframe per game
    bind_df <- data.frame("participantID" = player_names[[i]]$participantIdentities[[int]]$participantId,
                          "participantName" = player_names[[i]]$participantIdentities[[int]]$player$summonerName,
                          "teamID" = data[[i]]$participants[[int]]$teamId,
                          "kda" = (data[[i]]$participants[[int]]$stats$kills + 
                                  data[[i]]$participants[[int]]$stats$assists)/
                                  data[[i]]$participants[[int]]$stats$deaths,
                          "champID" = data[[i]]$participants[[int]]$championId)
    
    
    participant_df_rows <- rbind(participant_df_rows,bind_df)
    
    #win or loss
    outcome <- !is.na(match(participant_df_rows$teamID,data[[i]]$teams[[1]]$teamId))
    outcome <- ifelse(outcome,data[[i]]$teams[[1]]$win,data[[i]]$teams[[2]]$win)
    
    participant_df_rows <- cbind(participant_df_rows,outcome)
    
    temp <- list("data" = participant_df_rows)
    participant_list[[i]] <- temp$data
    names(participant_list)[i] <- paste0("gamenumber",i)
    #reset participant_df_rows
    participant_df_rows <- data.frame()
  }
  
  results <- lapply(participant_list, function(x){x[which(x$participantName == input_name),c(2,ncol(bind_df)+1)]})
  results_logic <- unlist(lapply(results,function(x){x$outcome == "Win"}))
  #winrate
  winrate <- sum(results_logic, na.rm = TRUE)/length(results_logic)*100
  #kda
  kda_avg <-sum(unlist(lapply(participant_list,"[[",4)))/length(results_logic)
  
  return(list("player" = participant_list,"winrate" = winrate, "KDA" = kda_avg))
}
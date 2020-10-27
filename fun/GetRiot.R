GetRiot <- function(name, base_url, property_url, api_key){
  #paste alle variables naar 1 string als input voor JSON
  content <- paste0(base_url,property_url,name,paste0("?api_key=",api_key))
  
  return(content)
}

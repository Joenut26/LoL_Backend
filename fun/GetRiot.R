GetRiot <- function(name, base_url, property_url, api_key,props){
  #paste alle variables naar 1 string als input voor JSON

  #voor props moet een "&"
  if(missing(props)){
  content <- paste0(base_url, property_url, name, paste0("?api_key=",api_key))
  }else{
    content <- paste0(base_url, property_url, name, paste0("?api_key=",api_key), var = paste0("&",props))
  }
  
  return(content)
}

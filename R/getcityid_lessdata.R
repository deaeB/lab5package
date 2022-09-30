#' getcityid
#' 
#' get the id of a municipality in kolada's api with less data GET
#' 
#' take a name of a sweden municipality, it will print and return the id of that municipality for further usage within kolada's api
#' 
#' @param name a string, full name of a municipality, Case INsensitive
#' @return a string, the id of the municipality you choose
#' @references https://www.kolada.se/
#' 
#' @examples 
#' 
#' getcityid_lessdata("linköping")
#' 
#' @import httr
#' @export 


getcityid_lessdata <- function(name){
  url <- "http://api.kolada.se/v2/"
  
  if (!is.character(name)) {
    stop("only string allowed")
  }
  
  muniurl <- paste(url, "municipality", "?", "title=", name, sep = "")
  
  getdata <- GET(muniurl)
  
  if(content(getdata)$count == 0){
    return("Oops, notfound. Check is the input correct and a full name")
  }
  
  cityinfo <- content(getdata)$values
  
  cityid <- cityinfo[[1]][["id"]]
  
  return(cityid)
  
}
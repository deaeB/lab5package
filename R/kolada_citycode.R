#' getcityid
#' 
#' get the id of a municipality in kolada's api
#' 
#' take one name of a Sweden municipality among different municipalities of a country so that R will print and return the id of that municipality for further usage within kolada's api
#' 
#' @param name a string, full name of a municipality(may include region-), Case INsensitive
#' @return a string, the id of the municipality you choose
#' @references https://www.kolada.se/
#' 
#' @examples 
#' 
#' getcityid("linköping")
#' 
#' @import httr
#' @export 


# depend: httr
getcityid <- function(name){
  
  getmuni <- GET("http://api.kolada.se/v2/municipality")
  
  if (!is.character(name)) {
    stop("only string allowed")
  }
  
  munilist <- content(getmuni)$values

  for (i in 1:length(munilist)) {
    if (toupper(munilist[[i]]["title"]) == toupper(name)) {
      cityid <- munilist[[i]][["id"]]
      cat(cityid, "is the id of", name, "\n")
      return(cityid)
    }
  }
  
  return("Oops, notfound. Check is the input correct and a full name")
}
# maybe too simple but not enough time





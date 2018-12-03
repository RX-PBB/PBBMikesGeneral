#' trim
#'
#' trim leading and trailing spaces
#' @param x string to trim
#' @export
#' @examples
#' trim(' a string to trim   ')

trim <- function (x) gsub("^\\s+|\\s+$", "", x)


#' db_clean
#'
#' df data frame to clean, will eliminate leading and trailing spaces and also prep the data for upload by taking care of apostrophe's
#' @param x string to trim
#' @export
#' @examples
#' db_clean(df)

db_clean<-function(df){
  #removes spaces and replaces apostrophe's in prep for writing to db

  for (i in 1:length(df)){

    df[,i]<-trim(df[,i])
    df[,i]<-gsub("'","''",df[,i])
  }

  return(df)

}

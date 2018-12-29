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


#' create_empty_df
#'
#' create an empty dataframe
#' @param n.cols number of columns
#' @param col.names column names
#' @export
#' @examples
#' create_empty_df(n.cols,col.names)

create_empty_df<-function(n.cols,col.names){

   df <- as.data.frame(matrix(0,nrow=0,ncol=n.cols),stringsAsFactors=F)
   colnames(df)<-col.names

  return(df)

}


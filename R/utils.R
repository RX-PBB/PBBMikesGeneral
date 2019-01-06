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


#' selectInput_filter
#'
#' subset a dataframe from select input
#' @param df dataframe to subset
#' @param filter input filter
#' @param col.filter column to filter on
#' @param all.option include all term, example "All Available"
#' @param by.rows if NULL then normal filtering, if FALSE then filter by columns. USed in Edit USers filtering.
#' @param includeBlanckDivs not sure if this is used anymore
#' @export
#' @examples
#' create_empty_df(n.cols,col.names)



selectInput_filter<-function(df,filter,col.filter,all.option,by.rows=NULL,includeBlanckDivs=NULL){


  if (is.null(filter))(return(df))

  if (is.element(all.option,filter) && length(filter)>1)(filter<-filter [! filter %in% all.option])

  if (length(filter)==1 && filter==all.option){df<-df
  }else{
    #browser()
    #by rows is default
    if (is.null(by.rows)){
      #Include blanks - this came up with battle creek and shawnee for unamed divisions
      filter<-c(filter,"",includeBlanckDivs)
      df<-df[which(is.element(df[,col.filter],filter)),]
    }
    #if we specify rows or column selection
    if (!is.null(by.rows)){
      if (by.rows==F){
        #browser()
        df<-df[,colnames(df) %in% c('User','SuperUser','ViewUser',filter)]
      }
      if (by.rows==T){

        df<-df[which(is.element(df[,col.filter],filter)),]

      }
    }
  }

  return(df)

}


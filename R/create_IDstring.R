#' create_IDstring
#'
#' helper function for writing SQL querries. Convers a numeric vector into character string typically used in WHERE IN statement
#' @param numeric_vector ndexes to pull a subset.
#' @export
#' @examples
#' statement<-paste("SELECT * FROM Alloc WHERE ItemID IN ",create_IDstring(Items$ItemID),";",sep='')


create_IDstring<-function(numeric_vector){

  if (length(numeric_vector)==0)(return(NULL))

  out<-NULL
  out<-paste("('",numeric_vector[1],sep='')
  if (length(numeric_vector)>1){
    for (i in 2:length(numeric_vector)){
      out<-paste(out,"','",numeric_vector[i],sep='')

    }
  }
  out<-paste(out,"')",sep='')

}

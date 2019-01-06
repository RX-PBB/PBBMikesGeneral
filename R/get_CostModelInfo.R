#' get_CostModelInfo
#'
#' Get the CostModelInfo so we know the names of stuff
#' @param db_name data base to use
#' @param CostModelName The costmodel name, FH or PBB. Typically PBB.
#' @param dbcon If a db con exists we can use it here.
#' @export
#' @examples
#' CostModelInfo<-get_CostModelInfo(db_name='RX_Training5')

get_CostModelInfo<-function(db_name,CostModelName='PBB',db_con=NULL){

  if(is.null(db_con)){
  con <- dbConnect(MySQL(),
                   user="mtseman",
                   password="cree1234",
                   host='ec2-52-11-250-69.us-west-2.compute.amazonaws.com',
                   dbname=db_name)
  }

  statement<-paste("SELECT * FROM CostModelInfo WHERE CostModelName='",CostModelName,"';",sep='')
  CostModelInfo<-dbGetQuery(con,statement)


  if(is.null(db_con)){
    dbDisconnect(con)
  }

  return(CostModelInfo)
}

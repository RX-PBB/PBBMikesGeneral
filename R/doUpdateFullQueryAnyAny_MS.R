#' doUpdateFullQueryAnyAny_MS
#'
#' SQL table update function
#' @param con RMySQL connection
#' @param tableName Table name to update
#' @param dfFull data frame to update
#' @param updateFields subset of fields to update or NULL for all fields
#' @param batchSize number of rows to process, works well up to 2000, generally good up to 5000
#' @export
#' @examples
#' statement<-paste("SELECT * FROM Alloc WHERE ItemID IN ",create_IDstring(Items$ItemID),";",sep='')


doUpdateFullQueryAnyAny_MS<- function(con,tableName,dfFull, updateFields, batchSize) {

  numRows = nrow(dfFull)
  numBatches = ceiling(numRows/batchSize)

  allFields = colnames(dfFull)
  numFieldsAll = length(allFields)

  if (is.null(updateFields)) {
    updateFields = allFields
  }

  numFieldsUpdate = length(updateFields)
  newUpdateArray = vector(length=numFieldsUpdate)

  for ( c in 1:numFieldsUpdate) {
     newUpdateArray[c] = paste(updateFields[c], "=VALUES(", updateFields[c],")",sep='')
  }
  updateFieldStr=paste("", newUpdateArray ,"",collapse=",",sep="")

  for ( b in 1:numBatches) {
    #Log("Batch#: ")
    startRow = (b-1)*batchSize +1
    stopRow = if ((b)*batchSize<numRows) ((b)*batchSize)  else numRows
    df = dfFull[c(startRow:stopRow),]
    #Log(paste("Batch: ", startRow, "to " , stopRow))
    nrows = nrow(df)
    queryArray = vector(length=nrows)
    newValueArray = vector(length=nrows)

    for(i in 1:nrow(df)){

      tmpstr="("
      for (j in 1:numFieldsAll) {
        if (j==numFieldsAll){
          tmpstr=paste(tmpstr, "'",df[i,j],"')",sep="")
        }else {
          tmpstr=paste(tmpstr,"'",df[i,j],"',",sep="")
        }

      }

      newValueArray[i] = tmpstr
    }

    newStr =paste(newValueArray,collapse=", ",sep="")

    q = paste(allFields,collapse=", ",sep="")
    q = paste("(", q, ")",sep="")


    query=paste("INSERT INTO  ", tableName, " ", q, " VALUES ", newStr, " ON DUPLICATE KEY UPDATE ", updateFieldStr, ";",sep='')
    query<-gsub("'NA'","NULL",query) # Will replace all NA's with NULL so the db keeps blanks
    #browser()
    #logjs(query)
    #print(query)
    rs<-dbSendQuery(conn=con, statement=query)
    dbClearResult(rs)

  }
}

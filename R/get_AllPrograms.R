#' get_AllPrograms
#'
#' Function to pull programs in onlinepbb.net/home
#' @param BudgetID If using for download template provide this
#' @param ItemMeta1
#' @param db_name data base to use
#' @param CostModelInfo The cost model info table
#' @param CostModelID Costmodel ID to use, typically the one from the PBB cost model
#' @export
#' @examples
#' ProgInfo<-get_AllPrograms(BudgetID=NULL, ItemMeta1=NULL,db_name,CostModelInfo,CostModelID)



get_AllPrograms<-function(BudgetID=NULL, ItemMeta1=NULL,db_name,CostModelInfo,CostModelID){

  Div1Name=CostModelInfo$Div1Name
  Div2Name=CostModelInfo$Div2Name

  #browser()

  con <- dbConnect(MySQL(),
                   user="mtseman",
                   password="cree1234",
                   host='ec2-52-11-250-69.us-west-2.compute.amazonaws.com',
                   dbname=db_name)


  # Step 1 Get old item IDs
  statement<-paste("SELECT * FROM ProgInfo;",sep='')
  Programs<-dbGetQuery(con,statement)

  # if(admin==FALSE){
  #    Programs<-Programs[c(colnames(Programs)[which(!is.element(colnames(Programs),'RX_ProgID'))])]
  # }

  #If we want to subset by UserGroup
  if(!is.null(ItemMeta1)){

    Programs<-selectInput_filter(Programs,filter=ItemMeta1,col.filter='ItemMeta1',all.option='All Available')
  }

  #If we want to select only programs in a specific budget
  if(!is.null(BudgetID)){
    statement<-paste("SELECT * FROM ProgBudgetInfo WHERE BudgetID=",BudgetID,";",sep='')
    ProgBudgetInfo<-dbGetQuery(con,statement)

    Programs<-Programs[which(is.element(Programs$ProgID,ProgBudgetInfo$ProgID)),]
  }

  dbDisconnect(con)




  #Retrun an empty array if query is no rows
  if (nrow(Programs)==0){
    col.names<-c("ProgID", "CostModelID","ProgNum","ProgName","ProgNameShort","ServiceType", "ScoringType","ProgActive", "ProgDept", "ProgDiv" ,
                 "ProgFund","ProgDescription","ProgComments","ProgGroup","ProgSubGroup","AssignedRevenue", "AssignedAllocatedCost", "AssignedFixedCost",
                 "FinalScore","Quartile","ProgLastUpdated","ProgLastUser",'ItemMeta1')

    Programs<-create_empty_df(n.cols=23,col.names)

  }

  colnames(Programs)[c(3,4,6,7,9,10,11,12,13)]<-c('ProgramNumber','Program','ServiceType','Fixed',Div1Name,Div2Name,'Fund','Description','Comments')

  return(Programs)
}


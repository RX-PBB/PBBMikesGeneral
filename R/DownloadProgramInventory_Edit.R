#' DownloadProgramInventory_Edit
#'
#' Some standard templates for ProgInventory and Costs
#' @param fname If using for download template provide this
#' @param db_name
#' @param BudgetID Table name to update
#' @param ItemMeta1 Subset by a user group
#' @param CostModelInfo The cost model info table
#' @param CostModelID Costmodel ID to use, typically the one from the PBB cost model
#' @param include.sheets Which sheets to include in the list of standard templates
#' @param include.RX_ProgID Include the RX_ProgID that relates back to master list
#' @export
#' @examples
#' ProgInfo<-DownloadProgramInventory_Edit(fname=NULL,BudgetID=NULL,ItemMeta1=NULL,CostModelInfo,CostModelID)


DownloadProgramInventory_Edit<-function(fname,db_name,CostModelInfo,CostModelID,
                                        BudgetID=NULL,ItemMeta1=NULL,
                                        include.sheets=c('Program Inventory','BudgetInfo','ProgBudgetInfo'),
                                        include.RX_ProgID=T){


  Div1Name=CostModelInfo$Div1Name
  Div2Name=CostModelInfo$Div2Name

  Programs<-NULL
  BudgetInfo<-NULL
  ProgBudgetInfo<-NULL

  index<-0
  sheets<-list()
  col.widths<-list()

  if(is.element('Program Inventory',include.sheets)){
    index<-index+1

    Programs<-get_AllPrograms(BudgetID=BudgetID,ItemMeta1=ItemMeta1,db_name,CostModelInfo,CostModelID)
    Programs<-Programs[order(Programs$ItemMeta1),]

    Programs[,'Delete']<-'NO'

    #Include these if Admin and if the table has RX_ProgID's
    RX_ProgID<-NULL
    RX_ProgID.col<-NULL
    RX_ProgID.width<-NULL
    if(include.RX_ProgID==T){

      if (is.element('RX_ProgID',colnames(Programs))){
        RX_ProgID<-'RX_ProgID'
        RX_ProgID.col<-13
        RX_ProgID.width<-15
      }

    }
    Programs<-Programs[c('ItemMeta1','ServiceType',Div1Name,Div2Name,'ProgramNumber','Program','Description','Comments','ProgGroup','ProgSubGroup','Delete','ProgID',RX_ProgID)]
    colnames(Programs)[c(5,1)]<-c('Prog#','UserGroup')

    #browser()

    sheets[[index]]<-'Program Inventory'

    col.widths[[index]]<-list()
    col.widths[[index]]$cols<-c(1,2,3,4,5,6,7,8,9,10,11,12,RX_ProgID.col)
    col.widths[[index]]$widths<-c(20,20,20,20,12,40,60,20,20,20,20,15,RX_ProgID.width)
  }


  #Now get Budget info and ProgBudget Info
  con <- dbConnect(MySQL(),
                   user="mtseman",
                   password="cree1234",
                   host='ec2-52-11-250-69.us-west-2.compute.amazonaws.com',
                   dbname=db_name)



  #*********************************
  # Prepare sheet 2 for Budget info
  #*********************************
  if(is.element('BudgetInfo',include.sheets)){

    statement<-paste("SELECT * FROM BudgetInfo WHERE CostModelID=",CostModelID,";",sep='')
    BudgetInfo<-dbGetQuery(con,statement)

    index<-index+1

    sheets[[index]]<-'BudgetInfo'

    col.widths[[index]]<-list()
    col.widths[[index]]$cols<-c(1,2,3,4,5,6,7,8,9,10,11)
    col.widths[[index]]$widths<-c(15,15,12,20,15,20,15,20,15,30,25)
  }
  #*********************************
  # Prepare sheet 3 for ProgBudget info
  #*********************************
  if(is.element('ProgBudgetInfo',include.sheets)){
    index<-index+1

    statement<-paste("SELECT * FROM ProgBudgetInfo;",sep='')
    ProgBudgetInfo<-dbGetQuery(con,statement)

    ProgBudgetInfo<-merge(ProgBudgetInfo,BudgetInfo[,c('BudgetID','Year','BudgetName')],by='BudgetID')
    ProgBudgetInfo<-merge(ProgBudgetInfo,Programs[,c('ProgID','Prog#','Program')],by='ProgID')
    ProgBudgetInfo<-ProgBudgetInfo[c('BudgetID','ProgID','Year','BudgetName','Prog#','Program')]
    ProgBudgetInfo<-ProgBudgetInfo[order(ProgBudgetInfo$ProgID,-ProgBudgetInfo$BudgetID),]

    sheets[[index]]<-'ProgBudgetInfo'

    col.widths[[index]]<-list()
    col.widths[[index]]$cols<-c(1,2,3,4,5,6)
    col.widths[[index]]$widths<-c(15,15,12,25,15,40)
  }

  dbDisconnect(con)

  #browser()
  if(!is.null(fname)){

    Programs<-list(Programs,BudgetInfo,ProgBudgetInfo)
    saveWorkbook.openxlsx(make_ExcelTableOut(Programs,sheets,col.widths),fname)

  }else{

    return(Programs)
  }

}


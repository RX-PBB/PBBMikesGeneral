#' get_InvandCosts
#'
#' SQL table update function
#' @param BudgetID BudegtID to pull
#' @param db_name Table name to update
#' @param CostModelName Typically "PBB"
#' @export
#' @examples
#' data<-get_InvandCosts(BudgetID=1,db_name='RX_Training5')


get_InvandCosts<-function(BudgetID,db_name,CostModelName='PBB'){

  con <- dbConnect(MySQL(),
                   user="mtseman",
                   password="cree1234",
                   host='ec2-52-11-250-69.us-west-2.compute.amazonaws.com',
                   dbname=db_name)


  statement<-paste("SELECT * FROM CostModelInfo;",sep='')
  CostModelInfo<-dbGetQuery(con,statement)

  CostModelID<-CostModelInfo[CostModelInfo$CostModelName=='PBB','CostModelID']
  CostModelInfo<-CostModelInfo[CostModelInfo$CostModelID==CostModelID,]

  statement<-paste("SELECT * FROM ItemInfo WHERE BudgetID=",BudgetID," AND CostModelID=",CostModelID,";",sep='')
  ItemInfo<-dbGetQuery(con,statement)

  AcctIDs<-unique(ItemInfo$AcctID)
  statement<-paste("SELECT * FROM AcctInfo WHERE AcctID IN ",create_IDstring(AcctIDs),";",sep='')
  AcctInfo<-dbGetQuery(con,statement)

  statement<-paste("SELECT * FROM FundInfo WHERE CostModelID=",CostModelID,";",sep='')
  FundInfo<-dbGetQuery(con,statement)

  statement<-paste("SELECT * FROM Div1Info WHERE CostModelID=",CostModelID,";",sep='')
  Div1Info<-dbGetQuery(con,statement)

  statement<-paste("SELECT * FROM Div2Info WHERE CostModelID=",CostModelID,";",sep='')
  Div2Info<-dbGetQuery(con,statement)

  statement<-paste("SELECT * FROM Div3Info WHERE CostModelID=",CostModelID,";",sep='')
  Div3Info<-dbGetQuery(con,statement)

  statement<-paste("SELECT * FROM Div4Info WHERE CostModelID=",CostModelID,";",sep='')
  Div4Info<-dbGetQuery(con,statement)

  statement<-paste("SELECT * FROM Div5Info WHERE CostModelID=",CostModelID,";",sep='')
  Div5Info<-dbGetQuery(con,statement)

  statement<-paste("SELECT * FROM Div6Info WHERE CostModelID=",CostModelID,";",sep='')
  Div6Info<-dbGetQuery(con,statement)

  statement<-paste("SELECT * FROM Div7Info WHERE CostModelID=",CostModelID,";",sep='')
  Div7Info<-dbGetQuery(con,statement)

  statement<-paste("SELECT * FROM Div8Info WHERE CostModelID=",CostModelID,";",sep='')
  Div8Info<-dbGetQuery(con,statement)

  statement<-paste("SELECT * FROM Obj1Info WHERE CostModelID=",CostModelID,";",sep='')
  Obj1Info<-dbGetQuery(con,statement)

  statement<-paste("SELECT * FROM Obj2Info WHERE CostModelID=",CostModelID,";",sep='')
  Obj2Info<-dbGetQuery(con,statement)

  statement<-paste("SELECT * FROM Obj3Info WHERE CostModelID=",CostModelID,";",sep='')
  Obj3Info<-dbGetQuery(con,statement)

  statement<-paste("SELECT * FROM Obj4Info WHERE CostModelID=",CostModelID,";",sep='')
  Obj4Info<-dbGetQuery(con,statement)

  statement<-paste("SELECT * FROM ProgBudgetInfo WHERE BudgetID=",BudgetID,";",sep='')
  ProgBudgetInfo<-dbGetQuery(con,statement)

  ProgIDs<-unique(ProgBudgetInfo$ProgID)
  statement<-paste("SELECT * FROM ProgInfo WHERE ProgID IN ",create_IDstring(ProgIDs),";",sep='')
  ProgInfo<-dbGetQuery(con,statement)

  dbDisconnect(con)


  df<-merge(ItemInfo,AcctInfo,by=c('AcctID','CostModelID'))
  df<-merge(df,FundInfo[c('FundID','Fund')],by='FundID')
  df<-merge(df,Div1Info[c('Div1ID','Div1')],by='Div1ID')
  df<-merge(df,Div2Info[c('Div2ID','Div2')],by='Div2ID')
  df<-merge(df,Div3Info[c('Div3ID','Div3')],by='Div3ID')
  df<-merge(df,Div4Info[c('Div4ID','Div4')],by='Div4ID')
  df<-merge(df,Div5Info[c('Div5ID','Div5')],by='Div5ID')
  df<-merge(df,Div6Info[c('Div6ID','Div6')],by='Div6ID')
  df<-merge(df,Div7Info[c('Div7ID','Div7')],by='Div7ID')
  df<-merge(df,Div8Info[c('Div8ID','Div8')],by='Div8ID')

  df<-merge(df,Obj1Info[c('Obj1ID','Obj1')],by='Obj1ID')
  df<-merge(df,Obj2Info[c('Obj2ID','Obj2')],by='Obj2ID')
  df<-merge(df,Obj3Info[c('Obj3ID','Obj3')],by='Obj3ID')
  df<-merge(df,Obj4Info[c('Obj4ID','Obj4')],by='Obj4ID')

  fields<-c('ItemID','AcctID','ItemMeta1','TotalCost','ObjType','AcctType','NumberOfItems','Obj1','Obj2','Obj3','Obj4','Fund','Div1','Div2','Div3','Div4','Div5','Div6','Div7',
            'Div8')

  df<-df[fields]
  colnames(df)[colnames(df)=='Div1']<-CostModelInfo$Div1Name
  colnames(df)[colnames(df)=='Div2']<-CostModelInfo$Div2Name
  colnames(df)[colnames(df)=='Div3']<-CostModelInfo$Div3Name
  colnames(df)[colnames(df)=='Div4']<-CostModelInfo$Div4Name
  colnames(df)[colnames(df)=='Div5']<-CostModelInfo$Div5Name
  colnames(df)[colnames(df)=='Div6']<-CostModelInfo$Div6Name
  colnames(df)[colnames(df)=='Div7']<-CostModelInfo$Div7Name
  colnames(df)[colnames(df)=='Div8']<-CostModelInfo$Div8Name

  Personnel<-df[df$ObjType=='Personnel',]
  colnames(Personnel)[colnames(Personnel)=='Obj1']<-CostModelInfo$Obj1NameP
  colnames(Personnel)[colnames(Personnel)=='Obj2']<-CostModelInfo$Obj2NameP
  colnames(Personnel)[colnames(Personnel)=='Obj3']<-CostModelInfo$Obj3NameP
  colnames(Personnel)[colnames(Personnel)=='Obj4']<-CostModelInfo$Obj4NameP

  Personnel<-Personnel[ , -which(colnames(Personnel) %in% c('ObjType','AcctType','Div1','Div2','Div3','Div4','Div5','Div6','Div7','Div8','Obj1','Obj2','Obj3','Obj4'))]
  Personnel<-Personnel[order(Personnel$ItemMeta1),]

  NonPersonnel<-df[df$ObjType=='NonPersonnel',]
  colnames(NonPersonnel)[colnames(NonPersonnel)=='Obj1']<-CostModelInfo$Obj1Name
  colnames(NonPersonnel)[colnames(NonPersonnel)=='Obj2']<-CostModelInfo$Obj2Name
  colnames(NonPersonnel)[colnames(NonPersonnel)=='Obj3']<-CostModelInfo$Obj3Name
  colnames(NonPersonnel)[colnames(NonPersonnel)=='Obj4']<-CostModelInfo$Obj4Name

  NonPersonnel<-NonPersonnel[ , -which(colnames(NonPersonnel) %in% c('ObjType','Div1','Div2','Div3','Div4','Div5','Div6','Div7','Div8','Obj1','Obj2','Obj3','Obj4'))]
  NonPersonnel<-NonPersonnel[order(NonPersonnel$AcctType,NonPersonnel$ItemMeta1),]

  Revenue<-NonPersonnel[NonPersonnel$AcctType=='Revenue',]
  NonPersonnel<-NonPersonnel[NonPersonnel$AcctType=='Expense',]

  #ProgInfo<-ProgInfo[c('ProgID','ItemMeta1','ProgDept','ProgDiv','ProgNum','ProgName','ProgDescription')]
  #ProgInfo<-ProgInfo[order(ProgInfo$ItemMeta1),]
  ProgInfo<-DownloadProgramInventory_Edit(fname=NULL,BudgetID=NULL,ItemMeta1=NULL,include.sheets=c('Program Inventory'))
  #browser()

  data<-list()
  data$Inventory<-ProgInfo
  data$Personnel<-Personnel
  data$NonPersonnel<-NonPersonnel
  data$Revenue<-Revenue

  return(data)
}


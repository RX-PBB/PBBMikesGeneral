#' get_RadSlicer
#'
#' Get Program Inventory Costs and Revenue
#' @param BudgetID BudegtID to pull
#' @param db_name Table name to update
#' @export
#' @examples
#' data<-get_RadSlicer(BudgetID=1,db_name='RX_Training5')


get_RadSlicer<-function(db_name,BudgetIDs){

  start<-Sys.time()

  exlcude.names<-c('Exclude','EXCLUDES')

  con <- dbConnect(MySQL(),
                   user="mtseman",
                   password="cree1234",
                   host='ec2-52-11-250-69.us-west-2.compute.amazonaws.com',
                   dbname=db_name)


  statement<-paste("SELECT * FROM CostModelInfo;",sep='')
  CostModelInfo<-dbGetQuery(con,statement)

  CostModelID<-CostModelInfo[CostModelInfo$CostModelName=='PBB','CostModelID']
  CostModelInfo<-CostModelInfo[CostModelInfo$CostModelID==CostModelID,]

  CostModelID<-CostModelID

  #browser()
  #needs to be updated to pull all budgets that are view

  #browser()
  if (length(BudgetIDs)==0){

    session$sendCustomMessage(type = 'alertmessage',
                              message=paste('No budgets currently open for viewing, please contact your SuperUser',sep=''))
    return(NULL)

  }

  #Now get all buget cost summaries
  temp<-NULL
  for (i in 1:length(BudgetIDs)){
    BudgetID<-BudgetIDs[i]
    AcctSummary<-get_ProgramCosts(db_name,BudgetID)
    temp<-rbind(AcctSummary,temp)
  }

  AcctSummary<-temp
  #browser()
  df<-get_FinalScores(db_name,All.Programs=T)
  FinalScores<-df

  statement<-paste("SELECT ResultType, Scored FROM ResultTypes;",sep='')
  ResultTypes<-dbGetQuery(con,statement)

  statement<-paste("SELECT * FROM BudgetInfo;",sep='')
  BudgetInfo<-dbGetQuery(con,statement)

  statement<-paste("SELECT * FROM Scores;",sep='')
  Scores<-dbGetQuery(con,statement)

  statement<-paste("SELECT * FROM ResultSetup;",sep='')
  ResultSetup<-dbGetQuery(con,statement)

  statement<-paste("SELECT * FROM ResultDefs;",sep='')
  ResultDefs<-dbGetQuery(con,statement)

  statement<-paste("SELECT * FROM ProgBudgetInfo WHERE BudgetID IN ",create_IDstring(BudgetIDs),";",sep='')
  ProgBudgetInfo<-dbGetQuery(con,statement)

  statement<-paste("SELECT * FROM PBBComments;",sep='')
  PBBComments<-dbGetQuery(con,statement)

  statement<-paste("SELECT * FROM RXComments;",sep='')
  RXComments<-dbGetQuery(con,statement)

  statement<-paste("SELECT * FROM ProgInfo;",sep='')
  ProgInfo<-dbGetQuery(con,statement)

  dbDisconnect(con)

  ResultAbbr<-ResultSetup[which(is.element(ResultSetup$ResultType,ResultTypes[ResultTypes$Scored==1,"ResultType"])),] #Linker df

  BPAs<-Scores[which(is.element(Scores$ResultID,ResultAbbr[ResultAbbr$ResultType=='BPA','ResultID'])),]
  BPAs<-BPAs[c('ProgID','ResultID','ScorePeer')]
  BPAs<-dcast(BPAs,ProgID~ResultID,value.var = 'ScorePeer',fill = NA)
  #update colnames to BPAs names
  BPAs<-colnameIDs_to_Names(df.update=BPAs,df.LinkID_Name=ResultAbbr,ID='ResultID',Name='ResultAbbr')
  BPAs.names<-colnames(BPAs[which(!is.element(colnames(BPAs),'ProgID'))])

  Results<-Scores[which(is.element(Scores$ResultID,ResultAbbr[ResultAbbr$ResultType!='BPA','ResultID'])),]
  Results<-Results[c('ProgID','ResultID','ScorePeer')]
  Results<-dcast(Results,ProgID~ResultID,value.var = 'ScorePeer',fill = NA)
  Results<-colnameIDs_to_Names(df.update=Results,df.LinkID_Name=ResultAbbr,ID='ResultID',Name='ResultAbbr')
  Results.names<-colnames(Results[which(!is.element(colnames(Results),'ProgID'))])

  #browser()
  #Some merging and combining to get Budget ID's and Q's
  AcctSummary[,'ProgramCost']<-AcctSummary[,'TotalCost']*AcctSummary[,'PercentAppliedToProg']
  AcctSummary<-merge(AcctSummary,ResultTypes[c('ResultType','Scored')],by.x='ServiceType',by.y='ResultType')
  AcctSummary<-merge(AcctSummary,BudgetInfo[c('BudgetID','Year','BudgetName')],by='BudgetID')
  AcctSummary[AcctSummary$Scored==1,'Scored']<-'Prioritized'
  AcctSummary[AcctSummary$Scored==0,'Scored']<-'Non-Prioritized'

  df<-df$FinalScores
  df<-df[df$Score=='Peer',]
  if(!is.null(df)){
    AcctSummary<-merge(AcctSummary,df[c('ProgID','Quartile','FinalScore')],by='ProgID',all.x = T)
    AcctSummary[is.na(AcctSummary$Quartile),'Quartile']<-'Non-Prioritized'
  }


  colnames(AcctSummary)[colnames(AcctSummary)=='Div1']<-CostModelInfo$Div1Name
  colnames(AcctSummary)[colnames(AcctSummary)=='Div2']<-CostModelInfo$Div2Name
  colnames(AcctSummary)[colnames(AcctSummary)=='Div3']<-CostModelInfo$Div3Name
  colnames(AcctSummary)[colnames(AcctSummary)=='Div4']<-CostModelInfo$Div4Name
  colnames(AcctSummary)[colnames(AcctSummary)=='Div5']<-CostModelInfo$Div5Name
  colnames(AcctSummary)[colnames(AcctSummary)=='Div6']<-CostModelInfo$Div6Name
  colnames(AcctSummary)[colnames(AcctSummary)=='Div7']<-CostModelInfo$Div7Name
  colnames(AcctSummary)[colnames(AcctSummary)=='Div8']<-CostModelInfo$Div8Name

  #Add a Year - Budget Column
  AcctSummary[,'BudgetYearName']<-paste(AcctSummary[,'Year']," - ",AcctSummary[,'BudgetName'],sep='')

  #********************************
  # Update with Scores
  #*********************************

  AcctSummary<-merge(AcctSummary,BPAs,by='ProgID',all.x=T)
  AcctSummary<-merge(AcctSummary,Results,by='ProgID',all.x=T)

  #**********************************************************************
  #Update with Comments - use RXComment ID to style by standard basis set
  #**********************************************************************
  #browser()
  CommentIDs<-merge(ProgBudgetInfo[c('ProgID','BudgetID','PBBCommentID')],PBBComments,by='PBBCommentID')
  AcctSummary<-merge(AcctSummary,CommentIDs,by=c('ProgID','BudgetID'),all.x=T)
  colnames(AcctSummary)[colnames(AcctSummary)=='RXCommentID.y']<-'RXCommentID'

  #**********************************************************************
  #Update with ProgDescription
  #**********************************************************************
  AcctSummary<-merge(AcctSummary,ProgInfo[c('ProgID','ProgDescription','ProgGroup')],by=c('ProgID'),all.x=T)

  #**********************************************************************
  #Exclude!!
  #**********************************************************************
  AcctSummary<-AcctSummary[which(!is.element(AcctSummary$ItemMeta1,exlcude.names)),]


  df<-list()
  df$AcctSummary<-AcctSummary
  df$ResultSetup<-ResultSetup
  df$ResultTypes<-ResultTypes
  df$BPA.names<-BPAs.names
  df$Result.names<-Results.names
  df$ResultDefs<-ResultDefs
  df$ProgBudgetInfo<-ProgBudgetInfo
  df$ProgInfo<-ProgInfo
  df$PBBComments<-PBBComments
  df$Scores<-Scores
  df$FinalScores<-FinalScores
  return(df)

}

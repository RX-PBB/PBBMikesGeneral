#' get_ProgramCosts
#'
#' Get Program Inventory Costs and Revenue
#' @param BudgetID BudegtID to pull
#' @param db_name Table name to update
#' @export
#' @examples
#' data<-get_ProgramCosts(BudgetID=1,db_name='RX_Training5')

get_ProgramCosts<-function(db_name,BudgetID,ItemIDs=NULL,UserGroups='All Available'){

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
  statement<-paste("SELECT Obj1ID, ObjType,Obj1,Obj1Code FROM Obj1Info WHERE CostModelID='",CostModelID,"';",sep='')
  Obj1<-dbGetQuery(con,statement)
  if(nrow(Obj1)>0)(colnames(Obj1)[2]<-'Cost Type')

  statement<-paste("SELECT Obj2ID, Obj2,Obj2Code FROM Obj2Info WHERE CostModelID='",CostModelID,"';",sep='')
  Obj2<-dbGetQuery(con,statement)

  statement<-paste("SELECT Obj3ID, Obj3,Obj3Code FROM Obj3Info WHERE CostModelID='",CostModelID,"';",sep='')
  Obj3<-dbGetQuery(con,statement)

  statement<-paste("SELECT Obj4ID, Obj4,Obj4Code FROM Obj4Info WHERE CostModelID='",CostModelID,"';",sep='')
  Obj4<-dbGetQuery(con,statement)

  statement<-paste("SELECT Div1ID, Div1,Div1Code FROM Div1Info WHERE CostModelID='",CostModelID,"';",sep='')
  Div1<-dbGetQuery(con,statement)

  statement<-paste("SELECT Div2ID, Div2,Div2Code FROM Div2Info WHERE CostModelID='",CostModelID,"';",sep='')
  Div2<-dbGetQuery(con,statement)

  statement<-paste("SELECT Div3ID, Div3,Div3Code FROM Div3Info WHERE CostModelID='",CostModelID,"';",sep='')
  Div3<-dbGetQuery(con,statement)

  statement<-paste("SELECT Div4ID, Div4,Div4Code FROM Div4Info WHERE CostModelID='",CostModelID,"';",sep='')
  Div4<-dbGetQuery(con,statement)

  statement<-paste("SELECT Div5ID, Div5,Div5Code FROM Div5Info WHERE CostModelID='",CostModelID,"';",sep='')
  Div5<-dbGetQuery(con,statement)

  statement<-paste("SELECT Div6ID, Div6,Div6Code FROM Div6Info WHERE CostModelID='",CostModelID,"';",sep='')
  Div6<-dbGetQuery(con,statement)

  statement<-paste("SELECT Div7ID, Div7,Div7Code FROM Div7Info WHERE CostModelID='",CostModelID,"';",sep='')
  Div7<-dbGetQuery(con,statement)

  statement<-paste("SELECT Div8ID, Div8,Div8Code FROM Div8Info WHERE CostModelID='",CostModelID,"';",sep='')
  Div8<-dbGetQuery(con,statement)

  #Get the Fund Info
  statement<-paste("SELECT Fund, FundID FROM FundInfo WHERE CostModelID = ",CostModelID,";",sep='')
  Funds<-dbGetQuery(con,statement)

  #Programs
  statement<-paste("SELECT * FROM ProgInfo WHERE CostModelID='",CostModelID,"';",sep='')
  ProgInfo<-dbGetQuery(con,statement)
  if(nrow(ProgInfo)==0){

    session$sendCustomMessage(type = 'alertmessage',
                              message=paste('There are no programs loaded to check allocations against.',sep=''))
    return(NULL)
  }


  #Now subset the Programs that are included in this budget.
  #Note: for restricting what gets edited we will take care of that on upload. This way we can see existing from any previous budget
  #BudgetID<-getBudgetID_from_inputYear(con.exists=con,selectYear=input$Year,CostModelID)

  #Filter Budget by active programs - NEED to ADD this back AND fix NULL CASE for start-up
  BudgetProgIDs<-get_BudgetProgIDs(BudgetID,conexists=con,Div1Name = CostModelInfo$Div1Name,Div2Name = CostModelInfo$Div2Name)
  BudgetProgIDs<-BudgetProgIDs[,'ProgID']
  #browser()
  #subset ProgInfo by the programIDs tied to this budget
  ProgInfo<-ProgInfo[which(is.element(ProgInfo$ProgID,BudgetProgIDs)),]
  if(nrow(ProgInfo)==0){

    ProgInfo<-NULL

    session$sendCustomMessage(type = 'alertmessage',
                              message=paste('This Budget is closed for editing, please contact your superuser',sep=''))
    return(NULL)
  }



  #Get Programs and Program Alloations by BudgetID:

  #*****************************************************************


  if(is.null(ItemIDs)){
    #Get the Item and Acct Info Make it faster by subsetting by user group pull
    #if(UserGroup=='All Available'){
    statement<-paste("SELECT ItemID, AcctID, ItemMeta1 FROM ItemInfo WHERE CostModelID = ",CostModelID," AND BudgetID = ",BudgetID,";",sep='')
    Items<-dbGetQuery(con,statement)

    #Subset by user group to improve speed.
    #If All Avaliable is a member of userdepartments then do not subset Item IDs and pull all.

    if(!is.element('All Available',UserGroups)){
      Items<-Items[which(is.element(Items$ItemMeta1,values.setup$userdepartments)),c('ItemID','AcctID')]
    }

    #Make this on User group as well to
    statement<-paste("SELECT * FROM Alloc WHERE ItemID IN ",create_IDstring(Items$ItemID),";",sep='')
    Alloc<-dbGetQuery(con,statement)
    #browser()
    if(nrow(Alloc)==0){

      Alloc<-create_empty_df(n.cols=9,col.names=c('ItemID','PercentAppliedToProg','ProgID','AllocComments','AllocLastUpdated','AllocLastUser','byFTE','byFTEDivision','BudgetID'))

    }


    #browser()
    # statement<-paste("SELECT * FROM
    #                  ((SELECT * FROM ItemInfo WHERE CostModelID = ",CostModelID," AND BudgetID= ",BudgetID,") items
    #                  INNER JOIN AcctInfo ON items.AcctID = AcctInfo.AcctID);",sep='')
    # acctinfo<-dbGetQuery(con,statement)
    # acctinfo<-acctinfo[ , -which(names(acctinfo) %in% c("CostModelID"))]

    statement<-paste("SELECT * FROM
                     ((SELECT * FROM ItemInfo WHERE ItemID IN ",create_IDstring(Items$ItemID),") items
                     INNER JOIN AcctInfo ON items.AcctID = AcctInfo.AcctID);",sep='')
    acctinfo<-dbGetQuery(con,statement)
    acctinfo<-acctinfo[ , -which(names(acctinfo) %in% c("CostModelID"))]



  }
  #*******************************************************************************
  #Make this on User group as well to
  if(!is.null(ItemIDs)){
    statement<-paste("SELECT * FROM Alloc WHERE ItemID IN ",create_IDstring(ItemIDs),";",sep='')
    Alloc<-dbGetQuery(con,statement)
    #browser()
    if(nrow(Alloc)==0){

      Alloc<-create_empty_df(n.cols=9,col.names=c('ItemID','PercentAppliedToProg','ProgID','AllocComments','AllocLastUpdated','AllocLastUser','byFTE','byFTEDivision','BudgetID'))

    }

    #browser()
    #statement<-paste("SELECT * FROM AcctInfo WHERE CostModelID = ",CostModelID,";",sep='')
    statement<-paste("SELECT * FROM
                     ((SELECT * FROM ItemInfo WHERE ItemID IN ",create_IDstring(ItemIDs),") items
                     INNER JOIN AcctInfo ON items.AcctID = AcctInfo.AcctID);",sep='')
    acctinfo<-dbGetQuery(con,statement)
    acctinfo<-acctinfo[ , -which(names(acctinfo) %in% c("CostModelID"))]

  }

  #********************************************************************************

  if (nrow(acctinfo)==0)(return(NULL))


  statement<-paste("SELECT * FROM ProgBudgetInfo WHERE BudgetID = ",BudgetID,";",sep='')
  ProgBudgetInfo<-dbGetQuery(con,statement)

  statement<-paste("SELECT * FROM PBBComments;",sep='')
  PBBComments<-dbGetQuery(con,statement)

  dbDisconnect(con)

  #browser()
  if(!is.null(Alloc)){
    #browser()
    allocations<-merge(acctinfo,Alloc[c('ItemID','PercentAppliedToProg','ProgID','AllocComments','AllocLastUpdated','AllocLastUser','byFTE','byFTEDivision')],by='ItemID',all.x=T)
    allocations[is.na(allocations$PercentAppliedToProg),'PercentAppliedToProg']<-0

    #Merge Objects
    allocations<-merge(allocations,Funds,by='FundID',all.x=T)
    allocations<-merge(allocations,Obj1,by='Obj1ID',all.x=T)
    allocations<-merge(allocations,Obj2,by='Obj2ID',all.x=T)
    allocations<-merge(allocations,Obj3,by='Obj3ID',all.x=T)
    allocations<-merge(allocations,Obj4,by='Obj4ID',all.x=T)

    #Merge Divisions
    #browser()
    allocations<-merge(allocations,Div1,by='Div1ID',all.x=T)
    allocations<-merge(allocations,Div2,by='Div2ID',all.x=T)
    allocations<-merge(allocations,Div3,by='Div3ID',all.x=T)
    allocations<-merge(allocations,Div4,by='Div4ID',all.x=T)
    allocations<-merge(allocations,Div5,by='Div5ID',all.x=T)
    allocations<-merge(allocations,Div6,by='Div6ID',all.x=T)
    allocations<-merge(allocations,Div7,by='Div7ID',all.x=T)
    allocations<-merge(allocations,Div8,by='Div8ID',all.x=T)

    allocations<-merge(allocations,ProgInfo[c('ProgID','ProgNum','ProgName','ServiceType')],by='ProgID',all.x=T)

    allocations[,'obj_level_01']<-'Level1'
    allocations[,'obj_level_02']<-'Level2'
    allocations[,'obj_level_03']<-'Level3'
    allocations[,'obj_level_04']<-'Level4'

    #helper function to create and name some object levels
    obj_level_names<-function(allocations,obj='Obj1',objcode='Obj1Code',level='obj_level_01'){

      #Both Code and Name
      # browser()
      allocations[allocations[,obj]!=obj & allocations[,objcode]!=objcode,level]<-paste("(",allocations[allocations[,obj]!=obj & allocations[,objcode]!=objcode,objcode],") ",
                                                                                        allocations[allocations[,obj]!=obj & allocations[,objcode]!=objcode,obj],sep='')
      # Obj1 Only Used
      allocations[allocations[,obj]!=obj & allocations[,objcode]==objcode,level]<-paste(allocations[allocations[,obj]!=obj & allocations[,objcode]==objcode,obj],sep='')

      # Obj1Code Only Used
      allocations[allocations[,obj]==obj & allocations[,objcode]!=objcode,level]<-paste(allocations[allocations[,obj]==obj & allocations[,objcode]!=objcode,objcode],sep='')

      # This Obj Level not Used
      allocations[allocations[,obj]==obj & allocations[,objcode]==objcode,level]<-'Classifiction Level Not Used'

      return(allocations)
    }


    allocations<-obj_level_names(allocations,obj='Obj1',objcode='Obj1Code',level='obj_level_01')
    allocations<-obj_level_names(allocations,obj='Obj2',objcode='Obj2Code',level='obj_level_02')
    allocations<-obj_level_names(allocations,obj='Obj3',objcode='Obj3Code',level='obj_level_03')
    allocations<-obj_level_names(allocations,obj='Obj4',objcode='Obj4Code',level='obj_level_04')

    #Add comments
    CommentIDs<-merge(ProgBudgetInfo[c('ProgID','BudgetID','PBBCommentID')],PBBComments,by='PBBCommentID')
    allocations<-merge(allocations,CommentIDs[c('ProgID','BudgetID','RXCommentID')],by=c('ProgID','BudgetID'),all.x=T)


    allocations<-allocations[c('ItemID','AcctID','Cost Type','AcctType','ItemMeta1','Obj1','Obj1Code','PercentAppliedToProg','NumberOfItems','TotalCost','ProgID','ProgNum','ProgName','AllocComments','AllocLastUpdated','AllocLastUser','byFTE','byFTEDivision','NameMeta','Fund','AcctCode','obj_level_01','obj_level_02','obj_level_03','obj_level_04','Obj2','Obj3','Obj4','Div1','Div2','Div3','Div4','Div5','Div6','Div7','Div8','ServiceType','BudgetID','RXCommentID')]
    allocations[,'RXCommentID_cp']<- allocations[,'RXCommentID']

    #Exclude!!
    allocations<-allocations[which(!is.element(allocations$ItemMeta1,exlcude.names)),]

    allocations$TotalCost<-as.numeric(allocations$TotalCost)
    allocations$PercentAppliedToProg<-as.numeric(allocations$PercentAppliedToProg)

    allocations[,'ProgramCost_Alloc']<-0
    allocations[,'ProgramCost_Alloc']<-allocations[,'TotalCost']*allocations[,'PercentAppliedToProg']

  }else{allocations<-NULL}

  return(allocations)
}

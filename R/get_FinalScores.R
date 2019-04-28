#' get_ProgramCosts
#'
#' Get Program Inventory Costs and Revenue
#' @param BudgetID BudegtID to pull
#' @param db_name Table name to update
#' @export
#' @examples
#' data<-get_ProgramCosts(BudgetID=1,db_name='RX_Training5')

get_FinalScores<-function(db_name,All.Programs=F){

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
    statement<-paste("SELECT * FROM Scores WHERE CostModelID =",CostModelID,";",sep='')
    Scores<-dbGetQuery(con,statement)

    statement<-paste("SELECT * FROM ResultSetup WHERE CostModelID =",CostModelID,";",sep='')
    AllResults<-dbGetQuery(con,statement)

    statement<-paste("SELECT * FROM ResultTypes;",sep='')
    ResultTypes<-dbGetQuery(con,statement)

    statement<-paste("SELECT * FROM ResultDefs;",sep='')
    Definitions<-dbGetQuery(con,statement)

    statement<-paste("SELECT * FROM ProgInfo;",sep='')
    ProgInfo<-dbGetQuery(con,statement)

    if(All.Programs==F){
      ProgInfo<-Programs_for_Scoring(con,CostModelID)
    }

    #browser()

    #If we have no scores, return NULL, this is unlikely if we are moving through the process properly
    if(nrow(Scores)==0){

      return(NULL)

    }

   dbDisconnect(con)

    #Combine Scores with Results - update the scores with ResultType, Abbr
    PeerReview<-merge(Scores,AllResults[c('ResultID','ResultAbbr','ResultType','Weight')],by='ResultID')

    #Add Programs - Force All programs in case there are missing dept
    PeerReview<-merge(ProgInfo[c('ProgID','ProgDept','ProgName', 'ProgDescription','ServiceType','ItemMeta1','ProgNum')],PeerReview,by='ProgID',all.x=T)

    #Exclude non scored service Types
    Scored.ResultTypes<-ResultTypes[ResultTypes$Scored==1,'ResultType']
    PeerReview<-PeerReview[which(is.element(PeerReview$ServiceType,Scored.ResultTypes)),]

    #We have to include only the scores for the current designation of the program
    PeerReview.results<-PeerReview[PeerReview$ServiceType==PeerReview$ResultType,]
    #But the line above we will lose the BPAs, go back and add those.
    PeerReview.bpas<-PeerReview[PeerReview$ResultType=='BPA',]

    #Combine back
    PeerReview<-rbind(PeerReview.bpas,PeerReview.results)

    #************************************************
    # Same up to here as peer review function
    # Now do the extra stuff for Final Scores
    #************************************************

    #For Final Scores calculated total weighted
    df<-PeerReview[c('ProgDept','ItemMeta1','ProgName','ProgDescription','ScoreDept','ScorePeer','DeptComments','PeerComments','ProgID','ServiceType', 'ResultID','ResultAbbr','ResultType','ScoreID','Weight')]
    #clean this for all result id's not NA's
    df<-df[!is.na(df$ResultID),]

    df[,'ScoreDept']<-df[,'ScoreDept']*df[,'Weight']
    df[,'ScorePeer']<-df[,'ScorePeer']*df[,'Weight']
    #browser()
    Dept<-df[c('ProgID','ResultID','ScoreDept')]
    Peer<-df[c('ProgID','ResultID','ScorePeer')]

    Dept<-dcast(Dept,ProgID ~ ResultID,value.var='ScoreDept')
    Peer<-dcast(Peer,ProgID ~ ResultID,value.var='ScorePeer')

    Dept[,'FinalScore']<-rowSums(Dept[,c(2:ncol(Dept))],na.rm = T)
    Peer[,'FinalScore']<-rowSums(Peer[,c(2:ncol(Peer))],na.rm = T)

    Dept<-Dept[c('ProgID','FinalScore')]
    Peer<-Peer[c('ProgID','FinalScore')]
    #browser()
    #Just get the Prog Info data
    Final_Peer<-PeerReview[!duplicated(PeerReview$ProgID),c('ProgDept','ItemMeta1','ProgNum','ProgName','ProgDescription','DeptComments','PeerComments','ProgID','ServiceType')]
    Final_Peer<-merge(Final_Peer,Peer,by='ProgID')
    Final_Peer<-merge(Final_Peer,ResultTypes[c('ResultType','Q1_Q2','Q2_Q3','Q3_Q4')],by.x='ServiceType',by.y='ResultType')

    Final_Dept<-PeerReview[!duplicated(PeerReview$ProgID),c('ProgDept','ItemMeta1','ProgNum','ProgName','ProgDescription','DeptComments','PeerComments','ProgID','ServiceType')]
    Final_Dept<-merge(Final_Dept,Dept,by='ProgID')
    Final_Dept<-merge(Final_Dept,ResultTypes[c('ResultType','Q1_Q2','Q2_Q3','Q3_Q4')],by.x='ServiceType',by.y='ResultType')

    #*************************************
    # Compute Boundaries - Peer Review
    #*************************************

    #split across service types and calculate the boundaries based on average and stdev
    boundaries<-function(Scores){
      temp<-NULL
      resulttypes<-split(Scores,Scores$ServiceType)
      for(i in 1:length(resulttypes)){
        resulttype<-resulttypes[[i]][1,'ServiceType']

        # Caluculate Max Score and update the scores to out of max score
        score.max<-AllResults[AllResults$ResultType==resulttype,]
        score.max<-rbind(AllResults[AllResults$ResultType=='BPA',],score.max)
        score.max[,'Score']<-4
        score.max[,'Score']<-score.max[,'Score']*score.max[,'Weight']
        score.max<-sum(score.max[,'Score'],na.rm=T)

        rows<-Scores[Scores$ServiceType==resulttype,]
        rows[,'FinalScore']<-rows[,'FinalScore']*100/score.max

        Q2_Q3<-mean(rows[,'FinalScore'],na.rm=T)
        Q1_Q2<-sd(rows[,'FinalScore'],na.rm=T)+Q2_Q3
        Q3_Q4<-Q2_Q3-sd(rows[,'FinalScore'],na.rm=T)


        #if zero then upate to algortithm values, otherwise keep the storered boundaries
        rows[rows$Q1_Q2==0,'Q1_Q2']<-Q1_Q2
        rows[rows$Q2_Q3==0,'Q2_Q3']<-Q2_Q3
        rows[rows$Q3_Q4==0,'Q3_Q4']<-Q3_Q4


        rows[,'Quartile']<-4
        rows[rows$FinalScore>=rows$Q3_Q4,'Quartile']<-3
        rows[rows$FinalScore>=rows$Q2_Q3,'Quartile']<-2
        rows[rows$FinalScore>=rows$Q1_Q2,'Quartile']<-1

        temp<-rbind(temp,rows)
      }
      Scores<-temp
      return(Scores)
    }

    Final_Dept<-boundaries(Final_Dept)
    Final_Peer<-boundaries(Final_Peer)

    Final_Dept[,'Score']<-'Dept'
    Final_Peer[,'Score']<-'Peer'

    Final_Dept<-Final_Dept[order(-Final_Dept$FinalScore),]
    Final_Peer<-Final_Peer[order(-Final_Peer$FinalScore),]

    #Lastly make a column of Quartle difference between department and peer
    Q_Diff<-Final_Peer[c('ProgID','Quartile')]
    colnames(Q_Diff)[2]<-'Peer_Quartile'

    Q_Diff<-merge(Q_Diff,Final_Dept[c('ProgID','Quartile')],by='ProgID')
    colnames(Q_Diff)[3]<-'Dept_Quartile'
    Q_Diff[,'Q_Diff']<-Q_Diff[,'Peer_Quartile']-Q_Diff[,'Dept_Quartile']

    FinalScores<-rbind(Final_Dept,Final_Peer)
    FinalScores<-merge(FinalScores,Q_Diff[c('ProgID','Q_Diff','Peer_Quartile','Dept_Quartile')],by='ProgID')


    #browser()
    data<-list()
    data$FinalScores<-FinalScores
    data$PeerReview<-PeerReview[c('ProgDept','ItemMeta1','ProgName','ProgDescription','ScoreDept','ScorePeer','DeptComments','PeerComments','ProgID','ServiceType', 'ResultID','ResultAbbr','ResultType','ScoreID','Weight')]
    data$ResultTypes<-ResultTypes
    data$AllResults<-AllResults
    return(data)
  }

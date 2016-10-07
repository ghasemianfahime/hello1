#This script is for generating some random teams to compare these teams with EnhancedSteiner and the proposed algorithm
rm(list=ls())

library('igraph')
library('MASS')
library('Matrix')
library('hash')
library('CITAN')
library('RMySQL')

con<-dbConnect(MySQL(), user='root', password='ihkkahdo', dbname='vivo_northwestern', host='127.0.0.1', port=3307)


Retrieve_Teams_from_Database<-function(folder_path, current_year, future_year){
  
  query<-"select researchareafor.resource_ID as team_concept,group_concat(DISTINCT researchareafor.concept_id) as concepts,("
  
  for(i in as.numeric(current_year):(as.numeric(future_year)-1)){
    query<-paste(query,"information_resource.c",as.character(i),"+",sep="")
  }
  
  query<-paste(query,"information_resource.c",future_year,") as citedBy ",sep="")
  query<-paste(query,"from ( researchareafor join information_resource on information_resource.UID=researchareafor.resource_ID) where LEFT(information_resource.date,4)=",current_year," group by researchareafor.resource_ID",sep="")
  
  authorship<-dbSendQuery(con, query)
  authorship<-fetch(authorship,n=-1)
  
  team_concept<-authorship$team_concept
  concepts_ID<-authorship$concepts
  citedBy<-authorship$citedBy
  
  authorship_authors<-read.csv(paste(folder_path,'/Input Information/journal_authorship.csv',sep=""),col.names=c('paper_ID','author_ID'),colClasses=c('character','character'))
  
  team_author<-authorship_authors$paper_ID
  team_author_unique<-team_author[!duplicated(team_author)]
  author<-authorship_authors$author_ID
  
  index_concept<-which(team_concept%in%team_author_unique)
  team_concept<-team_concept[index_concept]
  concepts_ID<-concepts_ID[index_concept]
  concepts_ID<-sapply(concepts_ID,function(x) strsplit(x,','))
  citedBy<-citedBy[index_concept]
  
  threshold<-median(citedBy)
  
  index_author<-which(team_author%in%team_concept)
  team_author<-team_author[index_author]
  author<-author[index_author]
  
  return (list(threshold=threshold,citedBy=citedBy,teams=team_concept,concepts_ID=concepts_ID,author=author,team_author=team_author))
}


#load files
alpha<-0.9
current_year<-c("2000","2001")
future_year<-c("2006","2007")
year_interval<-c("1990_1999","1991_2000")

Folder_path<-"D:/Team_Assembly_NU/Data"

for(i in 1:length(current_year)){
  
  print(current_year[i])
  
  load(paste(Folder_path,'/HyperGraph/H_W_Dv_De_Hyper_Graph_normalized_weighted_year_',year_interval[i],'.RData',sep=""))
  load(paste(Folder_path,'/HyperGraph/Hash_normalized_weighted_year_',year_interval[i],'.RData',sep=""))
  
  fileNameOut1<-paste(Folder_path,"/DreamTeam/Random_",current_year[i],".csv",sep="")
  
  result<-Retrieve_Teams_from_Database(Folder_path, current_year[i], future_year[i])
  teams<-result$teams
  team_author<-result$team_author
  concepts_ID<-result$concepts_ID
  author<-result$author
  citedBy<-result$citedBy
  
  noHyperEdge<-length(teams)
  noauth<-length(keys(author_Hash))
  
  A<-(solve(Dv)^0.5)%*%H%*%(W)%*%(solve(De)^1)%*%t(H)%*%(solve(Dv)^0.5)
  
  diag(A)<-0
  query_dim<-nrow(H)
  
  W_diag<-diag(W)  
  
  for(j in 1:noHyperEdge){  
    
    print(paste("Hypergraph number: ",j))
    
    S_concepts<-unname(unlist(concepts_ID[j]))
    
    S_team<-teams[j]
    
    S_authors<-author[which(team_author%in%S_team)]
    
    S_author_index<-unname(unlist(sapply(S_authors,function(x) author_Hash[[x]])))
    
    if(length(S_author_index)==length(S_authors)){
      
      S_concept_index<-sapply(unname(unlist(concepts_ID[j])),function(x) concept_Hash[[x]])
      
      S_concept_index<-unname(unlist(S_concept_index))
      
      print(S_concept_index)
      
      random_Team<-sample(1:noauth,sample(2:length(S_concept_index)),replace=T)
      
      write.table(t(c(length(S_concept_index),random_Team,citedBy[j])), fileNameOut1, append=TRUE, sep=';', col.names=FALSE, row.names=FALSE)
      
      
    }   
    
  }
  
}
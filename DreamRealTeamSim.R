#This script is for computing the Jaccard similarity of Dream Teams to Real Teams

rm(list=ls())

options(warn=-1)

if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")

library('RWeka')
library('RMySQL')
library('statnet')
library('igraph')
library('MASS')
library('Matrix')
library('hash')
library('CITAN')

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
    
  filename_Esteiner<-paste(Folder_path,"/DreamTeam/Enhanced_Esteiner_",current_year[i],".csv",sep="")
  filename_myAlgorithm1<-paste(Folder_path,"/DreamTeam/my_ALgorithm_",current_year[i],".csv",sep="")
  filename_random<-paste(Folder_path,"/DreamTeam/Random_",current_year[i],".csv",sep="")
  
  data1<-read.csv(filename_Esteiner,header=FALSE)
  data2<-read.csv(filename_myAlgorithm1,header=FALSE)
  data3<-read.csv(filename_random,header=FALSE)
  
  result<-Retrieve_Teams_from_Database(Folder_path, current_year[i], future_year[i])
  teams<-result$teams
  team_author<-result$team_author
  concepts_ID<-result$concepts_ID
  author<-result$author
  
  noHyperEdge<-length(teams)
  noauth<-length(keys(author_Hash))
  
  A<-(solve(Dv)^0.5)%*%H%*%(W)%*%(solve(De)^1)%*%t(H)%*%(solve(Dv)^0.5)
  
  diag(A)<-0
  query_dim<-nrow(H)
  
  W_diag<-diag(W)
  
  g<-graph.adjacency(A[1:noauth,1:noauth],weighted=TRUE)
  
  counter<-0
  
  jac_sim_mayAlgorithm<-rep(0,0)
  jac_sim_Esteiner<-rep(0,0)
  jac_sim_Random<-rep(0,0)
  CC<-rep(0,0)
  
  
  for(j in 1:noHyperEdge){  
    
#       print(paste("Hypergraph number: ",j))
    
    S_concepts<-unname(unlist(concepts_ID[j]))
    
    S_team<-teams[j]
    
    S_authors<-author[which(team_author%in%S_team)]
    
    S_author_index<-unname(unlist(sapply(S_authors,function(x) author_Hash[[x]])))
    
    if(length(S_author_index)==length(S_authors)){
      
      S_concept_index<-sapply(unname(unlist(concepts_ID[j])),function(x) concept_Hash[[x]])
      
      S_concept_index<-unname(unlist(S_concept_index))
      
      
      counter<-counter+1
      
      
      Esteiner_Team<-rep(0,0)
      row1<-data1[counter,]
      Team_size<-length(row1[which(!is.na(row1))])-2
      if(length(row1)>0 & Team_size>0){
        Esteiner_Team<-unlist(row1[2:(Team_size+1)])
        Esteiner_Team<-Esteiner_Team[!duplicated(Esteiner_Team)]  
      }
      
      Myalgorithm1_Team<-rep(0,0)
      row2<-data2[counter,]
      Team_size<-length(row2[which(!is.na(row2))])-2
      if(length(row2)>0 & Team_size>0){
        Myalgorithm1_Team<-unlist(row2[2:(Team_size+1)])
        Myalgorithm1_Team<-Myalgorithm1_Team[!duplicated(Myalgorithm1_Team)]
      }
      
      Random_Team<-rep(0,0)
      row3<-data3[counter,]
      Team_size<-length(row3[which(!is.na(row3))])-2
      if(length(row3)>0 & Team_size>0){
        Random_Team<-unlist(row3[2:(Team_size+1)])
        Random_Team<-Random_Team[!duplicated(Random_Team)]
      }
      
      
      if((length(Esteiner_Team)>0) & (length(S_author_index)>0) & (length(Myalgorithm1_Team)>0) & (length(Random_Team)>0) & Myalgorithm1_Team[1]!=-1 & Esteiner_Team[1]!=-1 & Random_Team[1]!=-1){
        
        #compute Jaccard Similarity
         result_jaccard_Myalgorithm1<-similarity.jaccard(g,c(S_author_index,Myalgorithm1_Team))
         result_jaccard_Esteiner<-similarity.jaccard(g,c(S_author_index,Esteiner_Team))
         result_jaccard_Random<-similarity.jaccard(g,c(S_author_index,Random_Team))
         
        if(length(S_author_index)>1){
          
          if((length(S_author_index)+1)<ncol(result_jaccard_Myalgorithm1))
            result_jaccard_Myalgorithm1<-mean(apply(result_jaccard_Myalgorithm1[1:length(S_author_index),(length(S_author_index)+1):ncol(result_jaccard_Myalgorithm1)],1,max))
          else
            result_jaccard_Myalgorithm1<-mean(result_jaccard_Myalgorithm1[1:length(S_author_index),ncol(result_jaccard_Myalgorithm1)])
          
          if((length(S_author_index)+1)<ncol(result_jaccard_Esteiner))
            result_jaccard_Esteiner<-mean(apply(result_jaccard_Esteiner[1:length(S_author_index),(length(S_author_index)+1):ncol(result_jaccard_Esteiner)],1,max))
          else
            result_jaccard_Esteiner<-mean(result_jaccard_Esteiner[1:length(S_author_index),ncol(result_jaccard_Esteiner)])
          
          if((length(S_author_index)+1)<ncol(result_jaccard_Random))
            result_jaccard_Random<-mean(apply(result_jaccard_Random[1:length(S_author_index),(length(S_author_index)+1):ncol(result_jaccard_Random)],1,max))
          else
            result_jaccard_Random<-mean(result_jaccard_Random[1:length(S_author_index),ncol(result_jaccard_Random)])
          
        }
        
        else{
          
          result_jaccard_Myalgorithm1<-mean(result_jaccard_Myalgorithm1[1:length(S_author_index),(length(S_author_index)+1):ncol(result_jaccard_Myalgorithm1)])
          result_jaccard_Esteiner<-mean(result_jaccard_Esteiner[1:length(S_author_index),(length(S_author_index)+1):ncol(result_jaccard_Esteiner)])
          result_jaccard_Random<-mean(result_jaccard_Random[1:length(S_author_index),(length(S_author_index)+1):ncol(result_jaccard_Random)])
          
        }
        
        
        jac_sim_mayAlgorithm<-c(jac_sim_mayAlgorithm,result_jaccard_Myalgorithm1)
        jac_sim_Esteiner<-c(jac_sim_Esteiner,result_jaccard_Esteiner)
        jac_sim_Random<-c(jac_sim_Random,result_jaccard_Random)
        CC<-c(CC,result$citedBy[j])
        
      } 
    }   
    
    
  }
  
  citation_counts<-CC
  Jaccard_Sim_EnhancedEsteiner<-jac_sim_Esteiner
  Jaccard_Sim_OurAlgorithm<-jac_sim_mayAlgorithm
  Jaccard_Sim_Random<-jac_sim_Random
  
  print(cor.test(Jaccard_Sim_EnhancedEsteiner,citation_counts,method='spearman'))
  print(cor.test(Jaccard_Sim_OurAlgorithm,citation_counts,method='spearman'))
  print(cor.test(Jaccard_Sim_Random,citation_counts,method='spearman'))
  
}
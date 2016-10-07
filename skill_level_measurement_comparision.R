#This script is for comparision of different skill level measurement 
rm(list=ls())
options(warn=-1)


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



alpha<-0.9
current_year<-c("2000","2001","2002","2003","2004","2005")
future_year<-c("2006","2007","2008","2009","2010","2011")
year_interval<-c("1990_1999","1991_2000","1992_2001","1993_2002","1994_2003","1995_2004")

Folder_path<-"D:/Team_Assembly_NU/Data"


for(i in 1:length(current_year)){
  
  print(current_year[i])
  
  load(paste(Folder_path,'/HyperGraph/H_W_Dv_De_Hyper_Graph_normalized_weighted_year_',year_interval[i],'.RData',sep=""))
  load(paste(Folder_path,'/HyperGraph/Hash_normalized_weighted_year_',year_interval[i],'.RData',sep=""))
  
  result<-Retrieve_Teams_from_Database(Folder_path, current_year[i], future_year[i])
  teams<-result$teams
  team_author<-result$team_author
  concepts_ID<-result$concepts_ID
  author<-result$author
  citedBy<-result$citedBy
  
  noHyperEdge<-length(teams)
  noauth<-length(keys(author_Hash))
  
  author_concept<-Matrix(0,nrow(H),nrow(H))
  author_concept<-H%*%t(H)
  weighted_author_concept<-H%*%W%*%t(H)
  A<-(solve(Dv)^0.5)%*%H%*%(W)%*%solve(De)%*%t(H)%*%(solve(Dv)^0.5)
  diag(A)<-0
 
  CB<-rep(0,0)
  
  collaboration_numer<-rep(0,0)
  enhanced_skill<-rep(0,0) 
  weighted_collaboration_number<-rep(0,0)
  
  for(j in 1:noHyperEdge){
    
#      print(j)
    
    S_concepts<-unname(unlist(concepts_ID[j]))
    
    S_team<-teams[j]
    
    S_authors<-author[which(team_author%in%S_team)]
    
    S_author_index<-unname(unlist(sapply(S_authors,function(x) author_Hash[[x]])))
    
    if(length(S_author_index)==length(S_authors)){
      
      S_concept_index<-sapply(unname(unlist(concepts_ID[j])),function(x) concept_Hash[[x]])
      
      S_concept_index<-unname(unlist(S_concept_index))
      
      if(length(S_author_index)>1){
      
        author_experience<-author_concept[S_author_index,S_concept_index]
        
#          ES<-author_experience
#          
#          for(k in 1:length(S_author_index)){
#            
#            for(l in 1:length(S_concept_index)){
#            
#              neighbor<-which(A[S_author_index[k],]>0)
#              neighbor_experience<-sum(author_concept[S_author_index,S_concept_index[l]])
#              ES[k,l]<-ES[k,l]+neighbor_experience  
#              
#            }
#            
#          }
#          
#         enhanced_skill<-c(enhanced_skill, sum(apply(ES, 2, max)))
        
        collaboration_numer<-c(collaboration_numer, sum(apply(author_experience, 2, max)))

        author_experience2<-weighted_author_concept[S_author_index,S_concept_index]
        weighted_collaboration_number<-c(weighted_collaboration_number, sum(apply(author_experience2, 2, max)))
        
      }
      
      else{
        
        author_experience<-author_concept[S_author_index,S_concept_index]
        
#          ES<-author_experience
#            
#          for(l in 1:length(S_concept_index)){
#            
#            neighbor<-which(A[S_author_index,]>0)
#            neighbor_experience<-sum(author_concept[S_author_index,S_concept_index[l]])
#            ES[l]<-ES[l]+neighbor_experience  
#          
#          }
#          
#          enhanced_skill<-c(enhanced_skill, sum(ES))
        
        collaboration_numer<-c(collaboration_numer, sum(author_experience))

        author_experience2<-weighted_author_concept[S_author_index,S_concept_index]
        weighted_collaboration_number<-c(weighted_collaboration_number, sum(author_experience2))
                
      }
      
      CB<-c(CB,citedBy[j])
      
    }
  
  }
  
  print(cor.test(collaboration_numer,CB,method='spearman'))
#   print(cor.test(enhanced_skill,CB,method='spearman'))
  print(cor.test(weighted_collaboration_number,CB,method='spearman'))

}


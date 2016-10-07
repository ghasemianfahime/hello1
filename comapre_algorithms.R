#This script is for executing different Team Assembly Algorithms and Saving their results in Files. 

  rm(list=ls())
  
  con<-dbConnect(MySQL(), user='root', password='ihkkahdo', dbname='vivo_northwestern', host='127.0.0.1', port=3307)

  
  Retrieve_Teams_from_Database<-function(Folder_path, current_year, future_year){
    
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
    
    authorship_authors<-read.csv(paste(Folder_path,'/Input Information/journal_authorship.csv',sep=""),col.names=c('paper_ID','author_ID'),colClasses=c('character','character'))
    
    team_author<-authorship_authors$paper_ID
    team_author_unique<-team_author[!duplicated(team_author)]
    author<-authorship_authors$author_ID
    
    index_concept<-which(team_concept%in%team_author_unique)
    team_concept<-team_concept[index_concept]
    concepts_ID<-concepts_ID[index_concept]
    concepts_ID<-sapply(concepts_ID,function(x) strsplit(x,','))
    citedBy<-citedBy[index_concept]
    
    index_author<-which(team_author%in%team_concept)
    team_author<-team_author[index_author]
    author<-author[index_author]
    
    return (list(citedBy=citedBy,teams=team_concept,concepts_ID=concepts_ID,author=author,team_author=team_author))
  }
  
  
  compare_algorithms<-function(Folder_path, current_year, future_year, year_interval){

    result<-Retrieve_Teams_from_Database(Folder_path, current_year, future_year)
    
    concepts_ID<-result$concepts_ID
    citedBy<-result$citedBy
    noHyperEdge<-length(result$teams)
    author<-result$author
    team_author<-result$team_author
    team_concept<-result$teams

    load(paste(Folder_path,'/Hypergraph/H_W_Dv_De_Hyper_Graph_normalized_weighted_year_',year_interval,'.RData',sep=""))
    load(paste(Folder_path,'/Hypergraph/Hash_normalized_weighted_year_',year_interval,'.RData',sep=""))
  
    noauth<-length(keys(author_Hash))
    
    fileNameOut1<-paste(Folder_path,'/DreamTeam/Enhanced_Esteiner10_',current_year,'.csv',sep="")
    fileNameOut2<-paste(Folder_path,'/DreamTeam/my_ALgorithm10_',current_year,'.csv',sep="")
   
    A<-(solve(Dv)^0.5)%*%H%*%(W)%*%solve(De)%*%t(H)%*%(solve(Dv)^0.5)
    diag(A)<-0
 
    collaboration_network<-H%*%W%*%t(H)
    diag(collaboration_network)<-0
    collaboration_network<-collaboration_network/max(collaboration_network)
    g<-graph.adjacency(collaboration_network[1:noauth,1:noauth], weighted=TRUE)
    E(g)$weight<-1/E(g)$weight
  
    for(i in 1:noHyperEdge){  
      
      S_concepts<-unname(unlist(concepts_ID[i]))
      
      S_team<-team_concept[i]
      
      S_authors<-author[which(team_author%in%S_team)]
      
      S_author_index<-unname(unlist(sapply(S_authors,function(x) author_Hash[[x]])))
      
      if(length(S_author_index)==length(S_authors)){
        
        S_concept_index<-sapply(unname(unlist(concepts_ID[i])),function(x) concept_Hash[[x]])
        
        S_concept_index<-unname(unlist(S_concept_index))
             
    #****************************** EnhancedSteiner Algorithm **************************************     
    
        if(length(S_concept_index>0)){
          
          print(paste("Hypergraph number: ",i))
        
          Dream_Team<-Enhanced_Steiner(S_concept_index,collaboration_network,noauth,g)
        
          write.table(t(c(length(S_concept_index),Dream_Team$solution,citedBy[i])), fileNameOut1, append=TRUE, sep=';', col.names=FALSE, row.names=FALSE)
          
          print("&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&")
      
          print(Dream_Team$solution)
      
      
    #***********************************My Algorithm*********************************************   
    
          Dream_Team<-MyAlgorithm(required_skills=S_concept_index, adjacency_matrix1=A, adjacency_matrix2=A[1:noauth,1:noauth], noScholar=noauth, Ts=0.3, neighbor_radius=6)
    
          print("&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&")
      
          print(Dream_Team)
      
          write.table(t(c(length(S_concept_index),Dream_Team,citedBy[i])), fileNameOut2, append=TRUE, sep=';', col.names=FALSE, row.names=FALSE)
    
    
       }
  
      }
    
  }

}


#Executing the function
current_year<-c("2000","2001")
future_year<-c("2006","2007")
  
year_interval<-c("1990_1999","1991_2000")

Features<-data.frame(Hyper_dependent_score_max=rep(0,0),Hyper_dependent_score_avg=rep(0,0),Hyper_independent_score_max=rep(0,0),Hyper_independent_score_avg=rep(0,0),max_hindex=rep(0,0),mean_hindex=rep(0,0),max_gindex=rep(0,0),mean_gindex=rep(0,0),similarity=rep(0,0),mean_degree=rep(0,0),max_degree=rep(0,0),expertise_level=rep(0,0),Fuzz_success_max=rep(0,0),Fuzz_unsuccess_max=rep(0,0),Fuzz_success_avg=rep(0,0),Fuzz_unsuccess_avg=rep(0,0),avg_citation=rep(0,0),max_citation=rep(0,0),class=rep(0,0))
Folder_path<-"D:/Team_Assembly_NU/Data"

  
source("D:/Team_Assembly_NU/EnhancedEsteiner.R")
source("D:/Team_Assembly_NU/my_algorithm.R")
  
for(i in 1:length(current_year)){
  
  compare_algorithms(Folder_path, current_year[i], future_year[i], year_interval[i])
  
}


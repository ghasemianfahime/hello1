#This script is for modeling the information about authors and MeSH concepts 
#using a Hyper-graph and Graph.
#in below paper is used for writting this script:
#Shulong Tan, Jiajun Bu, Chun Chen, Bin Xu, Can Wang and Xiaofei He,"Using Rich Social Media Information for Music Recommendation via Hypergraph Model", ACM Transactions on Multimedia Computing, Communications and Applications, Vol. 2, No. 3, Article 1, Publication date: May 2010. .


#Author: Fahimeh Ghasemian

library('RMySQL')
library('statnet')
library('MASS')
library('Matrix')
library('hash')

convertToHyperGraph<-function(Folder_path, year_start, year_end, current_year, future_year, teta){
  
  #connect to the database
  con<-dbConnect(MySQL(), user='root', password='ihkkahdo', dbname='vivo_northwestern', host='127.0.0.1', port=3307)
  
  #Fetch information about concepts and authors
  query<-paste("select information_resource.UID, researchareafor.concept_id from (information_resource join researchareafor on information_resource.UID=researchareafor.resource_ID) where LEFT(information_resource.date,4)>=",year_start," and LEFT(information_resource.date,4)<=",year_end,sep="")
  
  authorship_concepts<-dbSendQuery(con,query)
  
  authorship_concepts<-fetch(authorship_concepts,n=-1)
  
  team_concept<-authorship_concepts$UID
  concept<-authorship_concepts$concept
  
  authorship_authors<-read.csv(paste(Folder_path,'\\Input Information\\journal_authorship.csv',sep=""),col.names=c('paper_ID','author_ID'),colClasses=c('character','character'))
  
  team_author<-authorship_authors$paper_ID
  team_author_unique<-team_author[!duplicated(team_author)]
  author<-authorship_authors$author_ID
  
  index_concept<-which(team_concept%in%team_author_unique)
  team_concept<-team_concept[index_concept]
  team_concept_unique<-team_concept[!duplicated(team_concept)]
  concept<-concept[index_concept]
  concept_unique<-concept[!duplicated(concept)]
  
  index_author<-which(team_author%in%team_concept_unique)
  team_author<-team_author[index_author]
  team_author_unique<-team_author[!duplicated(team_author)]
  author<-author[index_author]
  author_unique<-author[!duplicated(author)]
  
  author_Hash<-hash(c(author_unique),1:length(author_unique))
  concept_Hash<-hash(c(concept_unique),(length(author_unique)+1):(length(author_unique)+length(concept_unique)))
  team_total<-team_author_unique
  team_Hash<-hash(c(team_total),1:length(team_total))  
  
  #******************************Construct Hyper-Graph**********************************   
  H<-Matrix(0,length(author_unique)+length(concept_unique),length(team_total),sparse=TRUE);
  # dimnames(H)<-list(c(author_ID_unique,concept_unique),c(team_ID_concept_unique))
  
  I1<-unname(values(concept_Hash,c(concept)))
  J1<-unname(values(team_Hash,c(team_concept)))
  
  I2<-unname(values(author_Hash,c(author)))
  J2<-unname(values(team_Hash,c(team_author)))
  
  n<-length(team_total)
  for(j in 1:n){
    concept_index<-which(J1==j)
    H[I1[concept_index],j]<-1
    author_index<-which(J2==j)
    H[I2[author_index],j]<-1
    print(j)
  }
  
  noHyperEdge<-ncol(H)
  noNode<-nrow(H)
  
  #compute De, Dv, W for formula: P=(Dv)^-0.5 H W (De)^-1 (H)^t (Dv)^-0.5
  De<-Diagonal(noHyperEdge)
  diag(De)<-colSums(H)
  
  Dv<-Diagonal(noNode,0)
  diag(Dv)<-rowSums(H)
  
  query<-"select information_resource.UID,LEFT(information_resource.date,4) as date,("
  for(i in as.numeric(year_start):(as.numeric(current_year)-1)){
    query<-paste(query,"information_resource.c",as.character(i),"+",sep="")
  }
  query<-paste(query,"information_resource.c",current_year,") as citedBy ",sep="")
  query<-paste(query,"from (information_resource join researchareafor on information_resource.UID=researchareafor.resource_ID) where LEFT(information_resource.date,4)>=",year_start,"  and LEFT(information_resource.date,4)<=",year_end," group by information_resource.UID",sep="")
  
  authorship_citedBy<-dbSendQuery(con, query)
  
  authorship_citedBy<-fetch(authorship_citedBy,n=-1)
  team_citedBy<-authorship_citedBy$UID
  index_citedBy<-which(team_citedBy%in%team_author_unique)
  team_citedBy<-team_citedBy[index_citedBy]
  citedBy<-authorship_citedBy$citedBy[index_citedBy]
  date<-authorship_citedBy$date[index_citedBy]
  
  team_citedBy_index<-unname(values(team_Hash,c(team_citedBy)))
  result<-sort(team_citedBy_index, index.return=TRUE)
  citedBy<-citedBy[result$ix]
  date<-date[result$ix]  
  
  query<-"select LEFT(information_resource.date,4) as date, max("
  for(i in as.numeric(year_start):(as.numeric(current_year)-1)){
    query<-paste(query,"information_resource.c",as.character(i),"+",sep="")
  }
  query<-paste(query,"information_resource.c",current_year,") as maximum ",sep="")
  query<-paste(query,"from (information_resource join researchareafor on information_resource.UID=researchareafor.resource_ID) where LEFT(information_resource.date,4)>=",year_start,"  and LEFT(information_resource.date,4)<=",year_end," group by LEFT(information_resource.date,4)",sep="")
  
  max_citedBy<-dbSendQuery(con, query)
  max_citedBy<-fetch(max_citedBy,n=-1)
  maximum_citedBy<-max_citedBy$maximum
  date2<-max_citedBy$date
  
  normalization_value<-rep(0,length(citedBy))
  year_coefficient<-rep(0,length(citedBy))
  
  for(i in 1:length(citedBy)){
    normalization_value[i]<-maximum_citedBy[which(date[i]==date2)]  
    year_coefficient[i]<-((1-teta)^(as.numeric(current_year)-as.numeric(date[i]))*teta)  
    print(paste("year_coefficient: ",year_coefficient[i],sep=""))
  }
  
  W<-Diagonal(noHyperEdge)
  
  diag(W)<-(citedBy/normalization_value)*year_coefficient  
  filename<-paste(Folder_path,"\\Hypergraph\\H_W_Dv_De_Hyper_Graph_normalized_weighted_year_",year_start,"_",year_end,".RData",sep="")
  save(H,W,Dv,De,file=filename)
  filename<-paste(Folder_path,"\\Hypergraph\\Hash_normalized_weighted_year_",year_start,"_",year_end,".RData",sep="")
  save(author_Hash,concept_Hash,team_Hash,file=filename)   
  
  diag(W)<-citedBy  
  filename<-paste(Folder_path,"\\Hypergraph\\H_W_Dv_De_Hyper_Graph_",year_start,"_",year_end,".RData",sep="")
  save(H,W,Dv,De,file=filename)
  filename<-paste(Folder_path,"\\Hypergraph\\Hash_",year_start,"_",year_end,".RData",sep="")
  save(author_Hash,concept_Hash,team_Hash,file=filename)   
  
}


year_start<-c("1990","1991","1992","1993","1994","1995")
year_end<-c("1999","2000","2001","2002","2003","2004")
current_year<-c("2000","2001","2002","2003","2004","2005")
future_year<-c("2006","2007","2008","2009","2010","2011")

Folder_path<-"D:\\Team_Assembly_NU\\Data"

teta<-0.6

for(i in 1:length(current_year)){
  
  print(current_year[i])
  convertToHyperGraph(Folder_path, year_start[i], year_end[i], current_year[i], future_year[i], teta)

}

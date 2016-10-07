#This script is for adding PubMed information (MeSH concepts) to articles in MySQL database.
#Therefore we had to limit our articles to ones in PubMed Database

#Author: Fahimeh Ghasemian 

rm(list=ls())

library('rpubmed')
library('RMySQL')

#connect to the database
con<-dbConnect(MySQL(), user='root', password='ihkkahdo', dbname='vivo_northwestern', host='127.0.0.1', port=3307)

#extract articles which have a PMID (limit articles to PubMed articles)
result<-dbSendQuery(con, "SELECT UID , PMID from informationresource where PMID>0")
result<-fetch(result,n=-1)
UID<-result$UID
PMID<-result$PMID

for(i in 1:length(PMID)){
  
  print(PMID[i])
  
  #extract MeSH of articles
  records <- fetch_in_chunks(PMID[i])
  
  if(!is.null(records$PubmedArticle$MedlineCitation$MeshHeadingList$MeshHeading)){
    # list of MeSH headings with frequencies:
    diab_assoc <- mesh_assoc_table(records)
    d <- diag(diab_assoc)
    
    Mesh_terms<-names(d)
    
    Mesh_terms<-sapply(Mesh_terms,function(x) gsub('\'','',x))
    
    Mesh_terms<-unname(Mesh_terms)
      
    #Add it to database
    for(j in 1:length(Mesh_terms)){
      
      query=paste("SELECT ID from mesh where concept='",Mesh_terms[j],"'",sep="");
      #print(query)
      result<-dbSendQuery(con, query)
      result<-fetch(result,n=-1)
      
      k<-1;
      while(k<=length(result$ID))
      {
       #   print(result)
          x<-data.frame(concept_id=result$ID[k],resource_id=UID[i])
          dbWriteTable(con, "researchareafor",x, row.names=0 , append = T); 
          k<-k+1
      }
    }
  }
}

#This script is for adding citation counts broken by year of the articles to the database. We have used Scopus API to extract this information.
#*************************************************************************

library('RMySQL')
library('RCurl')
library('XML')
library('httr')

con<-dbConnect(MySQL(), user='root', password='ihkkahdo', dbname='vivo_northwestern', host='127.0.0.1', port=3307)

papers<-dbSendQuery(con, "select PMID from informationresource where informationresource.PMID>0")

papers<-fetch(papers,n=-1)

PMID<-papers$PMID

n<-length(PMID)

cc<-matrix(0,n,25)
index<-rep(0,0)

#Please replace your API key in the query for accessing Scopus
for(i in 1:n){
  
    query<-paste("http://api.elsevier.com/content/abstract/citations?","pubmed_id=",PMID[i],"&date=1990-2014&apiKey=6492f9c867ddf3e84baa10b5971e3e3d",sep="");
    
    r1 <- GET(query)
    
    r<-content(r1)
    
    if(r1$status_code==200){
      
      print(i)
            
      temp_cc<-rep(0,25)
      
      for(j in 1:25){
        temp_cc[j]<- as.numeric(r$`abstract-citations-response`$citeInfoMatrix$citeInfoMatrixXML$citationMatrix$citeInfo[[1]]$cc[[j]]$'$' )
      }
      
      cc[i,]<-temp_cc
      index<-c(index,i)
  }
}

cc<-cc[index,]

papers<-dbSendQuery(con, "select PMID,DOI,UID,citedBy,date,venue,title,type,abstract from informationresource where PMID>0")

papers<-fetch(papers,n=-1)
papers<-papers[index,]

citation_Info<-data.frame(PMID=papers$PMID,DOI=papers$DOI,UID=papers$UID,citedBy=papers$citedBy,date=papers$date,venue=papers$venue,
                          title=papers$title,type=papers$type,abstract=papers$abstract,c1990=cc[,1],c1991=cc[,2],c1992=cc[,3],c1993=cc[,4],c1994=cc[,5],c1995=cc[,6],c1996=cc[,7],c1997=cc[,8]
                          ,c1998=cc[,9],c1999=cc[,10],c2000=cc[,11],c2001=cc[,12],c2002=cc[,13],c2003=cc[,14],c2004=cc[,15]
                          ,c2005=cc[,16],c2006=cc[,17],c2007=cc[,18],c2008=cc[,19],c2009=cc[,20],c2010=cc[,21],c2011=cc[,22],
                          c2012=cc[,23],c2013=cc[,24],c2014=cc[,25])

dbWriteTable(con, "information_resource3", citation_Info, row.names=0,append = T );
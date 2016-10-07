# This script is for automatically producing Scopus Query for extraction of information about citation counts broken by year

library(RMySQL)

con<-dbConnect(MySQL(), user='root', password='ihkkahdo', dbname='vivo_northwestern', host='127.0.0.1', port=3307)

papers<-dbSendQuery(con, "select informationresource.PMID
from informationresource
where informationresource.PMID>0 and LEFT(informationresource.date,4)>2006 and LEFT(informationresource.date,4)<2015")

papers<-fetch(papers,n=-1)

PMIDs<-papers$PMID

Query<-paste("PMID(",PMIDs[25001],")")

# for(i in 2:length(PMIDs)){
for(i in 1:length(PMIDs)){
  Query<-paste(Query,"OR","PMID(",PMIDs[i],")")
}

fileConn<-file("D:/Team_Assembly_NU/Data/Input Information/Scopus/PMID_Query7000.txt")
writeLines(Query, fileConn)
close(fileConn)

result<-dbSendQuery(con, "select scopus_information2.Y0,scopus_information2.Y1,scopus_information2.Y2,scopus_information2.Y3,scopus_information2.Y4,scopus_information2.Y5,scopus_information2.Y6,scopus_information2.Y7,scopus_information2.Y8,scopus_information2.Y9,scopus_information2.Y10,scopus_information2.Y11,scopus_information2.Y12,scopus_information2.Y13,scopus_information2.Y14,scopus_information2.Y15, informationresource.PMID,informationresource.UID, informationresource.date
from informationresource join scopus_information2 on informationresource.title=scopus_information2.DocumentTitle
where informationresource.PMID>0")

result<-fetch(result,n=-1)

dbWriteTable(con, "citation_information2", result, row.names=0,append=T);


#This script if for reading information about Northwestern scholars and 
#saving them in mySql database (vivo_northwestern), also in this script MeSH tree
#which contains MeSH concepts is transfer to a table named MeSH.

#author: Fahimeh Ghasemian

#establish a MySQL connection
library(RMySQL)

Folder_path<-"D:/Team_Assembly_NU/Data"

con<-dbConnect(MySQL(), user='root', password='ihkkahdo', dbname='vivo_northwestern_v3', host='127.0.0.1', port=3307)

#read MeSH2014 Tree.
Mesh_tree<-read.csv(paste(Folder_path,'/Input Information/mtrees2014.csv',sep=""), col.names=c('concept', 'ID'), colClasses=c('character','character'));

#Insert it in MeSH Table
dbWriteTable(con, "MeSH", Mesh_tree, row.names=0,append = T);

#read Northwestern Data about scholars
scholars1<-read.csv(paste(Folder_path,'/Input Information/author_list.csv',sep=""),col.names=c('UID','name','family','org_name','dep_name'),colClasses=c('character','character','character','character','character'),header=TRUE)
nonduplicatedEntry<-!(duplicated(scholars1$UID))
scholars1<-data.frame(UID=scholars1$UID[nonduplicatedEntry],name=scholars1$name[nonduplicatedEntry],family=scholars1$family[nonduplicatedEntry],org_name=scholars1$org_name[nonduplicatedEntry],dep_name=scholars1$dep_name[nonduplicatedEntry])

#Insert it in author Table
dbWriteTable(con, "author", scholars1, row.names=0,append = T);

#read Northwestern Data about articles
articles<-read.csv(paste(Folder_path,'/Input Information/journal_article_list.csv',sep=""),col.names=c('PMID','DOI','UID','citedBy','date','venue','title'),colClasses=c('character','character','character','character','character','character','character'),header=TRUE)
articles$type<-rep('AcademicArticle', length(articles$PMID))
articles$abstract<-rep('NULL',length(articles$PMID))

#Insert it in informationresource Table
dbWriteTable(con, "informationresource", articles, row.names=0,append = T );

#read Northwestern Data about Authorship
authorship<-read.csv(paste(Folder_path,'/Input Information/journal_authorship.csv',sep=""),col.names=c('paper_ID','author_ID'),colClasses=c('character','character'),header=TRUE)

#Insert it in authorship Table
# for(i in 1:length(authorship$paper_ID)){
#   print(i)
  authorship<-data.frame(paper_ID=authorship$paper_ID,author_ID=authorship$author_ID);
  flag<-dbWriteTable(con, "authorship",authorship, row.names=0 , append = T); 
# }

#read information about journals
journals<-read.csv(paste(Folder_path,'/Input Information/Journal_Information.csv',sep=""),col.names=c('ScopusSourceID','SourceTitle','1999SNIP','1999IPP','1999SJR','2000SNIP','2000IPP','2000SJR','2001SNIP','2001IPP','2001SJR','2002SNIP','2002IPP','2002SJR','2003SNIP','2003IPP','2003SJR','2004SNIP','2004IPP','2004SJR','2005SNIP','2005IPP','2005SJR','2006SNIP','2006IPP','2006SJR','2007SNIP','2007IPP','2007SJR','2008SNIP','2008IPP','2008SJR','2009SNIP','2009IPP','2009SJR','2010SNIP','2010IPP','2010SJR','2011SNIP','2011IPP','2011SJR','2012SNIP','2012IPP','2012SJR','2013SNIP','2013IPP','2013SJR'
))

#Insert it in author Table
dbWriteTable(con, "journal_information", journals, row.names=0,append = T);

result<-dbSendQuery(con, "select researchareafor.concept_id, researchareafor.resource_ID, mesh.concept
from (mesh join researchareafor on researchareafor.concept_id=mesh.ID)") 

result<-fetch(result,n=-1)

dbWriteTable(con, "researchareafor", result$concept, row.names=0,append = T);

dbDisconnect(con);
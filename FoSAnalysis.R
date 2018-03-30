################## A function I use regularly
manage.packages = function(packagelist){
  
  for (l in 1:length(packagelist)){
    if (packagelist[[l]] %in% installed.packages() & !(packagelist[[l]] %in% (.packages()))){
      library(packagelist[[l]],character.only = TRUE)
    } else if (!(packagelist[[l]]) %in% installed.packages()) {
      install.packages(packagelist[[l]])
      library(packagelist[[l]],character.only = TRUE)
    }
  }
  
}


devtools::install_github("nicolewhite/RNeo4j")
library(RNeo4j)
library(data.table)

## Used extensively
pword = readline(prompt="Enter neo4j password: ")
grph=startGraph("http://neo4j-genbank.syr.edu:7474/db/data/", username="neo4j",password=pword)
####################### FUNCTIONS


## Extract papers for child FoS
## Function will take an FoSID, search for papers, author papers, and authors for the FoS and sub-FoS
## Next iteration will search for papers written by the authors in the group

### Take a FoS ID, pull papers and authors, write that data to file, analyze structure, delete object

retrieve.sub.fos.papers = function(fosid,fosname){
  manage.packages(c('igraph','data.table','Matrix'))
  
  results = data.table(PaperID="",Year=0)
  results = results[0]
  
  ## We assume a Level 1 FoS
  q = paste("match (p:Paper)-[:about]->(f:FoS) where f.Level = 1 and f.FOSID='",fosid, "' return p.PaperID as PaperID,p.Year as Year", sep="")
  results = rbind(results,as.data.table(cypher(grph,q)))
  
  ## May be faster to hit the database 1000x
  
  for (l in 2:3){
    q = paste("match (f:FoS)-[:parent_of*]->(f2:FoS) where f.FOSID = '",fosid,"' and f2.Level = ",l," return f2.FOSID", sep="")
    foslist = as.list(unlist(cypher(grph,q)))
    
    for (li in 1:length(foslist)){
      q = paste("match (p:Paper)-[:about]->(f:FoS) where f.FOSID ='",foslist[[li]],"'return p.PaperID as PaperID, p.Year as Year", sep = "")
      results = rbind(results,as.data.table(cypher(grph,q)))
    }
    
  }
  
  results = unique(results)
  fname = paste("./Data/",fosname,"_papers.Rda",sep="")
  
  try(
    write(results,file = fname)
  )
  
  return (results)
}
retrieve.sub.fos.authors = function(fosid,fosname){
  
  manage.packages(c('igraph','data.table','Matrix'))
  
  results = data.table(AuthorID="",PaperID="",Gender="")
  results = results[0]
  
  ## We assume a Level 1 FoS
  q = paste("match (a:Author)-[:wrote]->(p:Paper)-[:about]->(f:FoS) where f.Level = 1 and f.FOSID='",fosid, "' return p.PaperID as PaperID,a.AuthorID as AuthorID, a.Gender as Gender", sep="")
  results = rbind(results,as.data.table(cypher(grph,q)))
  
  ## May be faster to hit the database 1000x
  
  for (l in 2:3){
    q = paste("match (f:FoS)-[:parent_of*]->(f2:FoS) where f.FOSID = '",fosid,"' and f2.Level = ",l," return f2.FOSID", sep="")
    foslist = as.list(unlist(cypher(grph,q)))
    
    for (li in 1:length(foslist)){
      q = paste("match (a:Author)-[:wrote]->(p:Paper)-[:about]->(f:FoS) where f.FOSID ='",foslist[[li]],"' return a.AuthorID as AuthorID, p.PaperID as PaperID, a.Gender as Gender", sep = "")
      results = rbind(results,as.data.table(cypher(grph,q)))
    }
    
  }
  
  results = unique(results)
  fname = paste("./Data/",fosname,"_author_papers.Rda",sep="")
  try(write(results,file=fname))
  
  return (results)
}
################### Analyze the FoS

### FoS Analysis
analyze.fos = function(fosid,fosname,papers,authpaper){
  ## just enter the name and id of the FoS you want to analyze
  ## papers should be a data.table with the following columns: PaperID, Year
  ## authpapers should be a data.table with the following columns: AuthorID, PaperID, Year
  
  ## dependent on manage.packages, igraph, data.table, Matrix, giant.component
  manage.packages(c('igraph','data.table','Matrix'))
  
  
  results = data.table(FoSID=fosid,FoSName=fosname,PubsPerAuthor=0,MeanDegree=0,GiantComp=0,ClusteringCoefficient= 0, 
                       NumPapers = 0,NumAuthors = 0,PctFemale = 0,PctMale = 0,PctUnk = 0)
  
  authors = unique(authpaper[,.(AuthorID,Gender)])
  
  ### Take data on papers and turn it into a matrix
  FoSBPMatrix = spMatrix(nrow=length(unique(authpaper$AuthorID)), 
                         ncol=length(unique(authpaper$PaperID)),
                         i = as.numeric(factor(authpaper$AuthorID)),
                         j = as.numeric(factor(authpaper$PaperID)),
                         x = rep(1,nrow(authpaper))
  )
  
  rownames(FoSBPMatrix) = levels(factor(authpaper$AuthorID))
  colnames(FoSBPMatrix) = levels(factor(authpaper$PaperID))
  
  FoSMat = tcrossprod(FoSBPMatrix)
  
  FoSGraph = graph.adjacency(FoSMat,mode = "undirected",weighted = TRUE, add.colnames = TRUE,add.rownames = TRUE)
  
  ## Transitivity
  results$ClusteringCoefficient = transitivity(FoSGraph,type="global")
  
  ## Giant Component
  gicom = giant.component(FoSGraph)
  results$GiantComp = length(V(gicom))/length((V(FoSGraph)))
  
  results$NumPapers = length(unique(papers$PaperID))
  results$NumAuthors = length(unique(authpaper$AuthorID))
  results$PctFemale = nrow(authors[Gender =='f'])/nrow(authors)
  results$PctMale = nrow(authors[Gender == 'm'])/nrow(authors)
  results$PctUnk = nrow(authors[Gender == 'u'])/nrow(authors)
  results$MeanDegree = mean(degree(FoSGraph))
  
  ### Pubs Per Author
  ppa = authpaper
  ppa$count = 1
  ppa[,pubcount := sum(count),by = AuthorID]
  results$PubsPerAuthor = mean(ppa$pubcount)
  
  ## Precautionary cleaning
  rm(FoSBPMatrix,FoSMat,ppa)
  gc()
  
  return(results)
}



### Enter the FoS ID's you want to analyze
FoSIDList = c("04371FDC","043F0954") #FoSID's
FoSNameList = c("Computational biology","Stereo chemistry")#same order as Id's

FoSResults = data.table(FoSID="",FoSName="",PubsPerAuthor=0,MeanDegree=0,GiantComp=0,ClusteringCoefficient= 0, 
                          NumPapers = 0,NumAuthors = 0,PctFemale = 0,PctMale = 0,PctUnk = 0)
FoSResults = FoSResults[0]



for(li in 1:length(FoSIDList)){
  papers= retrieve.sub.fos.papers(FoSIDList[[li]],FoSNameList[[li]])
  authorpapers= retrieve.sub.fos.authors(FoSIDList[[li]],FoSNameList[[li]])
  
  FoSResults = rbind(FoSResults,analyze.fos(FoSIDList[[li]],FoSNameList[[li]],papers,authorpapers))
  print(li)
}


Aa = analyze.fos("00137C13","Astronomy",AstroAuthors,AstroPapers,AstroAuthorPapers)

#match (p:Paper)-[:about]->(f:FoS)-[:parent_of]->(f2:FoS) where f.FoSName = 'Astronomy' and f2.Level = 2 return count(p)
#$match (p:Paper)-[:about]->(f:FoS)-[:parent_of]->(f2:FoS) where f.FoSName = 'Astronomy' and (f2.Level = 2 or f2.Level = 3) return count(p)



ab= retrieve.sub.fos.papers("00137C13")
ac= retrieve.sub.fos.authors("00137C13")
Ad = analyze.fos("00137C13","Astronomy",unique(ac[,.(AuthorID,Gender)]),ab,ac[,.(AuthorID,PaperID)])

#q = paste("match(p:Paper)-[:about]->(f:FoS) where f.Level = ", foslevel , " and f.FoSName='",fosname,"'  return p.PaperID as PaperID,p.Year as Year,p.Title as Title",setp="")
#papers = as.data.table(cypher(grph,q))

q = paste("match(a:Author)-[:wrote]->(p:Paper)-[:about]->(f:FoS) where f.Level = ", foslevel , " and f.FoSName='",fosname,"' return a.AuthorID as AuthorID, p.PaperID as PaperID ,p.Year as Year",setp="")
authpaper = as.data.table(cypher(grph,q))

q = paste("match(a:Author)-[:wrote]->(p:Paper)-[:about]->(f:FoS) where f.Level = ", foslevel , " and f.FoSName='",fosname,"'  return a.AuthorID as AuthorID, 
            a.FirstName as FirstName, a.LastName as LastName, a.Gender as Gender",setp="")
authors = as.data.table(cypher(grph,q))

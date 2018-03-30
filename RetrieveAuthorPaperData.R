######### Get author paper data
retrieve.sub.fos.authors = function(fosid,startyear){
  
  manage.packages(c('igraph','data.table','Matrix'))
  
  results = data.table(PaperID="", AuthorID="",FirstName="",LastName="",Year = "",Gender="")
  results = results[0]
  
  ## We assume a Level 1 FoS
  q = paste("match (a:Author)-[:wrote]->(p:Paper)-[:about]->(f:FoS) where f.Level = 1 and f.FOSID='",fosid, "' and p.Year >= ", startyear," return p.PaperID as PaperID,a.AuthorID as AuthorID, a.FirstName as FirstName, a. LastName as LastName,p.Year as Year, a.Gender as Gender", sep="")
  results = rbind(results,as.data.table(cypher(grph,q)))
  
  q = paste("match (f:FoS) where f.FOSID = '",fosid,"' return f.FoSName",sep="")
  fosname = cypher(grph,q)
  ## May be faster to hit the database 1000x
  
  for (l in 2:3){
    q = paste("match (f:FoS)-[:parent_of*]->(f2:FoS) where f.FOSID = '",fosid,"' and f2.Level = ",l," return f2.FOSID", sep="")
    foslist = as.list(unlist(cypher(grph,q)))
    
    if (length(foslist > 0)){
      for (li in 1:length(foslist)){
        q = paste("match (a:Author)-[:wrote]->(p:Paper)-[:about]->(f:FoS) where f.FOSID ='",foslist[[li]],"' and p.Year >= ", startyear," return a.AuthorID as AuthorID,a.FirstName as FirstName, a. LastName as LastName, p.PaperID as PaperID, p.Year as Year, a.Gender as Gender", sep = "")
        results = rbind(results,as.data.table(cypher(grph,q)))
      }  
    }
    
    
  }
  
  results = unique(results)
  fname = paste("./Data/",fosid,"_author_papers.Rda",sep="")
  try(write.table(results,file=fname))
  
  results$Year = as.integer(results$Year)
  return (results)
}


#################################### Enter the FoS ID's you want to analyze##############################################
options(digits = 3)
manage.packages(c('igraph','data.table','Matrix'))



####################### FUNCTIONS


## Extract papers for child FoS
## Function will take an FoSID, search for papers, author papers, and authors for the FoS and sub-FoS
## Next iteration will search for papers written by the authors in the group

### Take a FoS ID, pull papers and authors, write that data to file, analyze structure, delete object



################### Analyze the FoS

### FoS Analysis
analyze.fos = function(fosid,authpaper,windowsize){
  ## just enter the name and id of the FoS you want to analyze
  ## papers should be a data.table with the following columns: PaperID, Year
  ## authpapers should be a data.table with the following columns: AuthorID, PaperID, Year
  

  # Get FoSName
  q = paste("match (f:FoS) where f.FOSID = '",fosid,"' return f.FoSName",sep="")
  fosname = as.list(cypher(grph,q))
  
  results = data.table(FoSID=fosid,FoSName=fosname,StartYear = 0,EndYear = 0,PubsPerAuthor=0,MeanDegree=0,GiantComp=0,ClusteringCoefficient= 0, 
                       NumPapers = 0,NumAuthors = 0,PctFemale = 0,PctMale = 0,PctUnk = 0)
  
  results = results[0]
  
  for (yr in seq(from=min(authpaper$Year),to=max(authpaper$Year),by=windowsize/2)){
    endyr = yr+windowsize
    
    ap = authpaper[Year <= endyr]
    authors = unique(ap[,.(AuthorID,Gender)])
    
    ## Bipartite matrix with rows as authors and columns as papers
    FoSBPMatrix = spMatrix(nrow=length(unique(ap$AuthorID)), 
                           ncol=length(unique(ap$PaperID)),
                           i = as.numeric(factor(ap$AuthorID)),
                           j = as.numeric(factor(ap$PaperID)),
                           x = rep(1,nrow(ap))
    )
  

    ## row & column names are the ID's of the authors and papers, respectively
    rownames(FoSBPMatrix) = levels(factor(ap$AuthorID))
    colnames(FoSBPMatrix) = levels(factor(ap$PaperID)) 
  
    ## Turn bipartite matrix into unimodal matrix use tcrossprod
    FoSMat = tcrossprod(FoSBPMatrix)
    
    ## create a graph
    FoSGraph = graph.adjacency(FoSMat,mode = "undirected",weighted = TRUE, add.colnames = TRUE)
    
    
    temp = data.table(FoSID=fosid,FoSName=fosname,StartYear = min(authpaper$Year),EndYear = endyr,PubsPerAuthor=0,MeanDegree=0,GiantComp=0,ClusteringCoefficient= 0, 
                      NumPapers = 0,NumAuthors = 0,PctFemale = 0,PctMale = 0,PctUnk = 0)
    
    ## Transitivity
    temp$ClusteringCoefficient = transitivity(FoSGraph,type="global")
    
    ## Giant Component
    gicom = giant.component(FoSGraph)
    temp$GiantComp = length(V(gicom))/length((V(FoSGraph)))
    
    temp$NumPapers = length(unique(ap$PaperID))
    temp$NumAuthors = length(unique(ap$AuthorID))
    temp$PctFemale = nrow(authors[Gender =='f'])/nrow(authors)
    temp$PctMale = nrow(authors[Gender == 'm'])/nrow(authors)
    temp$PctUnk = nrow(authors[Gender == 'u'])/nrow(authors)
    temp$MeanDegree = mean(degree(FoSGraph))
    
    ### Pubs Per Author
    ppa = ap
    ppa$count = 1
    ppa[,pubcount := sum(count),by = AuthorID]
    temp$PubsPerAuthor = mean(ppa$pubcount)
    
    ## Precautionary cleaning
    rm(FoSBPMatrix,FoSMat,ppa)
    gc()
    
    results = rbind(results,temp)
  } ### End year loop
  
  
  
  return(results)
}

gc()
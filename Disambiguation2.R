manage.packages(c('data.table','splitstackshape','Matrix','proxy','utils'))


disambiguate.fiID = function(fiIDtable,id,runningmax){
  
  ############# subset the data
  ## all papers of the scientist we want to disambiguate
  papers = fiIDtable[fiID == id]$PaperID
  ## All the coauthors on those papers
  coauthors = fiIDtable[PaperID %in% papers]
  ### Have to find a way to re-id
  maxid = max(coauthors$fiID,coauthors$Id)
  
  
  ##temporary remap fiID of the focal scientist so we do not accidentally map over another Id
  ##We want to treat all variations of the focal scientist as unique while combining their collaborators
  coauthors$step1ID = 0
  
  ### Assign a unique id for the focal scientist based on their MAG ID
  coauthors[fiID == id]$step1ID = as.numeric(as.factor(coauthors[fiID == id]$AuthorID))
  idcount = max(as.numeric(as.factor(coauthors[fiID == id]$AuthorID)))
  
  coauthors[fiID != id]$step1ID = as.numeric(as.factor(coauthors$fiID))+idcount
  maxid = max(coauthors$step1ID)
  
  
  ### Bipartite matrix with the new ID and Papers
  bpMat = sparseMatrix(i = coauthors$step1ID,
                       j = coauthors$PaperId,
                       x =1,
                       use.last.ij = TRUE
  )
  
  rownames(bpMat) = as.character(seq(1,nrow(bpMat)))
  colnames(bpMat) = as.character(seq(1,ncol(bpMat)))
  
  ## Coauthors
  coauthomat = tcrossprod(bpMat)
  diag(coauthomat) = 0
  
  #### We want to do pairwise comparison between rows, but only of the scientists we are trying to disambiguate
  compareMat = coauthomat[unique(coauthors[fiID == id]$step1ID),]
  compareMat = as.matrix(compareMat)
  coauth_simil = simil(compareMat,by_rows = TRUE,pairwise = TRUE,method = "Jaccard")
  coauth_simil = as.matrix(coauth_simil)
  diag(coauth_simil) = 0
  
  coauth_simil_u = triu(coauth_simil)
  diag(coauth_simil_u) = 0
  
  ### results are those intersections with a similiarty > 0
  res = cbind(which(coauth_simil_u > 0, arr.ind = TRUE),coauth_simil_u[which(coauth_simil_u > 0, arr.ind = TRUE)])
  res = as.data.table(res)
  res$nextId_1 = rownames(coauth_simil_u)[res$V1]
  res$nextId_2 = rownames(coauth_simil_u)[res$V2]
  
  ### Combine the ID's
  coauthors$step2ID = coauthors$step1ID
  
  ### Two scientists are in the same cluster if they have a jaccard similiarity > 0
  g = graph_from_adjacency_matrix(coauth_simil,mode = "undirected",weighted = TRUE)
  cl1 = as.data.table(cbind(as.numeric(V(g)$name),as.numeric(clusters(g)$membership)))
  
  ## We can reduce the data size now
  coauthors = coauthors[step1ID %in% cl1$V1]
  
  ## cluster results
  setkey(coauthors,step1ID)
  setkey(cl1,V1)
  
  ### we assign the step2ID based on the cluster number
  coauthors[cl1]$step2ID = cl1$V2
  
  ###### return 
  results = coauthors[fiID == id]
  
  ##should take into account previously disambiguated scientists
  results$step3ID = results$step3ID+runningmax
  return(results)
}


remap.ids = function(authpapertable,fosid,cores){
 
  
  
  ## start with a data.table of PaperIDs, AuthorIDs, and Author Information
  
  ## Can't process NA
  #authpapertable = authpapertable[!is.na(FirstName)]
  
  ######## Step 1 - create integer ID's for all the papers
  paperinfo = authpapertable[,.(PaperID,Year)]
  paperinfo = unique(paperinfo)
  paperinfo$PaperId = seq(1,nrow(paperinfo),1)
  
  ######## Step 2 - create integer ID's for all the authors
  authorinfo = authpapertable[,.(AuthorID,FirstName,LastName,Gender)]
  authorinfo = unique(authorinfo)
  authorinfo$FI = substr(authorinfo$FirstName,1,1)
  authorinfo$ID = seq(1,nrow(authorinfo),1)
  
  ##### Step 3 - create integer ID's for all the unique FI & LastName combinations
  name_aggregate = authorinfo
  name_aggregate$count = 1
  name_aggregate = aggregate(count~FI+LastName,data = name_aggregate,FUN=sum)
  name_aggregate = as.data.table(name_aggregate)
  name_aggregate$fiID = seq(1,nrow(name_aggregate),1)
  
  ### Combine original author information with the new ID's based on FI
  setkey(authorinfo,LastName,FI)
  setkey(name_aggregate,LastName,FI)
  midmap = authorinfo[name_aggregate]
  
  #### Combine the original data with the author data and paper data
  setkey(authpapertable,AuthorID)
  setkey(midmap,AuthorID)
  comb = authpapertable[midmap]
  comb = comb[,.(PaperID, Year, AuthorID,FirstName,LastName,FI,Gender,ID,fiID,count)]
  setkey(comb,PaperID,Year)
  comb = comb[paperinfo]
  
  ### For each unique fiID with more than 1 instance in the combined data set
  ### our algorithm will produce 3 sets of IDs
  comb$step1ID = 0
  comb$step2ID = 0
  comb$step3ID = 1
  
  toAnalyze = unique(comb[count > 1,.(fiID,count)])
  
  n = nrow(toAnalyze)
  
  for (i in 1:n){
    comb2 = disambiguate.fiID(comb,toAnalyze[i]$fiID,max(comb$step3ID))
  }
  
  rm(comb)
  gc()
  
  
  ### For genderization in subsequent stepts
  comb2$Gendered = 'u'
  comb2$GenderProb = 1.0
  
  ## Save the intermediate file
  fname = paste('/home/rstudio1/ADVISOR/Data/',fosid,"_disambiguated_pt1.Rda",sep="")
  saveRDS(comb2,fname)
  
  system('mail -s \"First loop is done\" mrcosta@syr.edu')
  

  
  return(comb2)
}

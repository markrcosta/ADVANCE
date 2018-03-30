manage.packages(c('data.table','splitstackshape','Matrix','proxy'))


disambiguate.fiID = function(fiIDtable,id,runningmax,round2){
  
  
  
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
  
  coauthors[fiID != id]$step1ID = as.numeric(as.factor(coauthors[fiID != id]$AuthorID))
  maxid = max(coauthors$step1ID)
  
  ## this is for the focal scientist
  coauthors[fiID == id]$step1ID = seq(1,nrow(coauthors[fiID == id]),1)+maxid
  
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
  
  ## cluster results
  setkey(coauthors,step1ID)
  setkey(cl1,V1)
  
  ### we assign the step2ID based on the cluster number
  coauthors[step1ID %in% cl1$V1][cl1]$step2ID = maxid+ cl1$V2
  
  ## for each row in cluster results, assign the cluster number plus the maxid to the scientist clustered
  for (row in 1:nrow(cl1)){
    coauthors[step1ID == cl1[row]$V1]$step2ID = cl1[row]$V2+maxid
  }
  
  ### now we combine authors who have the same AuthorID assigned by MAG
  uniqueIDs = unique(coauthors[fiID == id]$AuthorID)
  
  ## If the scientist has more than 1 row of publications, we assign the mind step2ID to all those rows
  for (i in 1:length(uniqueIDs)){
    if (nrow(coauthors[AuthorID == uniqueIDs[[i]]]) > 1){
      coauthors[AuthorID == uniqueIDs[[i]]]$step2ID = min(coauthors[AuthorID == uniqueIDs[[i]]]$step2ID)
    }
  }
  
  ## Do we want a second pass? Probably doesn't work b/c have to move step3ID assignment
  if (round2 == TRUE){
    ############################### round 2
    bpMat = sparseMatrix(i = coauthors$step2ID,
                         j = coauthors$PaperId,
                         x =1
    )
    
    rownames(bpMat) = as.character(seq(1,nrow(bpMat)))
    colnames(bpMat) = as.character(seq(1,ncol(bpMat)))
    
    coauthomat = tcrossprod(bpMat)
    diag(coauthomat) = 0
    
    compareMat = coauthomat[unique(coauthors[fiID == id]$step2ID),]
    compareMat = as.matrix(compareMat)
    coauth_simil = simil(compareMat,by_rows = TRUE,pairwise = TRUE,method = "Jaccard")
    coauth_simil = as.matrix(coauth_simil)
    diag(coauth_simil) = 0
    
    coauth_simil_u = triu(coauth_simil)
    diag(coauth_simil_u) = 0
    
    res = cbind(which(coauth_simil_u > 0, arr.ind = TRUE),coauth_simil_u[which(coauth_simil_u > 0, arr.ind = TRUE)])
    res = as.data.table(res)
    res$nextId_1 = rownames(coauth_simil_u)[res$V1]
    res$nextId_2 = rownames(coauth_simil_u)[res$V2]
    
    ### If there are any id's to merge
    if (nrow(res)>0){
      ### Two scientists are in the same cluster if they have a jaccard similiarity > 0
      g = graph_from_adjacency_matrix(coauth_simil,mode = "undirected",weighted = TRUE)
      cl1 = as.data.table(cbind(as.numeric(V(g)$name),as.numeric(clusters(g)$membership)))
      
      ## cluster results
      setkey(coauthors,step1ID)
      setkey(cl1,V1)
      
      ### we assign the step3ID based on the cluster number
      coauthors[step1ID %in% cl1$V1][cl1]$step3ID = maxid+ cl1$V2
      
      
      for (row in 1:nrow(cl1)){
        coauthors[step1ID == cl1[row]$V1]$step3ID = cl1[row]$V2+maxid
      }
      
      ### now we combine authors who have the same AuthorID assigned by MAG
      uniqueIDs = unique(coauthors[fiID == id]$AuthorID)
      
      for (i in 1:length(uniqueIDs)){
        if (nrow(coauthors[AuthorID == uniqueIDs[[i]]]) > 1){
          coauthors[AuthorID == uniqueIDs[[i]]]$step3ID = min(coauthors[AuthorID == uniqueIDs[[i]]]$step3ID)
        }
      }  
    }
  }

  coauthors$step3ID = coauthors$step2ID

  ###### return 
  results = coauthors[fiID == id]
  
  ##should take into account previously disambiguated scientists
  results$step3ID = results$step3ID+runningmax
  return(results)
}

genderize.disambiguated = function(distable){
  
  ngendered = length(unique(distable[Gender != 'u' | is.na(Gender)]$Gender))
  
  if (ngendered > 1){
    nmale = nrow(distable[Gender == 'm'])
    nfemale = nrow(distable[Gender == 'f'])
    if (nmale > nfemale){
      distable$Gendered = 'm'
      distable$GenderProb = nmale/(nmale+nfemale)
    } else if (nfemale > nmale){
      distable$Gendered = 'f'
      distable$GenderProb = nfemale/(nmale+nfemale)
    } else if (nmale == nfemale){
      distable$Gendered = 'a'
      distable$GenderProb = 0.5
    }
  } else if (ngendered == 1){
    ## update all gender values if only 1 gender identified
    distable$Gendered = unique(distable[Gender != 'u' | is.na(Gender)]$Gender)
    distable$GenderProb = 1.0
  } else if (ngendered == 0){
    distable$Gendered = 'u'
    distable$GenderProb = 1.0
  }
  
  return(distable)
}

fix.id = function(disambTable){
  disambTable$reID = 0
  totalUnique = nrow(disambTable[step3ID == 1])+length(unique(disambTable[step3ID != 1]$step3ID))
  idlist = seq(1,totalUnique,1)
  blankid = setdiff(idlist,unique(disambTable$step3ID))
  disambTable[step3ID == 1]$reID = blankid[1:nrow(disambTable[step3ID == 1])]
  
  disambTable[step3ID != 1]$reID = disambTable[step3ID != 1]$step3ID
  setnames(disambTable,"AuthorID","AuthorIDOld")
  setnames(disambTable,"reID","AuthorID")
  return(disambTable)
}

remap.ids = function(authpapertable,round2){
 
  
  
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
  
  for (r in 1:nrow(toAnalyze)){
    comb[fiID == toAnalyze[r]$fiID] = disambiguate.fiID(comb,toAnalyze[r]$fiID,max(comb$step3ID),round2)
  }
  
  comb$Gendered = 'u'
  comb$GenderProb = 1.0
  
  toAnalyze = unique(comb[count > 1,.(step3ID,count)])
  
  for (r in 1:nrow(toAnalyze)){
    comb[step3ID == toAnalyze[r]$step3ID] = genderize.disambiguated(comb[step3ID == toAnalyze[r]$step3ID])
  }
  
  ## fix all scientists who still have a step3ID of 1
  #comb[step3ID == 1]$step3ID = max(comb$step3ID)+seq(1,nrow(comb[step3ID == 1]),1)
  
  return(comb)  
}





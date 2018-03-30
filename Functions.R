####################################### Functions
## remove punctuation
strip_names = function(x){
  res = gsub("[[:punct:]]","",x)
  res = gsub("^\\s+|\\s+$", "", res)
  return(res)
}

## Generate name variants for search

name.variants = function(firstn, m1,m2,lastn){
  ## from a table of names, create a list of list of name variants
  # fi mi1 last, fi m1 last, fi m1 m2 last, fi m1 mi2 last, fi last, fi mi1 m2 last, fi mi1m mi2 last
  # f mi1 last, f m1 last, f m1 m2 last, f m1 mi2 last, f last, f mi1 m2 last, f mi1m mi2 last
  
  ## descending order
  # if middle1 is empty, so is middle 2
  
  if (is.na(m1)){
    m1 = ''
  }
  if (is.na(m2)){
    m2 = ''
  }
  
  results = list()
  
  
  ## full first name
  # f m1 last
  results = c(results,
              paste(firstn, m1, lastn, sep=" ")
  )
  # f mi1 last
  results = c(results,
              paste(firstn, substr(m1,1,1), lastn, sep=" ")
  )
  
  # f m1 m2 last
  results = c(results,
              paste(firstn, m1, m2, lastn, sep=" ")
  )
  
  # f m1 mi2 last
  results = c(results,
              paste(firstn, m1, substr(m2,1,1), lastn, sep=" ")
  )
  
  # f last
  results = c(results,
              paste(firstn,lastn, sep=" ")
  )
  
  # f mi1 m2 last
  results = c(results,
              paste(firstn, substr(m1,1,1), m2, lastn, sep=" ")
  )
  
  # f mi1m mi2 last
  results = c(results,
              paste(firstn, substr(m1,1,1), substr(m2,1,1), lastn, sep=" ")
  )
  
  ### fi
  
  # fi m1 last
  results = c(results,
              paste(substr(firstn,1,1), m1, lastn, sep=" ")
  )
  # fi mi1 last
  results = c(results,
              paste(substr(firstn,1,1), substr(m1,1,1), lastn, sep=" ")
  )
  
  # fi m1 m2 last
  results = c(results,
              paste(substr(firstn,1,1), m1, m2, lastn, sep=" ")
  )
  
  # fi m1 mi2 last
  results = c(results,
              paste(substr(firstn,1,1), m1, substr(m2,1,1), lastn, sep=" ")
  )
  
  # fi last
  results = c(results,
              paste(substr(firstn,1,1),lastn, sep=" ")
  )
  
  # fi mi1 m2 last
  results = c(results,
              paste(substr(firstn,1,1), substr(m1,1,1), m2, lastn, sep=" ")
  )
  
  # fi mi1m mi2 last
  results = c(results,
              paste(substr(firstn,1,1), substr(m1,1,1), substr(m2,1,1), lastn, sep=" ")
  )
  
  ## Remove duplicate spaces created by null values
  results = gsub(' +',' ',results)
  
  ## If some name fields are empty there will be duplicate values
  results = unique(results)
  
  return (results)
}

## Get ids of researchers
get.ids = function (dt,dbcon,su){
  
  ## requires data.table and neo4j
  ## package checks
  if (!("data.table" %in% rownames(installed.packages()))){
    stop('data.table not installed')
  }
  
  if (!("RNeo4j" %in% rownames(installed.packages()))){
    stop('RNeo4j not installed')
  }
  
  if (!('name.variants' %in% lsf.str(pos = 1))){
    stop('name.variants function missing')
  }
  
  results = data.table(internalID=NA,AuthorID='', variant='')
  results = results[0]
  
  
  for (l in 1:nrow(dt)){
    
    ## generate name variant, requires name.variants function
    for (nvariant in name.variants(dt[l]$first,dt[l]$middle1, 
                                   dt[l]$middle2, dt[l]$last)){
      
      if (su == TRUE){
        afill = 'syr'
      } else {
        affil = dt[l]$degreeInstitution
      }    
      q = paste("MATCH (a:Author)-[r*2]->(af:Affiliation) where a.AuthorName = '",nvariant,"' AND af.AffiliationName CONTAINS '",affil,"' return a.AuthorID", sep='')
      r = cypher(dbcon,q)
      
      ## add if the result is not null
      if (!is.null(r)){
        results = rbind(results,
                              data.table(internalID = dt[l]$internalID, AuthorID = r$a.AuthorID, variant = nvariant)
        )
      } # end if
      
    } # end inner for loop
    
  } ## end outer for loop
  
  results = unique(results)  
  return (results)
}

### Basic function that takes list of MAG AuthorIDs and a list of the Field of Study Levels wanted and returns all AuthorID & FoSID pair
author.fos.pairs = function(auths, fos_level, children, child_target,dbcon){
  ## requires data.table and neo4j
  ## package checks
  if (!("data.table" %in% rownames(installed.packages()))){
    stop('data.table not installed')
  }
  
  if (!("RNeo4j" %in% rownames(installed.packages()))){
    stop('RNeo4j not installed')
  }
  
  ##
  ## Initiatlize a data table  
  results = data.table(AuthorID='',FoSID='')
  results = results[0]
  
  for (x in auths){
      q = paste("MATCH (a:Author) -[r:wrote]->(p:Paper)-[r2:about]->(f:FoS) where a.AuthorID ='",x,
                "' and f.Level=",fos_level," return a.AuthorID as AuthorID, f.FOSID as FoSID", sep='')
      
      r = as.data.table(cypher(dbcon,q))
      results = rbind(results,r)
      
  } ## End author loop
    
  
  
  results = unique(results)
  
  ## do we want the parent information?
  if (children == TRUE){
    
    results2 = data.table(AuthorID='',FoSID='')
    results2 = results[0]
    
    ## parents can go several levels up. What level are we targeting?
    for (l in (fos_level+1):child_target){
      
      for (x in auths){
        
        # Have to get potential child matches below
        q = paste("MATCH (a:Author) -[r:wrote]->(p:Paper)-[r2:about]->(f:FoS) where a.AuthorID ='",x,
                  "' and f.Level=",l," return a.AuthorID as AuthorID, f.FOSID as FoSID", sep='')
        
        r = as.data.table(cypher(dbcon,q))
        results2 = rbind(results2,r)
        
      } ## End author loop
      
      results2 = author.fos.parent.pairs(results2,fos_level,grph)
      results = rbind(results,results2)
      results = unique(results)
    } # end loop
  } # end if
  
  return(results)
} ## End author.fos.pairs

### Function that retrieves nested hierarchy of FoS, based on the fact that many papers have no L1 attached.
## Should take author-fos pairs and level of pairs and return author-fos parent pairs
author.fos.parent.pairs = function (authFoS,parentLevel, dbcon){
  
  ## requires data.table and neo4j
  ## package checks
  if (!("data.table" %in% rownames(installed.packages()))){
    stop('data.table not installed')
  }
  
  if (!("RNeo4j" %in% rownames(installed.packages()))){
    stop('RNeo4j not installed')
  }
  
  results = data.table(AuthorID='',FoSID='')
  results = results[0]
  
  for (n in 1:nrow(authFoS)){
    ## Takes an FoSID and gets the parent of that FoS at the level desired
    q= paste("match (f:FoS)<-[r:parent_of]-(f2:FoS) where f.FOSID = '",authFoS[n]$FoSID,"' and f2.Level = ",parentLevel," return f2.FOSID as FoSID",sep='')
    r = cypher(dbcon,q)
    if (!is.null(r)){
      temp = as.data.table(cbind(AuthorID=rep(authFoS[n]$AuthorID,length(r)),r))
      results = rbind(results,temp)
    }
  } # End for loop
  
  results = unique(results)
  
  return(results)
}

### Find the topic similarity between all potentially equivalent authors
## This will take a data.table with authorID-FoSID pairs and a data.table with internalID linked to variants and return
## a data.table with potentially equivalent author-author pairs and their similarity metric
## depends on proxy and data.table packages
## Function to compare the similaity of authors
author.author.fos.similarity = function(authFoS,authAuth){
  
  ## package checks
  if (!("data.table" %in% rownames(installed.packages()))){
    stop('data.table not installed')
  }
  
  if (!("proxy" %in% rownames(installed.packages()))){
    stop('proxy not installed')
  }
  
  
  ## data checks
  if (!('data.table' %in% class(authFoS))){
    stop('authFoS is not a data.table')
  } else if (!('AuthorID' %in% colnames(authFoS))){
    stop('AuthorID is not in column names')
  } else if (!('FoSID' %in% colnames(authFoS))){
    stop('FoSID is not in column names')
  }
  
  if (!('data.table' %in% class(authAuth))){
    stop('authAuth is not a data.table')
  } else if (!('AuthorID' %in% colnames(authAuth))){
    stop('AuthorID is not in column names')
  } else if (!('internalID' %in% colnames(authAuth))){
    stop('internalID is not in column names')
  }
  
  ## Order the authFoS table so we know output similarity matrix is ordered
  
  setkey(authFoS, AuthorID)
  
  ### identify all unique pairs of potentially equivalent authors
  ### need to keep an order because sim matrix contains only lower half of data
  setkey(authAuth,AuthorID)
  
  ## Initialize a data.table
  authAuth_new = authAuth[,.(internalID,AuthorID)]
  authAuth_new$AuthorID2 = ''
  
  authAuth_new = authAuth_new[0]
  
  for (id in unique(authAuth$internalID)){
    ## If there is more than one row we have to check for similarity
    if (nrow(authAuth[internalID == id]) > 1){
      subtab = authAuth[internalID == id]
      for (row in 1:(nrow(subtab)-1)){
        for (row2 in (row+1):nrow(subtab)){
          to_add = data.table(internalID = id, AuthorID = subtab[row]$AuthorID, AuthorID2 = subtab[row2]$AuthorID )
          authAuth_new = rbind(authAuth_new, to_add)
        }## end internal loop
      } ## end outer loop
    } # end if
  } # end master loop
  
  
  
  ## Initialize a bipartite matrix where authors in the rows & FoS IDs in the columns
  sim_results = matrix(data=0,nrow=length(unique(authFoS$AuthorID)), ncol = length(unique(authFoS$FoSID)))
  rownames(sim_results) = unique(authFoS$AuthorID)
  colnames(sim_results) = unique(authFoS$FoSID)
  
  ## Put a 1 at the intersection of authors and the areas they publish in
  for (n in 1:nrow(authFoS)){
    sim_results[authFoS[n]$AuthorID,authFoS[n]$FoSID] = 1
  }
  
  ## Run a rowwise similarity
  sim_mat = simil(sim_results, by_rows = TRUE, method = 'Jaccard', auto_convert_data_frames = TRUE)
  sim_mat = as.matrix(sim_mat,keep.rownames = TRUE, keep.colnames = TRUE)
  
  authAuth_new$similarity = 0
  for (row in 1:nrow(authAuth_new)){
    auth1 = authAuth_new[row]$AuthorID
    auth2 = authAuth_new[row]$AuthorID2
    
    if ((auth1 %in% rownames(sim_mat)) && (auth2 %in% rownames(sim_mat))){
      authAuth_new[row]$similarity = sim_mat[auth1,auth2]
    } else {
      authAuth_new[row]$similarity = NA
    }
  }
  return(authAuth_new)
}


## Pulling the first & last names out of a name string under the assumption the space is the delimiter
first.name = function (x){
  return(head
         (strsplit(x,split = " ")[[1]],1))
}

last.name = function (x){
  return(tail
         (strsplit(x,split = " ")[[1]],1))
}



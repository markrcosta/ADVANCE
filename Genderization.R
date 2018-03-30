genderize.disambiguated.name = function(distable,ncores){
  
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
  
  ## Renumber all single instances
  disambTable[step3ID == 1]$reID = seq(1,nrow(disambTable[step3ID == 1]),1)
  ## Renumber all disambiguated authors, starting with last number from above
  disambTable[step3ID != 1]$reID = as.numeric(as.factor(disambTable[step3ID != 1]$step3ID))+nrow(disambTable[step3ID == 1])
  
  ## rename columns
  setnames(disambTable,"AuthorID","AuthorIDOld")
  setnames(disambTable,"reID","AuthorID")
  return(disambTable)
}


genderize = function(dt,fosid,ncores){
  
  ### Need to find out which names we have to reconcile gender for
  toAnalyze = unique(dt[count > 1,.(step3ID,count)])
  
  ## How many names we have to genderize
  n = nrow(toAnalyze)
  
  ### More parallel magic
  cl = makeCluster(ncores)
  registerDoParallel(cl)
  
  results = foreach(i=1:n,.combine="rbind",.export=c("genderize.disambiguated.name"),.packages=c("data.table","doParallel","foreach")) %dopar% {
                      genderize.disambiguated.name(toAnalyze,ncores)
                    }
  stopCluster(cl)
  
  ### fix ids
  results = fix.id(results)
  
  ## save the table
  
  system(paste("mail -s \"Should be saving ",fosid,"\" mrcosta@syr.edu",sep=""))
  
  fname = paste('/home/rstudio1/ADVISOR/Data/',FoSList[[l]],"_disambiguated.Rda",sep="")
  saveRDS(results,fname)
  
  return(results)
}


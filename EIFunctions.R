### Basic functions to calculate EI-indices for gender & department

gender.EI.index = function(authtable, pubtable){
  ## Start with a table of scientists, including AuthorID, AuthorName, and Gender
  ## Also need an edge list of publications
  
  
  ## Requires an adjacency matrix, a table full of data on faculty, including Gender and Participant (name) columns,
  ## a list of female and male faculty, and a boolean flag stating whether the desired output is a table of EI-indices or means
  
  gender_totals = data.table(name = rownames(network[facList,]),
                             female.ties = rowSums(network[facList,femaleFac]),
                             male.ties = rowSums(network[facList,maleFaculty]),
                             total.ties = rowSums(network[facList,])
  )
  
  setkey (gender_totals, name)
  
  EI.gender = factable[,.(Participant,Gender)]
  setkey(EI.gender, Participant)
  
  EI.gender = EI.gender[gender_totals]
  EI.gender$EI = 0
  ## Ties by gender
  EI.gender[Gender == 1]$EI = (EI.gender[Gender == 1]$female.ties - EI.gender[Gender == 1]$male.ties)/EI.gender[Gender == 1]$total.ties
  EI.gender[Gender == 2]$EI = (EI.gender[Gender == 2]$male.ties - EI.gender[Gender == 2]$female.ties)/EI.gender[Gender == 2]$total.ties
  
  ### If we want to output the table or else just the means
  if (isTRUE(output.table)){
      return(EI.gender)
  } else {
    results = data.table(male.mean.EI =  mean(EI.gender[Gender == 1]$EI, na.rm = TRUE),
                           female.mean.EI = mean(EI.gender[Gender == 2]$EI, na.rm = TRUE)
                           )
    results$male.mean.EI = round(results$male.mean.EI, 3)
    results$female.mean.EI = round(results$female.mean.EI,3)
    return(results)
  }
}

gender.YQ.index = function(authtable,pubtable){
  ## Start with a table of scientists, including AuthorID, AuthorName, and Gender
  ## Also need an edge list of publications
  ## requires data.table, igraph, Matrix
  
  gender_totals = data.table(AuthorID = authtable$AuthorID,
                             Gender = authtable$Gender,
                             female.ties = 0,
                             male.ties = 0,
                             unk.ties = 0
                             total.ties = 0
                             Q = 0,
                             Q.male.upper = 0,
                             Q.female.upper = 0
  )
  
  ## create graph with AuthorID and Gender as properties
  ## For each V in the graph, get neighbordhood size, # male neighbors, # female neighbors, # uknown neighbors
  
  adj_mat = spMatrix(nrow=length(unique(gender_totals$AuthorID)), 
                     ncol=length(unique(pubtable$PaperID)),
                     i = as.numeric(factor(gender_totals$AuthorID)),
                     j = as.numeric(factor(pubtable$PaperID)),
                     x = rep(1,nrow(ap))
  )
                     
  rownames(adj_mat) = levels(factor(authtable$AuthorID))
  colnames(adj_mat) = levels(factor(papertable$PaperID)) 
                     
  ## Turn bipartite matrix into unimodal matrix use tcrossprod
  adjm = tcrossprod(FoSBPMatrix)
  g = graph_from_adjacency_matrix(adjm,weighted=TRUE,mode="undirected",diag=FALSE,add.colnames=TRUE)
  
  # Make sure AuthorIDs are ordered in the same manner as in graph object
  g = set_vertex_atrribute(g,Gender,index=V(g),gender_totals$Gender)
  
  ### Calculate YQ as attribute
  g = set_vertex_atrribute(g,Gender,index=V(g),gender_totals$Gender)
  
  YQ.gender = factable[,.(Participant,Gender)]
  setkey(YQ.gender, Participant)
  
  YQ.gender = YQ.gender[gender_totals]
  YQ.gender$YQ = 0
  
  ## Total possible ties by gender
  mcount = nrow(gender_totals[Gender == 'm'])
  fcount = nrow(gender_totals[Gender == 'f'])
  ucount = nrow(gender_totals[Gender == 'u'])
  
  ## Q = (IY - EX)/(IY+EX)
  ## I = Internal ties, E = External ties, X = missing internal ties, Y = missing external ties
  
  I = gender_totals[Gender == 'm']
  Y = fcount - YQ.gender[Gender == 1]$female.ties
  E = YQ.gender[Gender == 1]$female.ties
  X = mcount - YQ.gender[Gender == 1]$male.ties
  
  ## Ties by gender
  YQ.gender[Gender == 1]$YQ = ((I*Y) - (E*X))/((I*Y)+(E*X))
  
  I = YQ.gender[Gender == 2]$female.ties
  Y = mcount - YQ.gender[Gender == 2]$male.ties
  E = YQ.gender[Gender == 2]$male.ties
  X = fcount - YQ.gender[Gender == 2]$female.ties
  
  YQ.gender[Gender == 2]$YQ = o((I*Y) - (E*X))/((I*Y)+(E*X))
  
  ### If we want to output the table or else just the means
  if (isTRUE(output.table)){
    return(YQ.gender)
  } else {
    results = data.table(male.mean.YQ =  mean(YQ.gender[Gender == 1]$YQ, na.rm = TRUE),
                         female.mean.YQ = mean(YQ.gender[Gender == 2]$YQ, na.rm = TRUE)
    )
    results$male.mean.YQ = round(results$male.mean.YQ, 3)
    results$female.mean.YQ = round(results$female.mean.YQ,3)
    return(results)
  }
}
  

department.EI.index = function(network, factable, faclist, femaleFac, maleFac, output.table){
  
  ## Requires an adjacency matrix, a table full of data on faculty, including Gender and Participant (name) columns,
  ## a list of female and male faculty, and a boolean flag stating whether the desired output is a table of EI-indices or means
  
  EI.dept = data.table(Participant="", Gender = 0, within = 0, total = 0)
  EI.dept = EI.dept[0]
  
  for (n in 1:nrow(factable[Participant %in% faclist])){
    
    # to make the rest of the code readable
    nme = factable[Participant %in% faclist][n]$Participant
    dept = factable[Participant %in% faclist][n]$Department
    deptColl = factable[Department %in% dept]$Participant
    
    EI.dept = rbind(EI.dept, newtable = data.table(Participant = nme,
                                  Gender = factable[Participant == nme]$Gender,
                                  within = sum(network[nme,deptColl]),
                                  total = sum(network[nme,])
                                  ) #End newtable
    ) #End EI.dept
  } # End for loop
  
  EI.dept$EI = ((EI.dept$total-EI.dept$within)-EI.dept$within)/EI.dept$total
  
  ### If we want to output the table or else just the means
  if (isTRUE(output.table)){
    return(EI.dept)
  } else {
    results = data.table(male.mean.EI =  mean(EI.dept[Gender == 1]$EI, na.rm = TRUE),
                         female.mean.EI = mean(EI.dept[Gender == 2]$EI, na.rm = TRUE))
    
    results$male.mean.EI = round(results$male.mean.EI, 3)
    results$female.mean.EI = round(results$female.mean.EI,3)
    return(results)
  }
  
  
}

department.YQ.index = function(network, factable, faclist, femaleFac, maleFac, output.table){
  
  ## Requires an adjacency matrix, a table full of data on faculty, including Gender and Participant (name) columns,
  ## a list of female and male faculty, and a boolean flag stating whether the desired output is a table of YQ-indices or means
  
  YQ.dept = data.table(Participant="", Gender = 0, I = 0, E = 0, Y = 0, X = 0)
  YQ.dept = YQ.dept[0]
  
  for (n in 1:nrow(factable[Participant %in% faclist])){
    
    # to make the rest of the code readable
    nme = factable[Participant %in% faclist][n]$Participant
    dept = factable[Participant %in% faclist][n]$Department
    deptColl = factable[Department %in% dept]$Participant
    
    
    YQ.dept = rbind(YQ.dept, newtable = data.table(Participant = nme,
                                                   Gender = factable[Participant == nme]$Gender,
                                                   I = sum(network[nme,deptColl]),
                                                   E = sum(network[nme,]) - sum(network[nme,deptColl]),
                                                   Y = length(deptColl) - sum(network[nme,deptColl]),
                                                   X = (nrow(factable) - length(deptColl)) - (sum(network[nme,]) - sum(network[nme,deptColl]))
    ) #End newtable
    ) #End YQ.dept
  } # End for loop
  
  YQ.dept$YQ = ((YQ.dept$I*YQ.dept$Y) - (YQ.dept$E*YQ.dept$X))/((YQ.dept$I*YQ.dept$Y)+(YQ.dept$E*YQ.dept$X))
  
  ### If we want to output the table or else just the means
  if (isTRUE(output.table)){
    return(YQ.dept)
  } else {
    results = data.table(male.mean.YQ =  mean(YQ.dept[Gender == 1]$YQ, na.rm = TRUE),
                         female.mean.YQ = mean(YQ.dept[Gender == 2]$YQ, na.rm = TRUE))
    
    results$male.mean.YQ = round(results$male.mean.YQ, 3)
    results$female.mean.YQ = round(results$female.mean.YQ,3)
    return(results)
  }
  
  
}

## A function that can call either EI function
EI.calc = function(network, netType, rndm, Faculty, facultyList, femaleFaculty, maleFaculty, output.table){
  
  results = data.table(type = netType,random=rndm)
  if (netType == 'Gender'){
    results = cbind(results,gender.EI.index(network,Faculty,facultyList,femaleFaculty,maleFaculty, output.table))
  } else if(netType == 'Department'){
    results = cbind(results,department.EI.index(network,Faculty,facultyList,femaleFaculty,maleFaculty, output.table))
  } else {
    results = 'Error. Incorrect network type'
  }
  
  return(results)
}

YQ.calc = function(network, netType, rndm, Faculty, facultyList, femaleFaculty, maleFaculty, output.table){
  
  results = data.table(type = netType,random=rndm)
  if (netType == 'Gender'){
    results = cbind(results,gender.YQ.index(network,Faculty,facultyList,femaleFaculty,maleFaculty, output.table))
  } else if(netType == 'Department'){
    results = cbind(results,department.YQ.index(network,Faculty,facultyList,femaleFaculty,maleFaculty, output.table))
  } else {
    results = 'Error. Incorrect network type'
  }
  
  return(results)
}

match (a:Author) set a.Gender ='u' return count(*)

devtools::install_github("nicolewhite/RNeo4j")
library(RNeo4j)
library(data.table)

pword = readline(prompt="Enter neo4j password: ")
grph=startGraph("http://neo4j-genbank.syr.edu:7474/db/data/", username="neo4j",password=pword)

for (letter in letters){
  for (letter2 in letters){
  q = paste ("match (a:Author) where a.FirstName starts with '",letter,"' and a.LastName starts with '",letter2,"' set a.Probability = 1.0 return count(*)", sep = "")
  print(q)
  print(cypher(grph,q))
  }
}

rm(letter,letter2,q)

## Take data from genderizer and update

update.gender = function(genderizedNames){
  res = 0
  ## data.table with genderized names
  for (row in 1:nrow(genderizedNames)){
    q = paste ("match (a:Author) where a.FirstName = '",genderizedNames[row]$name,"' set a.Gender = '",genderizedNames[row]$gender,"', a.Probability = ",genderizedNames[row]$probability, 
               " return count(*)", sep = "")
    r = cypher(grph,q)
    res = res + r
  }
  return(res)
}

for (letter in letters){
  file2parse = paste('/home/ruser3/genderized/genderizedNames_',letter,'.rds',sep="")
  genderized = readRDS(file2parse)
  genderized = as.data.table(genderized)
  genderized$gender = substr(genderized$gender,1,1)
  print(paste(letter,update.gender(genderized), sep =" "))
}



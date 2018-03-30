# Generate Input files for First Literal. One file for each alphabet
# Connects to the neo4j db and writes .rds files in the location /home/roverma/fNameData

devtools::install_github("nicolewhite/RNeo4j")
library(RNeo4j)
library(data.table)

pword = readline(prompt="Enter neo4j password: ")
grph=startGraph("http://neo4j-genbank.syr.edu:7474/db/data/", username="neo4j",password=pword)

for (letter in letters){
  q = paste("match(a:Author) where a.FirstName starts with '",letter,"' return distinct a.FirstName as name, count(a.FirstName) as count", sep='')
  temp = as.data.table(cypher(grph,q))
  temp <- temp[which(nchar(temp$name) > 1),]
  rdsName <- gsub(" ","",paste("/home/roverma/fNameData/fNameWith_",letter,".rds"))  ##rds name format: fNameWith_<letter>.rds##
  saveRDS(temp, rdsName)
}
  
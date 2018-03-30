### Main function

############### Packages
options(stringsAsFactors = FALSE)

manage.packages(c('data.table','splitstackshape','igraph','Matrix','devtools','proxy','doParallel','foreach'))
devtools::install_github("nicolewhite/RNeo4j")
library(RNeo4j)

pword = readline(prompt="Enter neo4j password: ")

grph=startGraph("http://neo4j-genbank.syr.edu:7474/db/data/", username="neo4j",password=pword)


############### Call functions
source('Functions.R')

## First thing we need to do is identify which fields we want to analyze
### FoSIDList
FoSList = c("097AE067",
            "05ED8FE7",
            "0109A711",
            "1DFA4B82",
            "04371FDC",
            "02ACAD0D",
            "0A1E571C",
            "0765A2E4",
            "08FE992B",
            "03B9FD3C",
            "033D6521",
            "039D5C06")

### Then we need to get the following data for each field: PaperID,AuthorID,FirstName,LastName,Year, Gender
########### Loop through the FoSID's in the list
source("RetrieveAuthorPaperData.R")

authorPapers = list(length(FoSList))

## Get data ready
 
saveRDS(authorPapers,file='/home/rstudio1/ADVISOR/Data/authorPapers.Rda')
saveRDS(FoSList,file='/home/rstudio1/ADVISOR/Data/FoSList.Rda')

for (i in 1:length(authorPapers)){
  fname = paste('/home/rstudio1/ADVISOR/Data/',FoSList[[i]],'authorPapers.Rda',sep = '')
  saveRDS(authorPapers[[i]],fname)
}

rm(fname)

FoSResults = data.table(FoSID="",FoSName="",StartYear=0,EndYear=0,PubsPerAuthor=0,MeanDegree=0,GiantComp=0,ClusteringCoefficient= 0, 
                        NumPapers = 0,NumAuthors = 0,PctFemale = 0,PctMale = 0,PctUnk = 0)
FoSResults = FoSResults[0]

## Will create the object FoSResults
source("FoSAnalysisByYear.R")

## Analyze before disambiguation
for(li in 1:length(FoSList)){
  fname = paste('/home/rstudio1/ADVISOR/Data/',FoSList[[li]],'authorPapers.Rda',sep = '')
  fos = readRDS(fname)
  FoSResults = rbind(FoSResults,analyze.fos(FoSList[[li]],fos,6))
  print(li)
}
rm(li,fos)


##authorPaper length
apl = length(authorPapers)

#### Sometimes the result will include years beyond our time period of interest.
FoSResults = FoSResults[StartYear <= 2015 | EndYear > 2017]

## Disambiguate
source("Disambiguation2.R")
disambAuthorPapers = list(apl)

## Get data ready

cl = makeCluster(6)
registerDoParallel(cl)

disambAuthorPapers = foreach(i=3:apl,.export=c("evaluate.relationship","fix.id"),
                             .packages=c("Matrix","data.table","splitstackshape","proxy","igraph")) %dopar% {
  remap.ids(authorPapers[[i]],FoSList[[i]],5)
}
  
stopCluster(cl)

rm(authorPapers)
gc()

## Re-analyze
## Analyze after disambiguation
DisambFoSResults = FoSResults[0]


for(li in 1:length(disambAuthorPapers)){
  fname = fname = paste('home/rstudio1/ADVISOR/DATA/',FoSList[[li]],"_disambiguated.Rda",sep="")
  fos = load(fname)
  DisambFoSResults = rbind(DisambFoSResults,analyze.fos(FoSList[[li]],fos,6))
  fm(fos)
  print(li)
}
rm(i,l,li,email)

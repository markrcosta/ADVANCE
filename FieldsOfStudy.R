source('manage.packages.R')
manage.packages('data.table')
options(stringsAsFactors = FALSE)

## Get the files
download.file('https://academicgraph.blob.core.windows.net/graph-2016-02-05/FieldsOfStudy.zip','Data/FieldsOfStudy.zip')
download.file('https://academicgraph.blob.core.windows.net/graph-2016-02-05/FieldOfStudyHierarchy.zip','Data/FieldOfStudyHierarchy.zip')

## Unzip
unzip('Data/FieldsOfStudy.zip',exdir = 'Data')
unzip('Data/FieldOfStudyHierarchy.zip',exdir = 'Data')

## Read files
FoS = as.data.table(read.csv('Data/FieldsOfStudy.txt',sep='\t',quote = "", header = FALSE))
FoSHierarchy = as.data.table(read.csv('Data/FieldOfStudyHierarchy.txt',sep='\t',quote = "", header = FALSE))


## Rename the columns
setnames(FoS,c('FoSID','FoSName'))
setnames(FoSHierarchy,c('FoSID_Child','FoSID_Child_Level','FoSID_Parent','FoSID_Parent_Level','Confidence'))


## Basic description of FoS
## Unique levels
Levels = unique(FoSHierarchy$FoSID_Child_Level)
Levels = c(Levels,unique(FoSHierarchy$FoSID_Parent_Level))
Levels = unique(Levels)

## 4 levels - L0-L4

#### How many L0's are there?
a = unique(FoSHierarchy[FoSID_Parent_Level == 'L0']$FoSID_Parent)
b = unique(FoSHierarchy[FoSID_Child_Level == 'L0']$FoSID_Child)
length(unique(c(a,b)))

#### How many L1's are there?
a = unique(FoSHierarchy[FoSID_Parent_Level == 'L1']$FoSID_Parent)
b = unique(FoSHierarchy[FoSID_Child_Level == 'L1']$FoSID_Child)
length(unique(c(a,b)))


#### How many L2's are there?
a = unique(FoSHierarchy[FoSID_Parent_Level == 'L2']$FoSID_Parent)
b = unique(FoSHierarchy[FoSID_Child_Level == 'L2']$FoSID_Child)
length(unique(c(a,b)))

#### How many L3's are there?
a = unique(FoSHierarchy[FoSID_Parent_Level == 'L3']$FoSID_Parent)
b = unique(FoSHierarchy[FoSID_Child_Level == 'L3']$FoSID_Child)
length(unique(c(a,b)))

## Are there any FoS not in the hierarchy?
a = FoS$FoSID
b = c(FoSHierarchy$FoSID_Child, FoSHierarchy$FoSID_Parent)
c = setdiff(a,b)

FoS[FoSID %in%  c]

rm(a,b,c)

## Are there any FoS with more than one level?
## Create a master FoS table with Id, name, level
a = data.table(FoSHierarchy[,.(FoSID_Parent,FoSID_Parent_Level)])
setnames(a,c('FoSID','FoSLevel'))
b = data.table(FoSHierarchy[,.(FoSID_Child,FoSID_Child_Level)])
setnames(b,c('FoSID','FoSLevel'))
FoS_old = FoS
FoS = rbind(a,b)
FoS = unique(FoS)

FoS$count = 1
a = aggregate(count~FoSID+FoSLevel,data=FoS,FUN = sum)
a = as.data.table(a)  

a[count >1]
## = 0, no field is marked at 2 or more levels
rm(a,b)

FoS$count=NULL
setkey(FoS,FoSID)
setkey(FoS_old,FoSID)
FoS = FoS[FoS_old]
rm(FoS_old)

##### Who is parents of whom?

for (l in Levels){
  for (j in Levels){
    print(paste(l,j, nrow(FoSHierarchy[FoSID_Parent_Level == l & FoSID_Child_Level == j])))
  }
}
rm(j,l)

### Let's look at FoS that are in our set of interest
FoS[FoSLevel == 'L0']
FoS[FoSName %like% 'Speech' | FoSName %like% 'Communication']

## I want to manually inspect L1
write.table(FoS[FoSLevel == 'L1'],file = 'Output/L1.csv',quote = FALSE,sep = ',',row.names = FALSE)
write.table(FoS[FoSLevel == 'L0'],file = 'Output/L0.csv',quote = TRUE,sep = ',',row.names = FALSE)

## The list of L0 fields we want to keep
InList = c("00F03FC7",	"0205A1DB",	"0271BC14",	"052C8328",	"073B64E4",	"0796A60A",	"07982D63",	"0B0FEB68",	"0B7A44E7","0259B070")
# Psychology ,Mathematics, Computer Science, Biology, Physics, Geology, Engineering, Chemistry,  Materials Science, Environmental Science

## Expand the InList to include sub-topics
InList = c(InList, FoSHierarchy[FoSID_Parent %in% InList & FoSID_Child_Level == 'L1']$FoSID_Child)
InList = c(InList, FoSHierarchy[FoSID_Parent %in% InList & FoSID_Child_Level == 'L2']$FoSID_Child)
InList = c(InList, FoSHierarchy[FoSID_Parent %in% InList & FoSID_Child_Level == 'L3']$FoSID_Child)
InList = unique(InList)

write.csv(as.data.table(InList),file="inlist.csv")


## I want to create a few Gephi Files
## Node list
nodes = FoS
setnames(nodes, c('Id','Level','Name'))
nodes$In = 'N'
nodes[Id %in% InList | Id %in% FoSHierarchy[FoSID_Parent %in% InList]$FoSID_Child]$In = 'Y'
nodes = nodes[Level != 'NA']
nodes[Level == 'L0']$Level = 1
nodes[Level == 'L1']$Level = 2
nodes[Level == 'L2']$Level = 3
nodes[Level == 'L3']$Level = 4



write.table(nodes,file='Output/nodes.csv',quote=FALSE,sep=',',row.names = FALSE,col.names = TRUE)

edges = FoSHierarchy[,.(FoSID_Parent,FoSID_Child,Confidence)]
edges$Confidence = as.numeric(edges$Confidence)

setnames(edges,c('Source','Target', 'Weight'))
write.table(edges,file='Output/edges.csv',quote=FALSE,sep=',',row.names = FALSE,col.names = TRUE)
rm(nodes,edges)

rm(num_parents)

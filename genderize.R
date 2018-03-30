# genderize first literals

library(RNeo4j)
library(data.table)
library(genderizeR)
library(dplyr)
library(plyr)
library(stringr)

# function definition for api call
genderizeNames <- function(nameList) {
  tempDF = findGivenNames(nameList, queryLength = 10
                          , apikey = "1c07c3de8a958233fec00b8aff5b0db8")
  return(tempDF)
}

#### A #####
startTime <- proc.time() 
namesWith_a <- readRDS("/home/ruser3/fNameData/fNameWith_a.rds") 
namesWith_a$name <- str_replace_all(namesWith_a$name, "[^[:alnum:]]","") # remove all non alphanumeric
namesWith_a$name <- str_replace_all(namesWith_a$name, "[^[:ascii:]]","") # remove all non ascii
namesWith_a <- namesWith_a[which(str_length(namesWith_a$name) > 1),]
genderizedNames_a <- data.frame() 
failures <- data.frame() 
i <- 1
while (i <= nrow(namesWith_a)) {  
  status <- tryCatch({  
    genderizedNames_a <-  rbind(genderizedNames_a
                                ,ldply(namesWith_a$name[i],genderizeNames)) 
  }, error=function(e) e)
  if (class(status)[1] !='simpleError') i <- i + 1 
  else failures <- rbind(failures,c(paste('Error at: ',i))) # generate error message and retry process
}
runTime_a <- proc.time() - startTime
failures_a <- as.data.frame(table(failures))
saveRDS(runTime_a,"/home/ruser3/runtimes/runTime_a.rds")
saveRDS(failures_a,"/home/ruser3/failures/failures_a.rds")
saveRDS(genderizedNames_a,"/home/ruser3/genderized/genderizedNames_a.rds")

#### B ####
startTime <- proc.time() 
namesWith_b <- readRDS("/home/ruser3/fNameData/fNameWith_b.rds") 
namesWith_b$name <- str_replace_all(namesWith_b$name, "[^[:alnum:]]","") # remove all non alphanumeric
namesWith_b$name <- str_replace_all(namesWith_b$name, "[^[:ascii:]]","") # remove all non ascii
namesWith_b <- namesWith_b[which(str_length(namesWith_b$name) > 1),]
genderizedNames_b <- data.frame() 
failures <- data.frame() 
i <- 1
while (i <= nrow(namesWith_b)) {  
  status <- tryCatch({  
    genderizedNames_b <-  rbind(genderizedNames_b
                                ,ldply(namesWith_b$name[i],genderizeNames)) 
  }, error=function(e) e)
  if (class(status)[1] !='simpleError') i <- i + 1 
  else failures <- rbind(failures,c(paste('Error at: ',i)))
}
runTime_b <- proc.time() - startTime
failures_b <- as.data.frame(table(failures))
saveRDS(runTime_b,"/home/ruser3/runtimes/runTime_b.rds")
saveRDS(failures_b,"/home/ruser3/failures/failures_b.rds")
saveRDS(genderizedNames_b,"/home/ruser3/genderized/genderizedNames_b.rds")

#### C #####
startTime <- proc.time() 
namesWith_c <- readRDS("/home/ruser3/fNameData/fNameWith_c.rds") 
namesWith_c$name <- str_replace_all(namesWith_c$name, "[^[:alnum:]]","") # remove all non alphanumeric
namesWith_c$name <- str_replace_all(namesWith_c$name, "[^[:ascii:]]","") # remove all non ascii
namesWith_c <- namesWith_c[which(str_length(namesWith_c$name) > 1),]
genderizedNames_c <- data.frame() 
failures <- data.frame() 
i <- 1
while (i <= nrow(namesWith_c)) {  
  status <- tryCatch({  
    genderizedNames_c <-  rbind(genderizedNames_c
                                ,ldply(namesWith_c$name[i],genderizeNames)) 
  }, error=function(e) e)
  if (class(status)[1] !='simpleError') i <- i + 1 
  else failures <- rbind(failures,c(paste('Error at: ',i)))
}
runTime_c <- proc.time() - startTime
failures_c <- as.data.frame(table(failures))
saveRDS(runTime_c,"/home/ruser3/runtimes/runTime_c.rds")
saveRDS(failures_c,"/home/ruser3/failures/failures_c.rds")
saveRDS(genderizedNames_c,"/home/ruser3/genderized/genderizedNames_c.rds")


#### D ####
startTime <- proc.time() 
namesWith_d <- readRDS("/home/ruser3/fNameData/fNameWith_d.rds") 
namesWith_d$name <- str_replace_all(namesWith_d$name, "[^[:alnum:]]","") # remove all non alphanumeric
namesWith_d$name <- str_replace_all(namesWith_d$name, "[^[:ascii:]]","") # remove all non ascii
namesWith_d <- namesWith_d[which(str_length(namesWith_d$name) > 1),]
genderizedNames_d <- data.frame() 
failures <- data.frame() 
i <- 1
while (i <= nrow(namesWith_d)) {  
  status <- tryCatch({  
    genderizedNames_d <-  rbind(genderizedNames_d
                                ,ldply(namesWith_d$name[i],genderizeNames)) 
  }, error=function(e) e)
  if (class(status)[1] !='simpleError') i <- i + 1 
  else failures <- rbind(failures,c(paste('Error at: ',i)))
}
runTime_d <- proc.time() - startTime
failures_d <- as.data.frame(table(failures))
saveRDS(runTime_d,"/home/ruser3/runtimes/runTime_d.rds")
saveRDS(failures_d,"/home/ruser3/failures/failures_d.rds")
saveRDS(genderizedNames_d,"/home/ruser3/genderized/genderizedNames_d.rds")


#### E #####
startTime <- proc.time() 
namesWith_e <- readRDS("/home/ruser3/fNameData/fNameWith_e.rds") 
namesWith_e$name <- str_replace_all(namesWith_e$name, "[^[:alnum:]]","") # remove all non alphanumeric
namesWith_e$name <- str_replace_all(namesWith_e$name, "[^[:ascii:]]","") # remove all non ascii
namesWith_e <- namesWith_e[which(str_length(namesWith_e$name) > 1),]
genderizedNames_e <- data.frame() 
failures <- data.frame() 
i <- 1
while (i <= nrow(namesWith_e)) {  
  status <- tryCatch({  
    genderizedNames_e <-  rbind(genderizedNames_e
                                ,ldply(namesWith_e$name[i],genderizeNames)) 
  }, error=function(e) e)
  if (class(status)[1] !='simpleError') i <- i + 1 
  else failures <- rbind(failures,c(paste('Error at: ',i)))
}
runTime_e <- proc.time() - startTime
failures_e <- as.data.frame(table(failures))
saveRDS(runTime_e,"/home/ruser3/runtimes/runTime_e.rds")
saveRDS(failures_e,"/home/ruser3/failures/failures_e.rds")
saveRDS(genderizedNames_e,"/home/ruser3/genderized/genderizedNames_e.rds")

#### F ####
startTime <- proc.time() 
namesWith_f <- readRDS("/home/ruser3/fNameData/fNameWith_f.rds") 
namesWith_f$name <- str_replace_all(namesWith_f$name, "[^[:alnum:]]","") # remove all non alphanumeric
namesWith_f$name <- str_replace_all(namesWith_f$name, "[^[:ascii:]]","") # remove all non ascii
namesWith_f <- namesWith_f[which(str_length(namesWith_f$name) > 1),]
genderizedNames_f <- data.frame() 
failures <- data.frame() 
i <- 1
while (i <= nrow(namesWith_f)) {  
  status <- tryCatch({  
    genderizedNames_f <-  rbind(genderizedNames_f
                                ,ldply(namesWith_f$name[i],genderizeNames)) 
  }, error=function(e) e)
  if (class(status)[1] !='simpleError') i <- i + 1 
  else failures <- rbind(failures,c(paste('Error at: ',i)))
}
runTime_f <- proc.time() - startTime
failures_f <- as.data.frame(table(failures))
saveRDS(runTime_f,"/home/ruser3/runtimes/runTime_f.rds")
saveRDS(failures_f,"/home/ruser3/failures/failures_f.rds")
saveRDS(genderizedNames_f,"/home/ruser3/genderized/genderizedNames_f.rds")


### G ####
startTime <- proc.time() 
namesWith_g <- readRDS("/home/ruser3/fNameData/fNameWith_g.rds") 
namesWith_g$name <- str_replace_all(namesWith_g$name, "[^[:alnum:]]","") # remove all non alphanumeric
namesWith_g$name <- str_replace_all(namesWith_g$name, "[^[:ascii:]]","") # remove all non ascii
namesWith_g <- namesWith_g[which(str_length(namesWith_g$name) > 1),]
genderizedNames_g <- data.frame() 
failures <- data.frame() 
i <- 1
while (i <= nrow(namesWith_g)) {  
  status <- tryCatch({  
    genderizedNames_g <-  rbind(genderizedNames_g
                                ,ldply(namesWith_g$name[i],genderizeNames)) 
  }, error=function(e) e)
  if (class(status)[1] !='simpleError') i <- i + 1 
  else failures <- rbind(failures,c(paste('Error at: ',i)))
}
runTime_g <- proc.time() - startTime
failures_g <- as.data.frame(table(failures))
saveRDS(runTime_g,"/home/ruser3/runtimes/runTime_g.rds")
saveRDS(failures_g,"/home/ruser3/failures/failures_g.rds")
saveRDS(genderizedNames_g,"/home/ruser3/genderized/genderizedNames_g.rds")


  

#### H ####
startTime <- proc.time() 
namesWith_h <- readRDS("/home/ruser3/fNameData/fNameWith_h.rds") 
namesWith_h$name <- str_replace_all(namesWith_h$name, "[^[:alnum:]]","") # remove all non alphanumeric
namesWith_h$name <- str_replace_all(namesWith_h$name, "[^[:ascii:]]","") # remove all non ascii
namesWith_h <- namesWith_h[which(str_length(namesWith_h$name) > 1),]
genderizedNames_h <- data.frame() 
failures <- data.frame() 
i <- 1
while (i <= nrow(namesWith_h)) {  
  status <- tryCatch({  
    genderizedNames_h <-  rbind(genderizedNames_h
                                ,ldply(namesWith_h$name[i],genderizeNames)) 
  }, error=function(e) e)
  if (class(status)[1] !='simpleError') i <- i + 1 
  else failures <- rbind(failures,c(paste('Error at: ',i)))
}
runTime_h <- proc.time() - startTime
failures_h <- as.data.frame(table(failures))
saveRDS(runTime_h,"/home/ruser3/runtimes/runTime_h.rds")
saveRDS(failures_h,"/home/ruser3/failures/failures_h.rds")
saveRDS(genderizedNames_h,"/home/ruser3/genderized/genderizedNames_h.rds")

#### I ####
startTime <- proc.time() 
namesWith_i <- readRDS("/home/ruser3/fNameData/fNameWith_i.rds") 
namesWith_i$name <- str_replace_all(namesWith_i$name, "[^[:alnum:]]","") # remove all non alphanumeric
namesWith_i$name <- str_replace_all(namesWith_i$name, "[^[:ascii:]]","") # remove all non ascii
namesWith_i <- namesWith_i[which(str_length(namesWith_i$name) > 1),]
genderizedNames_i <- data.frame() 
failures <- data.frame() 
i <- 1
while (i <= nrow(namesWith_i)) {  
  status <- tryCatch({  
    genderizedNames_i <-  rbind(genderizedNames_i
                                ,ldply(namesWith_i$name[i],genderizeNames)) 
  }, error=function(e) e)
  if (class(status)[1] !='simpleError') i <- i + 1 
  else failures <- rbind(failures,c(paste('Error at: ',i)))
}
runTime_i <- proc.time() - startTime
failures_i <- as.data.frame(table(failures))
saveRDS(runTime_i,"/home/ruser3/runtimes/runTime_i.rds")
saveRDS(failures_i,"/home/ruser3/failures/failures_i.rds")
saveRDS(genderizedNames_i,"/home/ruser3/genderized/genderizedNames_i.rds")

#### J ####
startTime <- proc.time() 
namesWith_j <- readRDS("/home/ruser3/fNameData/fNameWith_j.rds") 
namesWith_j$name <- str_replace_all(namesWith_j$name, "[^[:alnum:]]","") # remove all non alphanumeric
namesWith_j$name <- str_replace_all(namesWith_j$name, "[^[:ascii:]]","") # remove all non ascii
namesWith_j <- namesWith_j[which(str_length(namesWith_j$name) > 1),]
genderizedNames_j <- data.frame() 
failures <- data.frame() 
i <- 1
while (i <= nrow(namesWith_j)) {  
  status <- tryCatch({  
    genderizedNames_j <-  rbind(genderizedNames_j
                                ,ldply(namesWith_j$name[i],genderizeNames)) 
  }, error=function(e) e)
  if (class(status)[1] !='simpleError') i <- i + 1 
  else failures <- rbind(failures,c(paste('Error at: ',i)))
}
runTime_j <- proc.time() - startTime
failures_j <- as.data.frame(table(failures))
saveRDS(runTime_j,"/home/ruser3/runtimes/runTime_j.rds")
saveRDS(failures_j,"/home/ruser3/failures/failures_j.rds")
saveRDS(genderizedNames_j,"/home/ruser3/genderized/genderizedNames_j.rds")

#### K ####
startTime <- proc.time() 
namesWith_k <- readRDS("/home/ruser3/fNameData/fNameWith_k.rds") 
namesWith_k$name <- str_replace_all(namesWith_k$name, "[^[:alnum:]]","") # remove all non alphanumeric
namesWith_k$name <- str_replace_all(namesWith_k$name, "[^[:ascii:]]","") # remove all non ascii
namesWith_k <- namesWith_k[which(str_length(namesWith_k$name) > 1),]
genderizedNames_k <- data.frame() 
failures <- data.frame() 
i <- 1
while (i <= nrow(namesWith_k)) {  
  status <- tryCatch({  
    genderizedNames_k <-  rbind(genderizedNames_k
                                ,ldply(namesWith_k$name[i],genderizeNames)) 
  }, error=function(e) e)
  if (class(status)[1] !='simpleError') i <- i + 1 
  else failures <- rbind(failures,c(paste('Error at: ',i)))
}
runTime_k <- proc.time() - startTime
failures_k <- as.data.frame(table(failures))
saveRDS(runTime_k,"/home/ruser3/runtimes/runTime_k.rds")
saveRDS(failures_k,"/home/ruser3/failures/failures_k.rds")
saveRDS(genderizedNames_k,"/home/ruser3/genderized/genderizedNames_k.rds")

#### L ####
startTime <- proc.time() 
namesWith_l <- readRDS("/home/ruser3/fNameData/fNameWith_l.rds") 
namesWith_l$name <- str_replace_all(namesWith_l$name, "[^[:alnum:]]","") # remove all non alphanumeric
namesWith_l$name <- str_replace_all(namesWith_l$name, "[^[:ascii:]]","") # remove all non ascii
namesWith_l <- namesWith_l[which(str_length(namesWith_l$name) > 1),]
genderizedNames_l <- data.frame() 
failures <- data.frame() 
i <- 1
while (i <= nrow(namesWith_l)) {  
  status <- tryCatch({  
    genderizedNames_l <-  rbind(genderizedNames_l
                                ,ldply(namesWith_l$name[i],genderizeNames)) 
  }, error=function(e) e)
  if (class(status)[1] !='simpleError') i <- i + 1 
  else failures <- rbind(failures,c(paste('Error at: ',i)))
}
runTime_l <- proc.time() - startTime
failures_l <- as.data.frame(table(failures))
saveRDS(runTime_l,"/home/ruser3/runtimes/runTime_l.rds")
saveRDS(failures_l,"/home/ruser3/failures/failures_l.rds")
saveRDS(genderizedNames_l,"/home/ruser3/genderized/genderizedNames_l.rds")


#### M ####
startTime <- proc.time() 
namesWith_m <- readRDS("/home/ruser3/fNameData/fNameWith_m.rds") 
namesWith_m$name <- str_replace_all(namesWith_m$name, "[^[:alnum:]]","") # remove all non alphanumeric
namesWith_m$name <- str_replace_all(namesWith_m$name, "[^[:ascii:]]","") # remove all non ascii
namesWith_m <- namesWith_m[which(str_length(namesWith_m$name) > 1),]
genderizedNames_m <- data.frame() 
failures <- data.frame() 
i <- 1
while (i <= nrow(namesWith_m)) {  
  status <- tryCatch({  
    genderizedNames_m <-  rbind(genderizedNames_m
                                ,ldply(namesWith_m$name[i],genderizeNames)) 
  }, error=function(e) e)
  if (class(status)[1] !='simpleError') i <- i + 1 
  else failures <- rbind(failures,c(paste('Error at: ',i)))
}
runTime_m <- proc.time() - startTime
failures_m <- as.data.frame(table(failures))
saveRDS(runTime_m,"/home/ruser3/runtimes/runTime_m.rds")
saveRDS(failures_m,"/home/ruser3/failures/failures_m.rds")
saveRDS(genderizedNames_m,"/home/ruser3/genderized/genderizedNames_m.rds")

#### N ####
startTime <- proc.time() 
namesWith_n <- readRDS("/home/ruser3/fNameData/fNameWith_n.rds") 
namesWith_n$name <- str_replace_all(namesWith_n$name, "[^[:alnum:]]","") # remove all non alphanumeric
namesWith_n$name <- str_replace_all(namesWith_n$name, "[^[:ascii:]]","") # remove all non ascii
namesWith_n <- namesWith_n[which(str_length(namesWith_n$name) > 1),]
genderizedNames_n <- data.frame() 
failures <- data.frame() 
i <- 1
while (i <= nrow(namesWith_n)) {  
  status <- tryCatch({  
    genderizedNames_n <-  rbind(genderizedNames_n
                                ,ldply(namesWith_n$name[i],genderizeNames)) 
  }, error=function(e) e)
  if (class(status)[1] !='simpleError') i <- i + 1 
  else failures <- rbind(failures,c(paste('Error at: ',i)))
}
runTime_n <- proc.time() - startTime
failures_n <- as.data.frame(table(failures))
saveRDS(runTime_n,"/home/ruser3/runtimes/runTime_n.rds")
saveRDS(failures_n,"/home/ruser3/failures/failures_n.rds")
saveRDS(genderizedNames_n,"/home/ruser3/genderized/genderizedNames_n.rds")


#### O ####
startTime <- proc.time() 
namesWith_o <- readRDS("/home/ruser3/fNameData/fNameWith_o.rds") 
namesWith_o$name <- str_replace_all(namesWith_o$name, "[^[:alnum:]]","") # remove all non alphanumeric
namesWith_o$name <- str_replace_all(namesWith_o$name, "[^[:ascii:]]","") # remove all non ascii
namesWith_o <- namesWith_o[which(str_length(namesWith_o$name) > 1),]
genderizedNames_o <- data.frame() 
failures <- data.frame() 
i <- 1
while (i <= nrow(namesWith_o)) {  
  status <- tryCatch({  
    genderizedNames_o <-  rbind(genderizedNames_o
                                ,ldply(namesWith_o$name[i],genderizeNames)) 
  }, error=function(e) e)
  if (class(status)[1] !='simpleError') i <- i + 1 
  else failures <- rbind(failures,c(paste('Error at: ',i)))
}
runTime_o <- proc.time() - startTime
failures_o <- as.data.frame(table(failures))
saveRDS(runTime_o,"/home/ruser3/runtimes/runTime_o.rds")
saveRDS(failures_o,"/home/ruser3/failures/failures_o.rds")
saveRDS(genderizedNames_o,"/home/ruser3/genderized/genderizedNames_o.rds")

#### P ####
startTime <- proc.time() 
namesWith_p <- readRDS("/home/ruser3/fNameData/fNameWith_p.rds") 
namesWith_p$name <- str_replace_all(namesWith_p$name, "[^[:alnum:]]","") # remove all non alphanumeric
namesWith_p$name <- str_replace_all(namesWith_p$name, "[^[:ascii:]]","") # remove all non ascii
namesWith_p <- namesWith_p[which(str_length(namesWith_p$name) > 1),]
genderizedNames_p <- data.frame() 
failures <- data.frame() 
i <- 1
while (i <= nrow(namesWith_p)) {  
  status <- tryCatch({  
    genderizedNames_p <-  rbind(genderizedNames_p
                                ,ldply(namesWith_p$name[i],genderizeNames)) 
  }, error=function(e) e)
  if (class(status)[1] !='simpleError') i <- i + 1 
  else failures <- rbind(failures,c(paste('Error at: ',i)))
}
runTime_p <- proc.time() - startTime
failures_p <- as.data.frame(table(failures))
saveRDS(runTime_p,"/home/ruser3/runtimes/runTime_p.rds")
saveRDS(failures_p,"/home/ruser3/failures/failures_p.rds")
saveRDS(genderizedNames_p,"/home/ruser3/genderized/genderizedNames_p.rds")

#### Q ####
startTime <- proc.time() 
namesWith_q <- readRDS("/home/ruser3/fNameData/fNameWith_q.rds") 
namesWith_q$name <- str_replace_all(namesWith_q$name, "[^[:alnum:]]","") # remove all non alphanumeric
namesWith_q$name <- str_replace_all(namesWith_q$name, "[^[:ascii:]]","") # remove all non ascii
namesWith_q <- namesWith_q[which(str_length(namesWith_q$name) > 1),]
genderizedNames_q <- data.frame() 
failures <- data.frame() 
i <- 1
while (i <= nrow(namesWith_q)) {  
  status <- tryCatch({  
    genderizedNames_q <-  rbind(genderizedNames_q
                                ,ldply(namesWith_q$name[i],genderizeNames)) 
  }, error=function(e) e)
  if (class(status)[1] !='simpleError') i <- i + 1 
  else failures <- rbind(failures,c(paste('Error at: ',i)))
}
runTime_q <- proc.time() - startTime
failures_q <- as.data.frame(table(failures))
saveRDS(runTime_q,"/home/ruser3/runtimes/runTime_q.rds")
saveRDS(failures_q,"/home/ruser3/failures/failures_q.rds")
saveRDS(genderizedNames_q,"/home/ruser3/genderized/genderizedNames_q.rds")

#### R ####
startTime <- proc.time() 
namesWith_r <- readRDS("/home/ruser3/fNameData/fNameWith_r.rds") 
namesWith_r$name <- str_replace_all(namesWith_r$name, "[^[:alnum:]]","") # remove all non alphanumeric
namesWith_r$name <- str_replace_all(namesWith_r$name, "[^[:ascii:]]","") # remove all non ascii
namesWith_r <- namesWith_r[which(str_length(namesWith_r$name) > 1),]
genderizedNames_r <- data.frame() 
failures <- data.frame() 
i <- 1
while (i <= nrow(namesWith_r)) {  
  status <- tryCatch({  
    genderizedNames_r <-  rbind(genderizedNames_r
                                ,ldply(namesWith_r$name[i],genderizeNames)) 
  }, error=function(e) e)
  if (class(status)[1] !='simpleError') i <- i + 1 
  else failures <- rbind(failures,c(paste('Error at: ',i)))
}
runTime_r <- proc.time() - startTime
failures_r <- as.data.frame(table(failures))
saveRDS(runTime_r,"/home/ruser3/runtimes/runTime_r.rds")
saveRDS(failures_r,"/home/ruser3/failures/failures_r.rds")
saveRDS(genderizedNames_r,"/home/ruser3/genderized/genderizedNames_r.rds")

#### S ####
startTime <- proc.time() 
namesWith_s <- readRDS("/home/ruser3/fNameData/fNameWith_s.rds") 
namesWith_s$name <- str_replace_all(namesWith_s$name, "[^[:alnum:]]","") # remove all non alphanumeric
namesWith_s$name <- str_replace_all(namesWith_s$name, "[^[:ascii:]]","") # remove all non ascii
namesWith_s <- namesWith_s[which(str_length(namesWith_s$name) > 1),]
genderizedNames_s <- data.frame() 
failures <- data.frame() 
i <- 1
while (i <= nrow(namesWith_s)) {  
  status <- tryCatch({  
    genderizedNames_s <-  rbind(genderizedNames_s
                                ,ldply(namesWith_s$name[i],genderizeNames)) 
  }, error=function(e) e)
  if (class(status)[1] !='simpleError') i <- i + 1 
  else failures <- rbind(failures,c(paste('Error at: ',i)))
}
runTime_s <- proc.time() - startTime
failures_s <- as.data.frame(table(failures))
saveRDS(runTime_s,"/home/ruser3/runtimes/runTime_s.rds")
saveRDS(failures_s,"/home/ruser3/failures/failures_s.rds")
saveRDS(genderizedNames_s,"/home/ruser3/genderized/genderizedNames_s.rds")

#### T ####
startTime <- proc.time() 
namesWith_t <- readRDS("/home/ruser3/fNameData/fNameWith_t.rds") 
namesWith_t$name <- str_replace_all(namesWith_t$name, "[^[:alnum:]]","") # remove all non alphanumeric
namesWith_t$name <- str_replace_all(namesWith_t$name, "[^[:ascii:]]","") # remove all non ascii
namesWith_t <- namesWith_t[which(str_length(namesWith_t$name) > 1),]
genderizedNames_t <- data.frame() 
failures <- data.frame() 
i <- 1
while (i <= nrow(namesWith_t)) {  
  status <- tryCatch({  
    genderizedNames_t <-  rbind(genderizedNames_t
                                ,ldply(namesWith_t$name[i],genderizeNames)) 
  }, error=function(e) e)
  if (class(status)[1] !='simpleError') i <- i + 1 
  else failures <- rbind(failures,c(paste('Error at: ',i)))
}
runTime_t <- proc.time() - startTime
failures_t <- as.data.frame(table(failures))
saveRDS(runTime_t,"/home/ruser3/runtimes/runTime_t.rds")
saveRDS(failures_t,"/home/ruser3/failures/failures_t.rds")
saveRDS(genderizedNames_t,"/home/ruser3/genderized/genderizedNames_t.rds")

#### U ####
startTime <- proc.time() 
namesWith_u <- readRDS("/home/ruser3/fNameData/fNameWith_u.rds") 
namesWith_u$name <- str_replace_all(namesWith_u$name, "[^[:alnum:]]","") # remove all non alphanumeric
namesWith_u$name <- str_replace_all(namesWith_u$name, "[^[:ascii:]]","") # remove all non ascii
namesWith_u <- namesWith_u[which(str_length(namesWith_u$name) > 1),]
genderizedNames_u <- data.frame() 
failures <- data.frame() 
i <- 1
while (i <= nrow(namesWith_u)) {  
  status <- tryCatch({  
    genderizedNames_u <-  rbind(genderizedNames_u
                                ,ldply(namesWith_u$name[i],genderizeNames)) 
  }, error=function(e) e)
  if (class(status)[1] !='simpleError') i <- i + 1 
  else failures <- rbind(failures,c(paste('Error at: ',i)))
}
runTime_u <- proc.time() - startTime
failures_u <- as.data.frame(table(failures))
saveRDS(runTime_u,"/home/ruser3/runtimes/runTime_u.rds")
saveRDS(failures_u,"/home/ruser3/failures/failures_u.rds")
saveRDS(genderizedNames_u,"/home/ruser3/genderized/genderizedNames_u.rds")

#### V ####
startTime <- proc.time() 
namesWith_v <- readRDS("/home/ruser3/fNameData/fNameWith_v.rds") 
namesWith_v$name <- str_replace_all(namesWith_v$name, "[^[:alnum:]]","") # remove all non alphanumeric
namesWith_v$name <- str_replace_all(namesWith_v$name, "[^[:ascii:]]","") # remove all non ascii
namesWith_v <- namesWith_v[which(str_length(namesWith_v$name) > 1),]
genderizedNames_v <- data.frame() 
failures <- data.frame() 
i <- 1
while (i <= nrow(namesWith_v)) {  
  status <- tryCatch({  
    genderizedNames_v <-  rbind(genderizedNames_v
                                ,ldply(namesWith_v$name[i],genderizeNames)) 
  }, error=function(e) e)
  if (class(status)[1] !='simpleError') i <- i + 1 
  else failures <- rbind(failures,c(paste('Error at: ',i)))
}
runTime_v <- proc.time() - startTime
failures_v <- as.data.frame(table(failures))
saveRDS(runTime_v,"/home/ruser3/runtimes/runTime_v.rds")
saveRDS(failures_v,"/home/ruser3/failures/failures_v.rds")
saveRDS(genderizedNames_v,"/home/ruser3/genderized/genderizedNames_v.rds")

#### W ####
startTime <- proc.time() 
namesWith_w <- readRDS("/home/ruser3/fNameData/fNameWith_w.rds") 
namesWith_w$name <- str_replace_all(namesWith_w$name, "[^[:alnum:]]","") # remove all non alphanumeric
namesWith_w$name <- str_replace_all(namesWith_w$name, "[^[:ascii:]]","") # remove all non ascii
namesWith_w <- namesWith_w[which(str_length(namesWith_w$name) > 1),]
genderizedNames_w <- data.frame() 
failures <- data.frame() 
i <- 1
while (i <= nrow(namesWith_w)) {  
  status <- tryCatch({  
    genderizedNames_w <-  rbind(genderizedNames_w
                                ,ldply(namesWith_w$name[i],genderizeNames)) 
  }, error=function(e) e)
  if (class(status)[1] !='simpleError') i <- i + 1 
  else failures <- rbind(failures,c(paste('Error at: ',i)))
}
runTime_w <- proc.time() - startTime
failures_w <- as.data.frame(table(failures))
saveRDS(runTime_w,"/home/ruser3/runtimes/runTime_w.rds")
saveRDS(failures_w,"/home/ruser3/failures/failures_w.rds")
saveRDS(genderizedNames_w,"/home/ruser3/genderized/genderizedNames_w.rds")


#### X ####
startTime <- proc.time() 
namesWith_x <- readRDS("/home/ruser3/fNameData/fNameWith_x.rds") 
namesWith_x$name <- str_replace_all(namesWith_x$name, "[^[:alnum:]]","") # remove all non alphanumeric
namesWith_x$name <- str_replace_all(namesWith_x$name, "[^[:ascii:]]","") # remove all non ascii
namesWith_x <- namesWith_x[which(str_length(namesWith_x$name) > 1),]
genderizedNames_x <- data.frame() 
failures <- data.frame() 
i <- 1
while (i <= nrow(namesWith_x)) {  
  status <- tryCatch({  
    genderizedNames_x <-  rbind(genderizedNames_x
                                ,ldply(namesWith_x$name[i],genderizeNames)) 
  }, error=function(e) e)
  if (class(status)[1] !='simpleError') i <- i + 1 
  else failures <- rbind(failures,c(paste('Error at: ',i)))
}
runTime_x <- proc.time() - startTime
failures_x <- as.data.frame(table(failures))
saveRDS(runTime_x,"/home/ruser3/runtimes/runTime_x.rds")
saveRDS(failures_x,"/home/ruser3/failures/failures_x.rds")
saveRDS(genderizedNames_x,"/home/ruser3/genderized/genderizedNames_x.rds")

#### Y ####
startTime <- proc.time() 
namesWith_y <- readRDS("/home/ruser3/fNameData/fNameWith_y.rds") 
namesWith_y$name <- str_replace_all(namesWith_y$name, "[^[:alnum:]]","") # remove all non alphanumeric
namesWith_y$name <- str_replace_all(namesWith_y$name, "[^[:ascii:]]","") # remove all non ascii
namesWith_y <- namesWith_y[which(str_length(namesWith_y$name) > 1),]
genderizedNames_y <- data.frame() 
failures <- data.frame() 
i <- 1
while (i <= nrow(namesWith_y)) {  
  status <- tryCatch({  
    genderizedNames_y <-  rbind(genderizedNames_y
                                ,ldply(namesWith_y$name[i],genderizeNames)) 
  }, error=function(e) e)
  if (class(status)[1] !='simpleError') i <- i + 1 
  else failures <- rbind(failures,c(paste('Error at: ',i)))
}
runTime_y <- proc.time() - startTime
failures_y <- as.data.frame(table(failures))
saveRDS(runTime_y,"/home/ruser3/runtimes/runTime_y.rds")
saveRDS(failures_y,"/home/ruser3/failures/failures_y.rds")
saveRDS(genderizedNames_y,"/home/ruser3/genderized/genderizedNames_y.rds")

#### Z ####
startTime <- proc.time() 
namesWith_z <- readRDS("/home/ruser3/fNameData/fNameWith_z.rds") 
namesWith_z$name <- str_replace_all(namesWith_z$name, "[^[:alnum:]]","") # remove all non alphanumeric
namesWith_z$name <- str_replace_all(namesWith_z$name, "[^[:ascii:]]","") # remove all non ascii
namesWith_z <- namesWith_z[which(str_length(namesWith_z$name) > 1),]
genderizedNames_z <- data.frame() 
failures <- data.frame() 
i <- 1
while (i <= nrow(namesWith_z)) {  
  status <- tryCatch({  
    genderizedNames_z <-  rbind(genderizedNames_z
                                ,ldply(namesWith_z$name[i],genderizeNames)) 
  }, error=function(e) e)
  if (class(status)[1] !='simpleError') i <- i + 1 
  else failures <- rbind(failures,c(paste('Error at: ',i)))
}
runTime_z <- proc.time() - startTime
failures_z <- as.data.frame(table(failures))
saveRDS(runTime_z,"/home/ruser3/runtimes/runTime_z.rds")
saveRDS(failures_z,"/home/ruser3/failures/failures_z.rds")
saveRDS(genderizedNames_z,"/home/ruser3/genderized/genderizedNames_z.rds")
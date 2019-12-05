
# Importing PSP data ------------------------------------------------------

#Import sample and then tree data
import.psp <- function(dat.type, tsas){
  read.list <- list()
  dat.list <- list()
  for(i in 1:length(dat.type)){
    setwd(paste0(r.path,dat.type[i]))
    for(j in 1:length(tsas)){
      read.list[[j]]<- fread(paste0("TSA",tsas[j],".csv"))
    }
    dat.list[[i]] <- rbindlist(read.list)
  }
  return(dat.list)
}

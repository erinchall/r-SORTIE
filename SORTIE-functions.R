
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


# Create initial tree population table ------------------------------------

SORTIE.tree.create <- function(sizeClasses,SORTIE.species,Min.Adult.DBH,Max.Seedling.Hgt.m){
  Init.Dens.Seedling.Hgt.Class.1 <- rep(0, length(SORTIE.species))
  Init.Dens.Seedling.Hgt.Class.2 <- rep(0, length(SORTIE.species))
  Init.Dens.Seedling.Hgt.Class.3 <- rep(0, length(SORTIE.species))
  Init.Dens.Seedling <- rep(0, length(SORTIE.species))
  
  inits <- vector()
  init.values <- matrix(nrow=length(sizeClasses),ncol=length(SORTIE.species))
  for(i in 1:length(sizeClasses)){
    inits[i] <- paste0("Init.Dens.",sizeClasses[i])
  }
  row.names(init.values) <- inits
  for(j in 1:length(SORTIE.species)){
    init.values[,j] <- rep(0, length(inits))
  }
  
  # Now create the data.table of parameter values that vary
  SORTIE.tree.dat <- data.table()
  SORTIE.tree.dat <- rbind(Min.Adult.DBH,Max.Seedling.Hgt.m,Init.Dens.Seedling.Hgt.Class.1,
                           Init.Dens.Seedling.Hgt.Class.2,Init.Dens.Seedling.Hgt.Class.3,Init.Dens.Seedling,init.values)
  colnames(SORTIE.tree.dat)<-SORTIE.species
  return(SORTIE.tree.dat)
}


# Stems/ha defined by SORTIE DBH classes ----------------------------------

create.SORTIE.DBH.classes <- function(tree.dat,num.meas,sizeClasses,SORTIE.tree.dat,plot.SORTIE){
  out.list <- list()
  for(r in 1:length(plot.SORTIE)){
    SORTIE.tree.dat.list <- list()
    Plot.ReMeas.MainPlot.list <- list()
    Plot.ReMeas.list <- list()
    red.plot.SORTIE.meas.list <- list()
    ind.num <- num.meas[r]
    for(i in 1:ind.num){
      plot.SORTIE.meas <- tree.dat[samp_id==plot.SORTIE[r] & meas_no==(i-1)]
      plot.SORTIE.meas[,LD_Group:=ifelse(ld=="L",1,ifelse(ld=="I",1,ifelse(ld=="V",1,2)))]
      red.plot.SORTIE.meas <- plot.SORTIE.meas[,.(samp_id,meas_yr,phf_tree,sp_PSP,dbh,ld,LD_Group,age_bh,height)]
      red.plot.SORTIE.meas.list[[i]] <- red.plot.SORTIE.meas
      #just live species
      main.plot.phf <- min(red.plot.SORTIE.meas[,phf_tree])
      ld.red.plot.SORTIE.meas <- red.plot.SORTIE.meas[LD_Group==1 & phf_tree==main.plot.phf]
      for(j in 1:length(sizeClasses)){
        ld.red.plot.SORTIE.meas[dbh <= sizeClasses[j] & dbh > sizeClasses[j]-2,DBH_bin := j]
      }
      Plot.ReMeas.list[[i]] <- red.plot.SORTIE.meas[phf_tree==main.plot.phf]
      tree.per.bin <- ld.red.plot.SORTIE.meas[,.N, by=.(DBH_bin,sp_PSP)]
      tree.per.bin[,Trees.per.ha := N*main.plot.phf]
      setkey(tree.per.bin,sp_PSP,DBH_bin)
      
      SORTIE.tree.dat.list[[i]] <- SORTIE.tree.dat
      for(k in 1:nrow(tree.per.bin)){
        SORTIE.tree.dat.list[[i]][6+tree.per.bin[k,DBH_bin],tree.conv.table[PSP.species==tree.per.bin[k,sp_PSP],SORTIE.species]] <- tree.per.bin[k,Trees.per.ha]
      }
    }
    out.list[[r]] <- SORTIE.tree.dat.list
  }
  return(out.list)
}

# Import SORTIE outputs ---------------------------------------------------

import.SORTIE.output <- function(yr,folder.path,root.name){
  dt_sites2 <- list()
  setwd(folder.path)
  fl.name <- paste0(root.name,"_det_")
  dt_list <- list()
  dt <- data.table()
  yr <- yr
  for(i in 1:(yr+1)){
    dt <- fread(paste0(fl.name,i-1), sep="\t", header=T,na.strings = "--", skip=1)
    dt[, timestep := i-1]
    dt_list[i] <- list(dt)
  }
  dt_sites2 <- rbindlist(dt_list)
}

#for more than one folder at a time:
'dt_sites2 <- list()
  for(j in 1:length(SNR)){
setwd(folder.path)
#site <- paste0("SNR",SNR[j],"SMR",SMR[j])
fl.name <- paste0(root.name,"_det_")
dt_list <- list()
dt <- data.table()
yr <- yr
for(i in 1:yr){
dt <- fread(paste0(fl.name,i), sep="\t", header=T,na.strings = "--", skip=1)
timestep <- i
dt[, timestep := i]
dt_list[i] <- list(dt)
}
dt_sites2[[j]] <- rbindlist(dt_list)
}'

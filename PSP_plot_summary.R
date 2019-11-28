#Tree.Map.To.Add.As.Text <- 1
Max.Seedling.Hgt.m <- rep(1.35, length(SORTIE.species))
Init.Dens.Seedling.Hgt.Class.1 <- rep(0, length(SORTIE.species))
Init.Dens.Seedling.Hgt.Class.2 <- rep(0, length(SORTIE.species))
Init.Dens.Seedling.Hgt.Class.3 <- rep(0, length(SORTIE.species))
Init.Dens.Seedling <- rep(0, length(SORTIE.species))
sizeClasses <- seq(4,80, by =2)
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
SORTIE.tree.dat <- rbind(Min.Adult.DBH,Max.Seedling.Hgt.m,Init.Dens.Seedling.Hgt.Class.1,Init.Dens.Seedling.Hgt.Class.2,Init.Dens.Seedling.Hgt.Class.3,Init.Dens.Seedling,init.values)
colnames(SORTIE.tree.dat)<-SORTIE.species

#Import sample and then tree data
dat.type <- c("Sample","Trees")
tsas <- c("03","12","14","20","24","26")
read.list <- list()
dat.list <- list()
for(i in 1:length(dat.type)){
  setwd(paste0(r.path,dat.type[i]))
  for(j in 1:length(tsas)){
    read.list[[j]]<- fread(paste0("TSA",tsas[j],".csv"))
  }
  dat.list[[i]] <- rbindlist(read.list)
}
samples.dt <- (dat.list[[1]])
tree.dat <- dat.list[[2]]

#convert to species id
#I need to decide when to clean the species data, but because I'm just making some runs for a plot that I know, I'm going to change SW to SX here for now
#unique(tree.dat[,species])
tree.dat[,sp_PSP:=ifelse(species=="SW","SX",species)]

PSP.species <- c("SX","PL","BL","AT")
SORTIE.species <- c("Interior_Spruce","Lodgepole_Pine","Subalpine_Fir","Trembling_Aspen")
tree.conv.table <- data.table(cbind(PSP.species,SORTIE.species))
num.meas <- length(unique(tree.dat[samp_id==plot.SORTIE,meas_no]))

#This assumes the min phf is the main plot
SORTIE.tree.dat.list <- list()
Plot.ReMeas.list <- list()
for(i in 1:num.meas){
  plot.SORTIE.meas <- tree.dat[samp_id==plot.SORTIE & meas_no==(i-1)]
  plot.SORTIE.meas[,LD_Group:=ifelse(ld=="L",1,ifelse(ld=="I",1,ifelse(ld=="V",1,2)))]
  red.plot.SORTIE.meas <- plot.SORTIE.meas[,.(samp_id,meas_yr,phf_tree,sp_PSP,dbh,ld,LD_Group,age_bh,height)]
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

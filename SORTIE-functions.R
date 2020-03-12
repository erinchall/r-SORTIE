
# Importing PSP data ------------------------------------------------------

#Import sample and then tree data
import.psp <- function(r.path, dat.type, tsas){
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


# Clean species labels ----------------------------------------------------
clean.sp.labels <- function(tree.dat){
  tree.dat[,sp_PSP:=ifelse(species=="SW","SX",
                           ifelse(species=="S","SX",
                                  ifelse(species=="B","BL",
                                         ifelse(species =="DM","D",
                                                ifelse(species=="DR","D",
                                                       ifelse(species=="AC","AT",
                                                              species))))))]
  return(tree.dat)
}

# Selecting and cleaning PSP data -----------------------------------------
sel.psp <- function(samples.dt,tree.dat,BECzone,BEClabel,SiteSeriesOfInterest,MinRemeasInterval){
  # Remove repeats (which I think represent sub-plots)
  uni.samples.dt<-unique(samples.dt, by="SAMP_ID")
  #create the list of criteria needed to determine whether a plot should be included. This assumes that coding is consistent 
  if(!is.null(BECzone)){criteria.samples <- uni.samples.dt[bgc_zone ==BECzone & bgc_ss_grd>0] #02
  } else{criteria.samples <- uni.samples.dt[beclabel_grd == BEClabel & bgc_ss_grd>0]} #05/06
  remeas.samples <- criteria.samples[(criteria.samples[,meas_yr_first]!=criteria.samples[,meas_yr_last])]
  remeas.samples <- remeas.samples[tot_period>=MinRemeasInterval & treatment != "THINNED" & stnd_org!="P"]
  plot.SORTIE <- unique(remeas.samples[bgc_ss_grd==SiteSeriesOfInterest]$SAMP_ID)
  
  #remove plots based on composition: actually just need to remove from plotID
  # c("XC","CW")
  #plot.SORTIE <- tree.dat[samp_id %in% ]
  #setkey(tree.dat)
  #remove plots based on composition: actually just need to remove from plotID
  #rm.plot <- unique(tree.dat[which(tree.dat[,species==.("XC","CW")]),.(samp_id)])
  #tree.dat <- tree.dat[!rm.plot]
  
  return(plot.SORTIE)
}


# Calculate number of years and age for a PSP run -------------------------
psp.years.age <- function(plot.SORTIE,tree.dat,samples.dt,age.crit){
  #make the right output for print functions
  psp.dets <- list()
  sp_comp_list <-list()
  #num.meas <- vector()
  #main.plot.phf <- vector()
  #run_years <- vector()
  for(i in 1:length(plot.SORTIE)){
    num.meas <- length(unique(tree.dat[samp_id==plot.SORTIE[i],meas_no]))
    for(j in 1:num.meas){
      main.plot.phf[j] <- min(na.omit(tree.dat[samp_id==plot.SORTIE[i] & meas_no==(j-1)]$phf_tree))
    }
    plots.each.meas[[i]] <- main.plot.phf
    #main.plot.phf <- min(na.omit(tree.dat[samp_id==plot.SORTIE[i]]$phf_tree))
    sp_comp <- table(tree.dat[samp_id==plot.SORTIE[i],sp_PSP])
    #unique(samples.dt[SAMP_ID==plot.SORTIE[i]]$beclabel_grd)
    age <- max(na.omit(tree.dat[samp_id==plot.SORTIE[i]&meas_no==0]$age_tot))
    run_years <- max(samples.dt[SAMP_ID==plot.SORTIE[i],meas_yr])-min(samples.dt[SAMP_ID==plot.SORTIE[i],meas_yr])
    psp.dets$plotid[i] <- plot.SORTIE[i]
    #psp.dets$main.plot.phf[i] <- plots.each.meas[[i]]
    psp.dets$num.meas[i] <- num.meas
    psp.dets$run.years[i] <- run_years
    psp.dets$age[i] <- age
    sp_comp_list[[i]] <- sp_comp
  }
  psp.dets[[6]] <- sp_comp_list
  #psp.dets[[7]] <- plots.each.meas
  return(psp.dets)
  #return(num.meas)
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

#  Summarize PSP data ---------------------------------------------------------------
#This assumes the min phf is the main plot
psp.meas <- function(tree.dat,study.plots,num.meas){
  Plot.ReMeas.list <- list()
  for(i in 1:num.meas){
    study.plots.meas <- tree.dat[samp_id==study.plots & meas_no==(i-1)]
    study.plots.meas[,LD_Group:=ifelse(ld=="L",1,ifelse(ld=="I",1,ifelse(ld=="V",1,2)))]
    red.study.plots.meas <- study.plots.meas[,.(samp_id,tree_no,meas_yr,meas_no,phf_tree,sp_PSP,dbh,ld,
                                                LD_Group,age_tot,height,batree,baha,volwsv,volcu10m,
                                                volcu15m,wsvha,gmv10ha,gmv15ha,nmv10ha,nmv15ha)]
    #just live species
    main.plot.phf <- min(na.omit(tree.dat[samp_id==study.plots & meas_no==(i-1)]$phf_tree))
    ld.red.study.plots.meas <- red.study.plots.meas[LD_Group==1 & phf_tree==main.plot.phf]
    Plot.ReMeas.list[[i]] <- red.study.plots.meas[phf_tree==main.plot.phf]
  }
  Plot.ReMeas.tab <- rbindlist(Plot.ReMeas.list)
  return(Plot.ReMeas.tab)
}

# Stems/ha defined by SORTIE DBH classes ----------------------------------
create.SORTIE.DBH.classes <- function(sizeClasses,SORTIE.tree.dat,all.meas.plot.SORTIE, main.plot.phf){
  meas.no <- unique(all.meas.plot.SORTIE[,meas_no])
  SORTIE.tree.dat.list <- list()
  for(i in 1: length(meas.no)){
    yr.meas.sortis <- all.meas.plot.SORTIE[meas_no==i-1]
    
    for(j in 1:length(sizeClasses)){
      yr.meas.sortis[dbh <= sizeClasses[j] & dbh > sizeClasses[j]-2,DBH_bin := j]
    }
    tree.per.bin <- yr.meas.sortis[,.N, by=.(DBH_bin,sp_PSP)] 
    tree.per.bin[,Trees.per.ha := N*main.plot.phf]
    setkey(tree.per.bin,sp_PSP,DBH_bin)
    
    SORTIE.tree.dat.list[[i]] <- SORTIE.tree.dat
    
    for(k in 1:nrow(tree.per.bin)){
      SORTIE.tree.dat.list[[i]][6+tree.per.bin[k,DBH_bin],tree.conv.table[PSP.species==tree.per.bin[k,sp_PSP],SORTIE.species]] <- tree.per.bin[k,Trees.per.ha]
    }
  }
  return(SORTIE.tree.dat.list)
}




# Stems/ha defined by SORTIE DBH classes ----------------------------------
#(should modify to allow flexbility in live, dead etc.)
old.create.SORTIE.DBH.classes <- function(tree.dat,num.meas,sizeClasses,SORTIE.tree.dat,plot.SORTIE){
  #out.list <- list()
  #for(r in 1:length(plot.SORTIE)){
    SORTIE.tree.dat.list <- list()
    Plot.ReMeas.MainPlot.list <- list()
    Plot.ReMeas.list <- list()
    red.plot.SORTIE.meas.list <- list()
    #ind.num <- num.meas[r]
    for(i in 1:num.meas){
      plot.SORTIE.meas <- tree.dat[samp_id==plot.SORTIE & meas_no==(i-1)]
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
    return(SORTIE.tree.dat.list)
  #}
  #return(out.list)
}

# Import SORTIE outputs ---------------------------------------------------
#should change the import function allow flexibility of a numeric value (all years) or a vector of specific years
import.SORTIE.output <- function(yr,folder.path,fl.name){
  dt_sites2 <- list()
  setwd(folder.path)
  dt_list <- list()
  dt <- data.table()
  yr <- yr
  for(i in 1:(yr+1)){
    dt <- fread(paste0(fl.name,i-1), sep="\t", header=T,na.strings = "--", skip=1)
    dt[, timestep := i-1]
    dt_list[i] <- list(dt)
  }
  dt_sites2 <- rbindlist(dt_list)
  return(dt_sites2)
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



# PSP species clean-up ----------------------------------------------------
#write this out to make a change species label - NOT DONE!
#sp.table <- data.table(Old=c("SW","P"),New=c("SX","PL"))
#psp.dat[[2]][,sp_PSP:=ifelse(species==sp.table[,Old],sp.table[,New],species)]


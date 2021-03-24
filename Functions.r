#This file contains all the functions that are needed
library("data.table")

FindFileLine <- function(rf,itype, varname, vargroup, varmaster) {
  #This routine finds the right line in the file
  ln1 <- 0
  lng <- 0
  
 
  if (itype==1 || itype == 2 || itype == 5) {
    #Basic find: the variable is unique
    ln1 <- grep(varname,rf)
    
  } else if (itype == 3 || itype == 4) {  #behaviour list parameter sections
    #need to find two levels: first the MasterGroup (eg QuadratLight1)
    #and then within that, the subgroup, if necessary (eg gr_nciMaxPotentialGrowth)
    #need to find the line of the master group first 
    lnm <- grep(varmaster,rf)     #this should return two values: the beginning and end of the group
    if (length(lnm)>0) {
      if (itype==3) {             #unique line within this group
        #print(paste(varmaster, varname, length(lnm)))
        if (length(lnm) >1 ) {
          lng2 <- grep(varname,rf[lnm[1]:lnm[2]])  #this returns the line number within this group
        } else {                                  #this only was found in the opening line (like for grids)
          lng2 <- grep(varname,rf[lnm[1]:length(rf)])  #so find the variable in all places after the opening. There might be several
        }
        ln1 <- lnm[1]+lng2-1    # to get the overall line number  (note,if there are sevearl with that varname, we just use the first instance)
      } else {
        lng <- grep(vargroup,rf[lnm[1]:lnm[2]])   #this should return two values: the beginning and end of the subgroup
        if (length(lng)>0) {
            stline <- lng[1] + lnm[1]-1          #need to translate the line number within the group to full file line number
            endline <- lng[2] + lnm[1] -1 
            lng2 <- grep(varname,rf[stline:endline])  #this returns the line number within this group
          ln1 <- stline+lng2-1    # to get the overall line number
        } else {
          print(paste("Variable Group:", vargroup, "not found."))
        }
      }
    } else {
      print(paste("Master Variable:", varmaster, "not found."))
    }
  } else if (itype == 6) {   #Initial Density section 
    #For this type, we need to find the section with the right species.
    #So we need to first add in the species (passed in as the master group for convenience)
    fullname <- paste(vargroup," whatSpecies=",varmaster,sep="")
    lnm <- grep(fullname,rf)     #this should return  the beginning of the group
    lne <- lnm + min(grep(vargroup,rf[lnm+1:length(rf)]))    #ending line
    if (length(lnm)>0) {
      tempRF <-gsub("\"","",rf[lnm[1]:lne])   #This makes a copy of the search area without the " marks
      lng2 <- grep(varname,tempRF)  #this returns the line number within this group 
      if (length(lng2)>0) {
        ln1 <- lnm[1]+lng2[1]-1               # to get the overall line number
      }
    } else {
      print(paste("Variable Group 6:", fullname, "not found."))
    }
    
  } else if (itype == 7) {   #Harvest
    #For this type, we need to find right instance of the item.
    #The variable vargroup will contain the which instance we want (e.g.  2)
    lnm <- grep(varmaster,rf)     #this should return  the beginning of the group
    lne <- lnm + min(grep(varmaster,rf[lnm+1:length(rf)]))    #ending line
    if (length(lnm)>0) {
      lng2 <- grep(varname,rf[lnm[1]:lne[1]])  #this returns the line numbers within this group 
      if (length(lng2)>0) {
        ln1 <- lnm[1]+lng2-1               # to get the overall line numbers
      }
      #print(paste("group line:", lnm, "start line: ", lng2))
    } else {
      print(paste("Variable Group 7:", varmaster, "not found."))
    }
    
  } else {
    print(paste("Variable,", varname, "with type:", itype, "is not a known type."))
  }

  return(ln1)
}

ReplaceParameter <- function(ln1, rf, varvalue) {
  #this routine works for cases where the xml file has the format:
  #   <timesteps>10</timesteps>
  #We assume the tag (e.g., timesteps) and its new value have been given.
  
  #find the line number for the first thing in the file (eg timesteps)
  # ln1 <- grep(pf1[i,ncol],rf)
  
  #Now, the line number has been passed in, along with the variable value
  
  #Find the characters just before the start and end of the value
  st_start <- str_locate(rf[ln1],">")
  st_end <- str_locate(rf[ln1],"</")
  
  #print(paste(ln1, varvalue, st_start, st_end))
  #print(paste(rf[ln1],substr(rf[ln1],st_start[1]+1,st_end[1]-1)))
  
  #and replace the value
  #newln <- str_replace(rf[ln1],substr(rf[ln1],st_start[1]+1,st_end[1]-1),as.character(varvalue))
  newln <- str_replace(rf[ln1],paste0(">",substr(rf[ln1],st_start[1]+1,st_end[1]-1),"<"), paste0(">",as.character(varvalue),"<"))
  rf[ln1] <- newln
  return(rf)
}

RemoveSpecies <-function(sp,rf) {
  #example code to remove all information about a species from the file
  #NOTE THIS WILL NOT WORK FOR HARVEST OR OTHER TYPES WHERE SPECIES IS ON A DIFFERENT LINE
  #                 
  sprows <- grep(sp, rf)
  rfnb <- rf[-sprows]
  
}

RemoveRow <-function(ln1, rf) {
  #ln1 is the row to remove
  #rf is the file
  rfnb <- rf[-c(ln1[1]:ln1[2])]
  
}

PrepareFile <-function(pfname) {
  
  #read the file with the new parameters
  #   note: when it reads as a csv the " marks are left as is
  #   note: when read as a Line the " marks are marked as \".
  #         this is necessary because the original xml file is read as Lines
  #print(paste("PrepareFile",pfname))
  #con <- open(as.character(pfname), r)
  #tempf1 <- readLines(pfname)
  tempf1 <- readLines(as.character(pfname))
  
  #determine the number of species by counting the number of commas in the first line
  ncols <- str_count(tempf1[1], ",")

  if (ncols>0) {  
    #strip the " from this file
    tempf <- gsub("\"","",tempf1)
    
    #but we need them around the species, so put those back
#TEST    tempf[1] <- tempf1[1]

    #The first line will be a header that has the species names, which must be in " and the same as in the xml file
    pf1 <- str_split_fixed(tempf, ",", n=ncols+1)
    
#TEST
    pf1[1,] <- paste0("\"",pf1[1,],"\"")
    
  } else {    #if there are no commas in the first line, we will assume that it is an xml section to insert
    tempf <-tempf1
    pf1 <- tempf
  }
    
  return(pf1)
}

ModifyFile <-function(paramFile, xml1) {
  pf1 <- PrepareFile(paramFile)
  #print("In Modify File")
  if (!is.null(ncol(pf1))) {#usual file type with variables on the lines and values in columns
    ncols <- ncol(pf1)-1
    #print("calling ReplaceInfo")
    xml2 <- ReplaceInfo(xml1, VariableNames, pf1, ncols, newname)
  } else { #there are no columns here so we will assume it is a .xml chunk
    #Because we don't know how the xml file was created, we will read and write it to make sure it is in line format
    #print("Ready to read_xml")
    #Two problems: 1) we need to change the filename to a string (rather than the element of a list)
    #2) (bigger) read_xml only works on a complete xml file. If we have more than one xml chunk inside the file,
    #           it will not be in the full proper format. So, the user MUST have the file already in line format.
    #p2 <- read_xml(toString(paramFile))
    p2 <- NULL
    try(p2 <- read_xml(toString(paramFile)),silent=TRUE)
    if (!is.null(p2)) {
      write_xml(p2, "p2.xml",options=c("no_declaration","format"))    #Note, we have now removed the extra line.
      p2 <- readLines("p2.xml", encoding="UTF-8")  
    } else {
      p2<- readLines(toString(paramFile), encoding="UTF-8")
    }
    pf1 <- gsub("\\\\", "//",p2)
    
    xml2 <- ReplaceLines(xml1, pf1)
    #print(paste("xml2 ",length(xml2)))
  }
  return(xml2)
}


RunSortie <-function(fname, sortie_loc) {
  #This function could be called as a stand-alone and may not be run with files created by the R scripts
  #So, we need to read the given xml, and write it again to put in the missing line breaks.
  res <- read_xml(fname)
  write_xml(res, "temp_run.xml")
  
  if (sortie_loc==0) {
    cmd=paste0("\"C:\\Program Files (x86)\\SORTIE\\bin\\coremodel.exe\" ","temp_run.xml")
  } else {
    cmd=paste0("\"",sortie_loc,"\" temp_run.xml")
  }
  write(cmd, file="runSortie.bat")
  
  system("runSortie.bat")
  
}

ExtractFiles <- function(itype,exname) {  #used for .gz.tar files - e.g., trees
  #pass in the directory that contains one or more tar files.
  #This program will then read the directory and extract all files from any tar files that are present
  #It also assumes that all the files in a single tar file were in the same directory. It looks at the
  #first file in the tar, determines how many subdirectories are present, and then strips all those subdirectories.
  #The extracted files will be placed in a new extracted directory. If you want to put them
  # somewhere else, then change the variable extractDir
  #The routine returns a list of the extracted files.
  write("", file="rungzip.bat")
  write("", file="runtar.bat")
  outdir <- exname
  extractDir <- paste0(outdir,"extracted")  #directory that will contain the extracted files
  dir.create(extractDir,showWarnings=FALSE)  #make the directory if it doesn't already exist
  
  if (itype != 1) {  #extract all the tar files in the directory
    FileList <- list.files(outdir,pattern="*.tar")
    for (ix in 1:length(FileList)) { 
      #first get a list of the files and find out how many directory levels down they are. Just check the first file.
      ndir <- str_count(untar(paste0(outdir,FileList[ix]), compressed = TRUE, list=TRUE),pattern="/")
      #untar(paste0(outdir,FileList[ix]),exdir=extractDir, compressed = TRUE, extras=paste0("--strip-components ",ndir[1]))
      #untar(paste0(outdir,FileList[ix]),exdir=extractDir, compressed = TRUE)
      cmd <-paste0("tar -xf \"",outdir,FileList[ix],"\""," --strip-components=",ndir," -C ",extractDir)
      write(cmd, file="runtar.bat", append=FALSE)
      system("runtar.bat")
      
      
      FileList2 <- list.files(extractDir,pattern="*.gz",recursive=TRUE)
      for (ix2 in 1:length(FileList2)) { 
        cmd<-paste0("gzip -d \"",extractDir,"/",FileList2[ix2],"\"")
        write(cmd, file="rungzip.bat", append=TRUE)
      }
    }
  } else {
    #THIS SECTION DOESN"T WORK RIGHT NOW
    #one tar file only
    #extract only 1 tar file
    untar(exname, compressed = TRUE)
    
    #TODO: get a list of the untarred files and then send them to gzip below.
    cmd<-paste("gzip -d \"C:/Projects/SORTIE/output/extracted/Projects/SORTIE/output/orig-t2_1.xml.gz\"")
    #print(cmd)
    write(cmd, file="rungzip.bat")
  }
  system("rungzip.bat")
  FileList3 <- list.files(extractDir,pattern="*.xml",recursive=TRUE,full.names=TRUE)
  return(FileList3)
}

ReadPlotFile <- function(outdir) {
    dt <- data.table()
    FileList <- list.files(outdir,pattern="*.out")
    dt <- fread(paste0(outdir,FileList[1]), sep="\t", header=T,na.strings = "--", skip=5)
    #dt_table <- rbind(dt_table,dt)
    #return(dt_table)
    return(dt)

}

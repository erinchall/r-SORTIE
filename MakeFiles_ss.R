rm(list=ls())

#______  Apply Harvest patterns to SORTIE parameter files _________#
#         May 31, 2021  E. Hall

# Applies different MakeFiles depending on whether the BaseParameter file has MPB
# applied (changes the position of the harvest & plant xml chunks)

#library("xml2", lib.loc="C:/Program Files/R/R-3.6.0/library")
#library("stringr", lib.loc="C:/Program Files/R/R-3.6.0/library")
library("stringr")
library(xml2) 
library(dplyr)
library(tidyr)
source(file.path("sourcefiles", "Functions.r"))
source(file.path("sourcefiles", "ReplaceInfo2.r"))
source(file.path("sourcefiles", "ParseXML.R"))

print("April 28, 2021")

# Pathways
source_path <- file.path("sourcefiles")
xml_path <- file.path("BaseParameterFiles")
param_path <- file.path("ParameterValues")
out_path <- file.path("output")
list_path <- file.path("FileLists")
sortie_path <- file.path("OutputSortieParFiles")

#VARIABLES THAT DECIDE WHAT ELSE RUNS
bRunSortie <- 0     #0= do not run Sortie, 1= Run Sortie with newly made xml files
bExtract <- 0  #DO NOT USE - Not yet finalized. But the call might work...
bParseXML <- 0      #1=parse files. 
YearsToExtract <- c("1","5","10")  #example PUT YOUR YEARS HERE for which tree years to extract
sortie_loc = 0    #location of the Sortie executable. If 0, it is in the default location. (C:\\Program Files (x86)\\SORTIE\\bin\\coremodel.exe)
#otherwise, put a string with the location here. Use / instead of \

###############################################################
# STEP 1: Create parameter files with each harvest pattern
###############################################################

#Start processing
#read the list of files         NOTE: number of allowable file types = 5
for(ss_n in 1:2){ #Add Loop to apply different MakeFiles and .xml chunks depending whether mpbn or mpby
  lstFiles <- read.csv(file.path(list_path, paste0("files_ss", ss_n, ".txt")))

xmlList <- c()
paramList1 <- vector("list",5)
maxtype <- 0
for (i in 1:nrow(lstFiles)) {
  fn <- as.character(trimws(lstFiles$name[i]))
  itype <- lstFiles$type[i]
  
  if (itype == 0) {
    xmlList <- c(xmlList,fn)
  }
  else {
    paramList1[[itype]] <- c(paramList1[[itype]],list(fn))
  }
  if (itype > maxtype) {maxtype <- itype}
}
numtype <- c()
for (iii in 1:5) {
  numtype <- c(numtype,length(paramList1[[iii]]))
}
  
#Valid values of itype:
#           1=basic case: variable parameter is directly after the name
#           2=basic case with species: same, but with a species name after it
#           3=behaviorlist type: but basic parameter (like 1)
#           4=behaviorlist type: but with species (like 2)
#           5=output files: so parameter file will have a directory name
#           6=groups with species on the previous line (e.g. for initial density)
#col 1: input parameter (the name in Alana's file)
#col 2: type
#col 3: name in the line being replaced
#col 4: group name 
VariableNames <- read.csv(file.path(source_path, "VariableNames_pd.csv"), header=TRUE, strip.white = TRUE) # _pd refers to updated with plant density variable reference

ListOfFiles <- c()
for (ix in 1:length(xmlList)) { #start loop over xml files
 
  #read the given xml
  res <- read_xml(file.path(xml_path, xmlList[ix]))
  #write the xml to a file again (this will put in the missing line breaks)
  write_xml(res, "temp.xml")
  
  #read the newly printed file, this time as lines of text
  tmp <- readLines("temp.xml", encoding="UTF-8")
  xml1 <- gsub("\\\\", "//",tmp)    #reverse the slash marks
  
  #make a vector that contains the length of each file type
  for (ip in 1:numtype[1]) {
    for (ip2 in 1:max(1,numtype[2])) {
      for (ip3 in 1:max(1,numtype[3])) {
        for (ip4 in 1:max(1,numtype[4])) {
          for (ip5 in 1:max(1,numtype[5])) {
            ip_vals <- c(ip,ip2,ip3,ip4,ip5)
            newname <- ""
            #newname <- paste(substr(xmlList[ix],1,nchar(xmlList[ix])-4),"-",substr(paramList1[[1]][ip],1,nchar(paramList1[[1]][ip])-4),sep="")
            newname <- paste(substr(xmlList[ix],1,2), substr(paramList1[[1]][ip],1,2), substr(xmlList[ix],9,12), substr(xmlList[ix],16,16), sep = "_") # Change the output file name to insert the harvest type only
            #newname <- paste(substr(xmlList[ix],1,nchar(xmlList[ix])-4),sep="")    #remove type 1 files from the naming
            
            #for each of the files, prepare it, and process it
            # note: we have to do all five files each time because we don't know which of the files might have the output directories (which need 'newname')

            for (iii in 1:5) {
              if (numtype[iii] > 0) {
                #print(paste("MakeFiles",iii,ip_vals[iii]))
                #print(paramList1[[iii]][ip_vals[iii]])
                xml2 <- ModifyFile(file.path(param_path, paramList1[[iii]][ip_vals[iii]]),xml1)
              } else {
                xml2 <- xml1
              }
              xml1 <- xml2
            } 
    
            xml2 <- gsub("//", "\\\\", xml2)    #turn any forward slashes into back into double backwards slashes
            #write the new file 
            newname <- paste(newname,".xml",sep="")
            #newname <- "erin/debug2.xml"
            writeLines(xml2,newname)
            ListOfFiles <- c(ListOfFiles, newname)    #store the newly created file in a list so it can be run automatically later
          }
        }
      }
    }
  } 
} 
}
 
# close MakeFiles loop for creating mpbn vs mpby parameter files (mpby has harvest and plant in positions 2 and 23 instead of 1 and 22 for mpbn base files)

###############################################################
# STEP 2: Apply appropriate species mix to each parameter file
###############################################################

#Start processing
#read the list of files         NOTE: number of allowable file types = 5

for(in_n in 1:5){ #Add Loop to apply different MakeFiles and .xml chunks depending the initialization/stand type
  lstFiles <- read.csv(file.path(list_path, paste0("files_ss_in", in_n, ".txt")))
  
  xmlList <- c()
  paramList1 <- vector("list",5)
  maxtype <- 0
  for (i in 1:nrow(lstFiles)) {
    fn <- as.character(trimws(lstFiles$name[i]))
    itype <- lstFiles$type[i]
    
    #if (itype == 0) {
      xmlList <- list.files(pattern = paste0("_", in_n, ".xml"))
    #}
    #else {
      paramList1[[itype]] <- c(paramList1[[itype]],list(fn))
    #}
    if (itype > maxtype) {maxtype <- itype}
  }
  numtype <- c()
  for (iii in 1:5) {
    numtype <- c(numtype,length(paramList1[[iii]]))
  }
  
  #Valid values of itype:
  #           1=basic case: variable parameter is directly after the name
  #           2=basic case with species: same, but with a species name after it
  #           3=behaviorlist type: but basic parameter (like 1)
  #           4=behaviorlist type: but with species (like 2)
  #           5=output files: so parameter file will have a directory name
  #           6=groups with species on the previous line (e.g. for initial density)
  #col 1: input parameter (the name in Alana's file)
  #col 2: type
  #col 3: name in the line being replaced
  #col 4: group name 
  VariableNames <- read.csv(file.path(source_path, "VariableNames_pd.csv"), header=TRUE, strip.white = TRUE) # _pd refers to updated with plant density variable reference
  
  ListOfParFiles <- c()
  for (ix in 1:length(xmlList)) { #start loop over xml files
    
    #read the given xml
    res <- read_xml(file.path(xmlList[ix]))
    #write the xml to a file again (this will put in the missing line breaks)
    write_xml(res, "temp.xml")
    
    #read the newly printed file, this time as lines of text
    tmp <- readLines("temp.xml", encoding="UTF-8")
    xml1 <- gsub("\\\\", "//",tmp)    #reverse the slash marks
    
    #make a vector that contains the length of each file type
    for (ip in 1:numtype[1]) {
      for (ip2 in 1:max(1,numtype[2])) {
        for (ip3 in 1:max(1,numtype[3])) {
          for (ip4 in 1:max(1,numtype[4])) {
            for (ip5 in 1:max(1,numtype[5])) {
              ip_vals <- c(ip,ip2,ip3,ip4,ip5)
              newname <- ""
              #newname <- paste(substr(xmlList[ix],1,nchar(xmlList[ix])-4),"-",substr(paramList1[[1]][ip],1,nchar(paramList1[[1]][ip])-4),sep="")
              newname <- paste0(substr(xmlList[ix],1,10), "_in", in_n) # Change the output file name to insert the harvest type only
              #newname <- paste(substr(xmlList[ix],1,nchar(xmlList[ix])-4),sep="")    #remove type 1 files from the naming
              
              #for each of the files, prepare it, and process it
              # note: we have to do all five files each time because we don't know which of the files might have the output directories (which need 'newname')
              
              for (iii in 1:5) {
                if (numtype[iii] > 0) {
                  #print(paste("MakeFiles",iii,ip_vals[iii]))
                  #print(paramList1[[iii]][ip_vals[iii]])
                  xml2 <- ModifyFile(file.path(param_path, paramList1[[iii]][ip_vals[iii]]),xml1)
                } else {
                  xml2 <- xml1
                }
                xml1 <- xml2
              } 
              
              xml2 <- gsub("//", "\\\\", xml2)    #turn any forward slashes into back into double backwards slashes
              #write the new file 
              newname <- paste(newname,".xml",sep="")
              #newname <- "erin/debug2.xml"
              writeLines(xml2,newname)
              ListOfFiles <- c(ListOfFiles, newname)    #store the newly created file in a list so it can be run automatically later
            }
          }
        }
      }
    }  
  }
} # close MakeFiles loop for adding planting specs (species %) 


###############################################
#   Clean up the root directory
# Move parameter files into SORTIE Parameter File directory
###############################################

# Remove mid-processing files
listing <- list.files()  
filesDelete <- listing[grep("_[0-9].xml", listing )]    # all preliminary files
file.remove(filesDelete)

# Move final Sortie Parameter Files to self-contained Directory
files <- list.files(pattern="*_in*", recursive=FALSE)
file.copy(from=files, to=sortie_path, 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)
file.remove(files)

###############################################
#Run the model with these files
   #Format: RunSortie(filename_to_run)
###############################################
if (bRunSortie == 1) {
  for (ix in 1:length(ListOfFiles)) { 
      RunSortie(ListOfFiles[ix],0)    #the second value is the run
  }
}

###############################################
###############################################
if (bExtract == 1) {
  #call a routine to extract all the files from a gz.tar file. 
  #Routine will return a list of the extracted files.
  
  #tell it whether to extract a single file (1) or a directory (0)
  # and then pass it the directory name and filename (if desired)
  onefile <- ""       #Put a filename here if you want to extract only a single file instead of a directory
  
  OutputDir <- "C:/Projects/SORTIE/output/"  #TODO: get this output directory from the input file.
  #NOTE: ListOfExtractedFiles will be NULL if OutputDir exists.
  ListofExtractedFiles <- ExtractFiles(0, OutputDir, onefile)
}

###############################################
###############################################
if (bParseXML ==1) {
  #give this a TREE OUTPUT xml file to parse or a list of files

  #pass it just one file:
  
  #output_df[1] <- ParseXML("C:/Projects/SORTIE/output/extracted//Projects/SORTIE/output/October14s_1.xml")
  
  #OR pass it multiple files 
    #YearsToExtract <- c("1","5","10")  #example PUT YOUR YEARS HERE
    p <- paste0("_",YearsToExtract,".xml",collapse="|")
    r <- grepl(p,ListofExtractedFiles)  #this returns a true/false list of what to extract
    
    ListofExtractedFiles <- c("Alana/ICH-A4-p_det_9.xml.gz")
    r <- c(TRUE)
     for (ix in 1:length(ListofExtractedFiles)) { 
       if (r[ix]){
         #print(ListofExtractedFiles[ix])
         #output_df[ix] <- ParseXML(ListofExtractedFiles[ix])
         tmp <- ParseXML(ListofExtractedFiles[ix])
         assign (paste0("trees_",ix), tmp)  #this puts the tree information into a variable named eg trees_1.
         
      }
     }
}

###############################################
###############################################
if (bParseMaps == 1) {
  #Read a detailed output file to get the different maps that are in it.
  #loop over the maps to put their information into a file with points, x, y, and values (which vary by map type)
  #TODO: loop over the output to read in multiple files
  
  #NOTE: right now, just manually give this a filename.
    dat <- read_xml("Alana/ICH-A4-p_det_9.xml.gz")
  
  #Find a list of the different maps (because there can be more than one in the file).
  grid_loc <- xml_find_all(dat, ".//grid")
  
  #read the map output for the different maps
  #This will make a dataframe that is called map_1 or map_2.
  #The list output_type will give what type of map this is  (e.g., Dispersed Seeds or GLI Map 1)
  #This is only for a single timestep, which is not yet specified anywhere.
  output_type <- list()
  for(i in 1:length(grid_loc)){
    tmp <- ParseMap(grid_loc[i])
    assign (paste0("map_",i), tmp)  #this puts the grid information into a variable named eg map_1.
    output_type[i] <- xml_attr(grid_loc[i],"gridName")  #this tells you what the map is.
  }
}

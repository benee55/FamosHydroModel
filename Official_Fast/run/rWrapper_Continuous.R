# RWrapper for Hydro Model




##################################################################
# Begin R Wrapper Function
##################################################################
##################################################################
# Function 1: Input File Text

writeInput<-function(par , # Parameters
                     j ,  # JobNumber
                     dir # Directory
                     ){
intervalMat<-c("2003060100","2007123100") # 2003/06/01-2007/12/31
  writeLabel<-c("ADD_PCTIM" ,"ADD_ADIMP" , "ADD_UZTWM" , "ADD_LZTWM" , 
                "ADD_LZFSM" , "ADD_LZFPM" , "ADD_LZSK" , "ADD_snow_SCF" , 
                "ADD_REXP" , "ADD_UZK" , "ADD_Q0CHN" , "ADD_QMCHN")

# Replace placeholders with values
readInputText  <- readLines(paste(dir,"/calsnow_Ben_template.card",sep="")) # Read Template
readInputText  <- gsub(pattern = "ADD_output", replace = paste("output",j,sep=""), # Replace output directory
                       x = readInputText)

  for(kk in 1:length(par)){
    readInputText  <- gsub(pattern = writeLabel[kk], replace = par[kk], x = readInputText) # Replace parameters
  }

readInputText  <- gsub(pattern = "ADD_DSTART", replace = intervalMat[1], # Replace Time Start
                       x = readInputText)
readInputText  <- gsub(pattern = "ADD_DEND", replace =intervalMat[2], # Replace Time End
                       x = readInputText)

  writeLines(readInputText, con=paste(dir,"/input",j,".card",sep="")) # Write Calsnow file
}

# Function 2: Delete Directory and Create
writeOutput<- function( j ,  # Job Number
                        dir # Directory
                       ){
newOutputDirectory<-paste(dir,"/output",j,sep="")
system(paste("rm -rf ",newOutputDirectory,";","mkdir ",newOutputDirectory , sep=""))
}

# Function 3: Run Model + Save FIle
runHydroModel<- function(j,dir){
newInputFile<-paste(dir,"/input",j,".card",sep="")
system(paste("/gpfs/group/kzk10/default/private/hydrocalib/SGrove/bin/rdhm ",newInputFile))
}


# Function 4: Read OutputFile
readOutput<-function(j,dir){
  # Dates evaluated within each interval
  diffInd<- #ADD NUMBER#
  output<-system(paste("cat ",dir,"/output",j,"/SBYP1_discharge_outlet.ts",sep=""), intern=TRUE)
  endInd<-length(output) # Row of final date
  startInd<-endInd-diffInd[interval]+1 # Row of first date
  flow<-as.numeric(unlist(lapply(output[startInd:endInd],substr,start = 22 , stop = 35))) #Streamflow for specified dates
  dates<-unlist(lapply(output[startInd:endInd],substr,start = 11 , stop = 16)) #Streamflow for specified dates
  useDate<-list(c("190904","200904"),
                c("150105","160105", "300305" , "310305" , "030405" , "040405" ,"050405" , "060405"),
                c("011205"),
                c("280606" , "290606" , "300606"),
                c("181106"),
                c("160307" , "170307"),
                c("080208" , "060308" , "090308" , "100308"))
  flow[which(dates%in%useDate[[interval]])]
}



# Combine Run File + Read
modelEval<-function( par, j , inputDir , outputDir){
  pt<-proc.time()
  for(i in 1:7){
    writeInput( par = par[-1] , j = j , interval = i , dir = inputDir)  # Write Input
    writeOutput( j = j , interval = i , dir = outputDir) # Write Output
    runHydroModel( j = j , interval = i , dir = inputDir) # Run Model
    
    if(i==1){ # Read model output - First
      output<-readOutput( j = j , interval=i, dir = outputDir)
    }else{  # Read model output - Next
      output<-c(output,readOutput( j = j , interval=i, dir = outputDir))
      }
  }
  ptFinal<-proc.time()-pt
  output<-c(output,ptFinal[3])
  return(output) # Return output
}
  


####################################################################################################
## FOr Debugging
####################################################################################################
# Combine Run File + Read
modelEval_only<-function( par, j , inputDir , outputDir){
  for(i in 1:7){
    writeInput( par = par[-1] , j = j , interval = i , dir = inputDir)  # Write Input
    writeOutput( j = j , interval = i , dir = outputDir) # Write Output
    runHydroModel( j = j , interval = i , dir = inputDir) # Run Model
  }
}

modelEval_read<-function( par, j , inputDir , outputDir){
  for(i in 1:7){
    
    if(i==1){ # Read model output - First
      output<-readOutput( j = j , interval=i, dir = outputDir)
    }else{  # Read model output - Next
      output<-c(output,readOutput( j = j , interval=i, dir = outputDir))
    }
  }
  return(output) # Return output
}


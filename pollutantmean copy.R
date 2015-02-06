pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files
	## 'pollutant' is a character vector of length 1 indicating
	## the name of the pollutant for which we will calculate the
	## mean; either "sulfate" or "nitrate".
	## 'id' is an integer vector indicating the monitor ID numbers
	## to be used
	## Return the mean of the pollutant across all monitors list
	## in the 'id' vector (ignoring NA values)
	##   structure date, sulphate, nitrate, ID
	## For a single file - this works
	##
	## x <- read.csv("~/Documents/Coursera/Data Science/Data Science 2 - R Programming/hw1_data.csv")
	##
	## For multiple files use the following:
	##
	## Set the working directory
	 basedir <- "/Users/bob/Documents/Coursera/Data Science/Data Science 2 - R Programming/"
 	
   datadir <- paste(basedir,directory,sep="")
  
	## - sma = sum, smb=mean and smc=count for pollutant 1
   
  sm <- c(id)
  sma <- c(id) 
  smb <- c(id)
  smc <- c(id)
  
	sme <- c(id) 
	smf <- c(id)
	smg <- c(id)
  
  	 setwd(datadir)
  
	##
	
 	 filenames <- list.files(path=datadir)
  
	##
	## if a single file - this works. The read also works for multiple files.
	##
	## for (i in c(id)){ 
	##     dm <- read.csv(filenames[i], header=TRUE)
	## } 
	##        remember the header line - otherwise it forces the data format
	##
	## CM<-colMeans(dm[,2:3],na.rm=TRUE)
	##
	## if multiple files - this works.
	## for (i in c(id)){ 
	##     dm[i] <- read.csv(filenames[i], header=TRUE)
	## } 
	##
	## The following function appends all the data into dm - for multiple csv files
	## this creates a single list - but its not easy to work on
	##
	## dm = apply(filenames, function(x){read.csv(file=x,header=TRUE)})
	##
	##	
	##	doesnt work on the list version CM[i] <- colMeans(dm[,2:3],na.rm=TRUE)
	##
	
	 if (pollutant=="sulfate") {pl <- 1}
	 else if (pollutant=="nitrate") { pl <- 2 }
     else { stop("Error - pollutant not recognized")}
  
	## else { stop("Error - pollutant not recognized") }
	 px <- pl +1 
	 for (i in c(id)){ 
	     dm <- read.csv(filenames[i], header=TRUE)
	     CS<-colSums(dm[,2:3],na.rm=TRUE)  
	     CP<-colMeans(dm[,2:3],na.rm=TRUE) 
	     
	     if (CS[1]==0) {CP[1] <- 0}
	     if (CS[2]==0) {CP[2] <- 0}
       
	     ## sulfates
	     sma[i] <- CS[1]
	     smb[i] <- CP[1]
	     if (sma[i]!=0) {
	         smc[i] <- round(sma[i]/smb[i])
	     } else {smc[i] <- 0}
       
	     ## sulfates
	     sme[i] <- CS[2]
	     smf[i] <- CP[2]
	     if (sme[i]!=0) {
	        smg[i] <- round(sme[i]/smf[i])
	     } else {smg[i] <- 0}
       
	 } 
	 
	## cx <- as.character(mx)

   ms <- sum(sma[id])
   mc <- sum(smc[id])
  
   mt <- sum(sme[id])
   mv <- sum(smg[id])
  
   if (ms!=0 && mc!=0) {
	    mx <- ms/mc
   } else  { mx <- 0}   
      
	 if (mt!=0 && mv!=0) {
     my <- mt/mv
	 } else  { my <- 0}  
  
  if (pl==1) { 
   print (mx)
  }
  
  if (pl==2) {
   print (my)
  }
  	 
	##	 print (pollutant) 	
  
  ## print ("Pollutant is ", pollutant, " with mean ", cx)
	
	## Use the same mean calculation
	
}



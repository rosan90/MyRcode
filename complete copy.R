complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
        
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
        
	## For a single file - this works
	##
	## x <- read.csv("~/Documents/Coursera/Data Science/Data Science 2 - R Programming/hw1_data.csv")
	##
	## For multiple files use the following:
	##
	## Set the working directory
	
	 basedir <- "/Users/bob/Documents/Coursera/Data Science/Data Science 2 - R Programming/"
 	
   datadir <- paste(basedir,directory,sep="")
	 
	 ## clever way of doing a count - sum(vector>0)
   
	 sm <- data.frame(c(sum(id>0)),2)
   colnames(sm) <- c("id", "nobs")

   setwd(datadir)
	
 	 filenames <- list.files(path=datadir)
	 c <- 1
	 for (i in c(id)){ 
	     dm <- read.csv(filenames[i], header=TRUE)
	     colnames(dm) <- c("date","sulfate", "nitrate","ID")
	     df <- dm[!is.na(dm$sulfate)&!is.na(dm$nitrate),]
	     dc <- nrow(df)
       sm[[c,1]] <- i
       sm[[c,2]] <- dc
       c <- c + 1
       
	     ##    print (sm[sm$nobs==34])
       
	     ##     print (paste(i, dc, sep=" "))
	 }
       
   setwd(basedir)
	 rc <- sm

}



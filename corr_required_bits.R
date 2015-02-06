corr <- function(directory, threshold = 0) {
    
   rc <- complete(directory)  
   filestouse <- subset(rc,rc[,2]>threshold)
    
	 if (nrow(filestouse)==0) { 
      cr <- numeric()
      return(cr) 
	 }
 
   op <- array(,3)

   basedir <- "/Users/bob/Documents/Coursera/Data Science/Data Science 2 - R Programming/"
	 datadir <- paste(basedir,directory,sep="")
	 setwd(datadir)
  
   count <- 1
  
	 for (i in filestouse[,1]) { 
     
     if (i<10) { filen <- paste("00", i,".csv", sep = "" ) } 
     if (i>9 & i< 100) { filen <- paste("0", i,".csv" , sep = "") }
     if (i>99) { filen <- paste(i,".csv" , sep = "") }
     
        dm <- read.csv(filen, header=TRUE)
        ttemp <- dm[!is.na(dm$sulfate)&!is.na(dm$nitrate),]
	      op[count] <- cor(as.vector(ttemp[,2]),as.vector(ttemp[,3]))
	      count <- count + 1
	   }
	res <- op
}





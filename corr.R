# Neal Gushue, 2014..
corr <- function(directory, threshold=0) {    
    # Be nice to the humans..  add the file system requiremnets to whatever variable was passed as directory
    directory <- paste("./", directory, sep="")
    directory <- paste(directory,"/", sep="")
    # find all files in the directory
    full_list <- list.files(directory) 
    # add proper file system requirements to each 
    proper_list <- paste(directory, full_list, sep="")
    # reuse my complete.r function to give a list of all complete observations, then mask to greater then the threshold
    # hardly efficient, but should do the trick.  
    obs <- complete(directory)$nobs
    obs.id <- complete(directory)$id
    obs.mask <- complete(directory)$nobs > threshold
    # build mylist of observations to work on
    mylist <- obs.id[obs.mask]
    # initialize my holder list
    holder <- matrix((length(mylist)),) # matrix is clumsy for this but..  
    ct <- 1 
    # go thru the list of elements and process
    for(n in mylist) {
        working_file <- read.table(proper_list[n], header=TRUE, sep=",")
        holder[ct] <- cor(working_file$sulfate, working_file$nitrate, use="complete.obs")
        ct <- ct + 1
    }
    # required output is 5 decimal places
    answer <- round(holder,5)
    return(answer)
}

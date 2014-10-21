# Neal Gushue, 2014  
complete <- function(directory, id = 1:332, ...) {
    # added ... for corr function.
    # Be nice to the humans..  add the file system requiremnets to whatever variable was passed as directory
    directory <- paste("./", directory, sep="")
    directory <- paste(directory,"/", sep="")
    full_list <- list.files(directory) 
    # add proper file system requirements to each 
    proper_list <- paste(directory, full_list, sep="")
    # initialize my holder list
    holder <- matrix(,(length(id))) # matrix is clumsy for this but..
    # setup counter for insertion loop, have to look at why the internal for loop control var didnt work.  
    ct <- 1 
    # go thru the list of elements and process
    for(n in id) {
        working_file <- read.table(proper_list[n], header=TRUE, sep=",")
        holder[ct] <- sum(complete.cases(working_file))
        ct <- ct + 1
    }
    answer <-data.frame(id = id, nobs = holder)
    return(answer)
}

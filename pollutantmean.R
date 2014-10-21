# Neal Gushue, 2014..
pollutantmean <- function(directory, pollutant, id = 1:332) {    
    # Be nice to the humans..  add the file system requiremnets to whatever variable was passed as directory
    directory <- paste("./", directory, sep="")
    directory <- paste(directory,"/", sep="")
    # find all files in the directory
    full_list <- list.files(directory) 
    # add proper file system requirements to each 
    proper_list <- paste(directory, full_list, sep="")
    # initialize my holder
    holder <- c()
    # go thru the list of elements and process
    for(n in id) {
        working_file <- read.table(proper_list[n], header=TRUE, sep=",")
        na_mask <- working_file[!is.na(working_file[, pollutant]), pollutant]
        holder <- c(holder, na_mask)
    }
    # required output is 3 decimal places
    answer <- round(mean(holder),3)
    return(answer)
}
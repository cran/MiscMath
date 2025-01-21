MaxRunLength <- function(x) {
     runlengths <- rle(x)
     RunLengths <- runlengths$lengths[runlengths$values==0]
     return(max(RunLengths))
}


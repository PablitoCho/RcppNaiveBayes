NaiveBayesTrain <- function(x){
    stopifnot(is.list(x[[1]]))
    ## FIXME ensure x[[1]][[]] is character
    for (i in 1: length(x))
    {
        for (j in 1:length(x[[i]]))
        {
            stopifnot(is.character(x[[i]][[j]]))
        }
    }
    .Call( "NaiveBayesTrain", x, PACKAGE = "RcppNaiveBayes" )
}

NaiveBayesPredict <- function(x,y){
    stopifnot(class(x) == "RcppNaiveBayes")
    ## only on observation
    if(is.character(y)) {
        .Call("NaiveBayesPredict", x, y, 0L, PACKAGE = "RcppNaiveBayes" )
    } else if (is.list(y) && length(y) > 0) {
            if(is.character(y[[1]]))
            .Call( "NaiveBayesPredict", x, y, 1L, PACKAGE = "RcppNaiveBayes" )
    } else {
        stop("Requires a list of string vectors or a string vector")
    }
}
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
    if(is.character(y))
        .Call("NaiveBayesPredict", x, y, PACKAGE = "RcppNaiveBayes" )
#     else if (is.list(y)) {
#         for (i in 1:length(y))
#             if(is.character(y))
#             .Call( "NaiveBayesPredict", x, y[[i]], PACKAGE = "RcppNaiveBayes" )
#     }
}
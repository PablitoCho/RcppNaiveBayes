##

NaiveBayesTrain <- function(x){
    if(is.list(x[[1]])) {
        for (i in 1: length(x))
        {
            for (j in 1:length(x[[i]]))
            {
                stopifnot(is.character(x[[i]][[j]]))
            }
        }
    }
    else
    {
        stopifnot(is.list(x))
        stopifnot(is.character(x[[1]]))
        x <- list(x)
    }
    .Call( "NaiveBayesTrain", x, PACKAGE = "RcppNaiveBayes" )
}

NaiveBayesPredict <- function(x,y){
    stopifnot(class(x) == "RcppNaiveBayesTrain")
    z <- list()
    if(is.character(y)){
        z <- .Call("NaiveBayesPredict", x, y, 0L, PACKAGE = "RcppNaiveBayes" )
    } else if (is.list(y) && length(y) > 0 && is.character(y[[1]])) {
        z <- .Call( "NaiveBayesPredict", x, y, 1L, PACKAGE = "RcppNaiveBayes" )
    } else {
        stop( "Requires a list of string vectors or a string vector" )
    }
    w <- list(scores=z,predicted=sapply(z, which.max))
    ## TODO confusion matrix
    attr (w, "class") <- "RcppNaiveBayesPredict";
    w
}

NaiveBayesConfusion <- function(x){
    ## stopifnot(class(x) == "RcppNaiveBayesPredict")
    ## x[[i]] <- NaiveBayesPredict(model,catedata[[i]])
    y <- length(x)
    M <- matrix(0,nrow=y,ncol=y)
    for (i in 1:y)
        M[i,] <- sapply(1:y, function(xx) sum(x[[i]]$predicted == xx))
    M
}

NaiveBayesUpdate <- function(x,y){
    stopifnot(class(x) == "RcppNaiveBayesTrain" &&
          class(y) == "RcppNaiveBayesTrain")
    z <- list();
    z[["wordsList"]] <- c(x[["wordsList"]], y[["wordsList"]])
    z[["categorySize"]] <- x[["categorySize"]] + y[["categorySize"]]
    z[["totalCorpusSize"]] <- x[["totalCorpusSize"]] + y[["totalCorpusSize"]]
    attr (z, "class") <- "RcppNaiveBayesTrain";
    z
}


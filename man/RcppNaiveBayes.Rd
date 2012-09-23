\name{RcppNaiveBayes}
\alias{NaiveBayesTrain}
\alias{NaiveBayesPredict}
\docType{package}
\title{Naive Bayes Classifier for corpora}
\description{
A naive Naive Bayesian Classifier for corpora
}
\details{
a "corpora" is a list of corpus, while a corpus is a list of texts,
with a category specified, and text is a simple string vector.
}
\author{
颜烈彬 <yanlb@ucweb.com>
}
\usage{
    NaiveBayesTrain(x)
    NaiveBayesPredict(m, y)
}

\arguments{
    \item{x}{a list of corpora, see Details.}
    \item{m}{an object of class RcppNaiveBayes return by NaiveBayesTrain.}
    \item{y}{a corpus, see Details.}
}
\references{

}

\keyword{classif}
\keyword{category}

\examples{
a <- list(c("A", "B", "B", "D", "A", "Z"),
          c("C", "B", "C", "Z", "H"))
b <- list(c("A", "F", "Y", "F", "W"),
          c("I", "A", "G", "F", "P", "D"),
          c("G", "A", "N", "P"))
d <- list(c("Y", "D", "P"),
          c("H", "H"),
          c("Z", "Z"))
## get a
m <- NaiveBayesTrain(list(a,b))

NaiveBayesPredict(m, d)
NaiveBayesPredict(m, c("php", "object", "oriented"))
}

#ifndef _RCPP_NAIVE_BAYES_H
#define _RCPP_NAIVE_BAYES_H

#include <Rcpp.h>
RcppExport SEXP NaiveBayesTrain(SEXP);
RcppExport SEXP NaiveBayesPredict(SEXP,SEXP,SEXP);
#endif

#include "rcpp_naive_bayes.h"
// TODO namespace issue

using namespace Rcpp;

/**
 * @brief Called by NaiveBayesTrain, internal use
 *
 * @param category a list of observations from on category
 * @return List list of feature/probabilities
 * @see http://css.dzone.com/articles/spam-filtering-naive-bayes
 **/

List NaiveBayesFeature (List category_in)
{
    BEGIN_RCPP
    int caseCount = category_in.size();
    double singleOccurrence = 1.0f / caseCount ;
    double zeroOccurrence = singleOccurrence;
    int wordsCount = 0;
    // TODO preserve size
    std::map<std::string, double> featuresProbabilities;
    std::map<std::string, double>::iterator it;
    // loop each observation to calculate feature/probability
    for (int i = 0; i < caseCount; ++i) {
        std::vector<std::string> observation = category_in[i];
        wordsCount = observation.size();
        for (int j = 0; j < wordsCount; ++j) {
            it = featuresProbabilities.find (observation[j]);
            if (it != featuresProbabilities.end()) {
                it->second += singleOccurrence;
            } else {
                featuresProbabilities.insert
                (std::pair<std::string, double>
                 (observation[j], zeroOccurrence + singleOccurrence));
            }
        }
    }
    return List::create (_["featuresProbabilities"] = featuresProbabilities,
                         _["caseCount"] = caseCount);
    END_RCPP
}

/**
 * @brief  Creates a training model from @a categories
 *
 * @param categories a list of category, each category consists of a list of
 * observations
 * @return SEXP a RcppNaiveBayes class
 **/

SEXP NaiveBayesTrain (SEXP categories_in)
{
    BEGIN_RCPP
    List categories (categories_in); //conversion
    int categoryCount = categories.size();
    List featuresList (categoryCount);
    std::vector<double> prior (categoryCount);
    double total = 0.0f;
    for (int i = 0; i < categoryCount; i++) {
        List category = NaiveBayesFeature (categories[i]);
        featuresList[i] = category;
        prior[i] = as<int> (category["caseCount"]);
        total += prior[i];
    }
    for (int i = 0; i < categoryCount; ++i) {
        prior[i] /= total;
    }
    List z = List::create (_["prior"] = prior,
                           _["featuresList"] = featuresList,
                           _["categories"] = categoryCount);
    z.attr ("class") = "RcppNaiveBayes";
    return z;
    END_RCPP
}


/**
 * @brief Predict the category from @a NBT_in for @a unknown_in observation
 *
 * @param NBT_in a NaiveBayesTrain class
 * @param unknown_in a string vector
 * @return SEXP a list of probabilities called scores
 **/

SEXP NaiveBayesPredict (SEXP NBT_in, SEXP unknown_in)
{
    BEGIN_RCPP
    List NBT (NBT_in);
    std::vector<std::string> unknown =
        as<std::vector<std::string> > (unknown_in);
    std::vector<std::string>::iterator  unknown_it;
    std::vector<double> scores = as<std::vector<double> > (NBT["prior"]);
    List featuresList = NBT["featuresList"];
    int categoryCount = as<int> (NBT["categories"]);
    double zero = 0.0;
    for (int i = 0; i < categoryCount; ++i) {
        List thefeature = featuresList[i];
        zero = 1 / as<double> (thefeature["caseCount"]);
        List thefeaturesProbabilities = thefeature["featuresProbabilities"];
        StringVector nam = thefeaturesProbabilities.names();
        std::vector<std::string> features = as<std::vector<std::string> > (nam);
        for (unknown_it = unknown.begin();
                unknown_it != unknown.end(); ++unknown_it) {
            for (int j = 0; j < features.size(); ++j) {
                if (features[j].compare (*unknown_it) == 0) {
                    scores[i] *= as<double> (thefeaturesProbabilities[j]);
                    goto END;
                }
            }
            scores[i] *= zero;
        END:;
        }
    }
    List z = List::create (_["scores"] = scores);
    return z;
    END_RCPP
}

/**
 * @brief A prediction dispatcher
 *
 * @param NBT_in the model return by NaiveBayesTrain
 * @param unknown_in a string vector or a list of string vectors
 * @param isList_in a flag to indicate if unknown_in is a List
 * @return SEXP s list of scores for each case
 **/

SEXP NaiveBayesPredict (SEXP NBT_in, SEXP unknown_in, SEXP isList_in)
{
    int isList = as<int> (isList_in);
    if (isList) {
        List cases (unknown_in);
        List predictList (cases.size());
        for (int i = 0; i < cases.size(); i++) {
            predictList[i] = NaiveBayesPredict (NBT_in, cases[i]);
        }
        return predictList;
    } else {
        return NaiveBayesPredict (NBT_in, unknown_in);
    }
}

// kate: indent-mode cstyle; indent-width 4; replace-tabs on; ;

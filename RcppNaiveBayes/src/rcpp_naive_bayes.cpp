#include "rcpp_naive_bayes.h"
// #define DEBUG
// TODO make use of std::transform(input.begin(), input.end(), output.begin(), f);

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

    std::vector<std::string> features (featuresProbabilities.size());
    std::vector<double> probabilities (featuresProbabilities.size());

    int ind = 0;

    for (it = featuresProbabilities.begin(); it != featuresProbabilities.end(); ++it) {
        features[ind] = it->first;
        probabilities[ind] = it->second;
        ind ++;
    }
#ifdef DEBUG
    printf ("DEBUG NaiveBayesFeature: %d\n", caseCount);
#endif
    return List::create (_["features"] = features,
                         _["probabilities"] = probabilities,
                         _["zero"] = zeroOccurrence, // To be removed
                         _["caseCount"] = caseCount);
    END_RCPP
}

/**
 * @brief  Creates a training model
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

    double total = 0.0f;// it's integer actually
    for (int i = 0; i < categoryCount; i++) {
        List category = NaiveBayesFeature (categories[i]);
        featuresList[i] = category;
        prior[i] = as<int> (category["caseCount"]);
        total += prior[i];
    }

    // calculate prior
    for (int i = 0; i < categoryCount; ++i) {
#ifdef DEBUG
        printf ("DEBUG NaiveBayesTrain: %f\n", prior[i]);
#endif
        prior[i] /= total;
    }

    // well, need correct names
    // categories could be ignored
    List z = List::create (_["prior"] = prior,
                           _["features"] = featuresList,
                           _["categories"] = categoryCount);
    z.attr ("class") = "RcppNaiveBayes";
    return z;
    END_RCPP
}


/**
 * @brief Predict the category from NB for unknown
 *
 * @param NB a RcppNaiveBayes class
 * @param unknown an observations to classify
 * @return SEXP a list of probabilities and a name of highest score
 **/

SEXP NaiveBayesPredict (SEXP NBT_in, SEXP unknown_in)
{
    BEGIN_RCPP
    List NBT (NBT_in);
    std::vector<std::string> unknown = as<std::vector<std::string> > (unknown_in);
    std::vector<std::string>::iterator  unknown_it;
    std::vector<double> prior = NBT["prior"];
    List featuresList = NBT["features"];
    int categoryCount = as<int> (NBT["categories"]);
    double zero = 0.0f;

    std::vector<double> scores (categoryCount);

    // This is a short-term loop, as categoriesCount is assumed to be less than 100 in practice
    for (int i = 0; i < categoryCount; ++i) {

        scores[i] = prior[i];
        List thefeature = featuresList[i];
        std::vector<std::string> features = thefeature["features"];
        std::vector<double> probabilities = thefeature["probabilities"];
        zero = as<double> (thefeature["zero"]);
        for (unknown_it = unknown.begin(); unknown_it != unknown.end();
                ++unknown_it) {
            // features.size() may be too large!
            for (int j = 0; j < features.size(); ++j) {
                // string compare is somewhat slow!
                if (features[j].compare (*unknown_it) == 0) {
                    scores[i] *= probabilities[j];
                    goto ENDCOMPARE;
                }
            }
            scores[i] *= zero;
        ENDCOMPARE:; //
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

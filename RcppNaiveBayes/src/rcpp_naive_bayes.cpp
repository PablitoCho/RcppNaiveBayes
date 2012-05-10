#include "rcpp_naive_bayes.h"

// TODO namespace issue
using namespace Rcpp;

/**
 * @brief Called by NaiveBayesTrain, internal use only
 *
 * @param corpus_in A corpus
 * @return List A list of words/frequencies
 * @see http://css.dzone.com/articles/spam-filtering-naive-bayes
 **/

List NaiveBayesWordsFreq (List corpus_in)
{
    //BEGIN_RCPP
    int corpusSize = corpus_in.size();
    int wordsCount = 0;
    std::map<std::string, int> wordsFrequencies;
    std::map<std::string, int>::iterator it;
    for (int i = 0; i < corpusSize; ++i) {
        std::vector<std::string> texts = corpus_in[i];
        wordsCount = texts.size();
        for (int j = 0; j < wordsCount; ++j) {
            it = wordsFrequencies.find (texts[j]);
            if (it != wordsFrequencies.end()) {
                it->second ++;
            } else {
                wordsFrequencies.insert (std::pair<std::string, int>
                                         (texts[j], 2));
            }
        }
    }
    return List::create (_["wordsFrequencies"] = wordsFrequencies,
                         _["corpusSize"] = corpusSize);
    //END_RCPP
}

/**
 * @brief Creates a training model from @a corpora_in
 *
 * @param corpora_in  A list of corpus
 * @return SEXP A RcppNaiveBayes class
 **/

SEXP NaiveBayesTrain (SEXP corpora_in)
{
    //BEGIN_RCPP
    List corpora (corpora_in);
    int corporaSize = corpora.size();
    List wordsList (corporaSize);
    int total = 0;
    for (int i = 0; i < corporaSize; i++) {
        List category = NaiveBayesWordsFreq (corpora[i]);
        wordsList[i] = category;
        total += as<int> (category["corpusSize"]);
    }
    List z = List::create (_["wordsList"] = wordsList,
                           _["categorySize"] = corporaSize,
                           _["totalCorpusSize"] = total);
    z.attr ("class") = "RcppNaiveBayesTrain";
    return z;
    //END_RCPP
}

/**
 * @brief Predict the category from @a NBT_in for @a unknown_in observation
 *
 * @param NBT_in a RcppNaiveBayesTrain class
 * @param unknown_in a string vector
 * @return SEXP a list of probabilities called scores
 **/

SEXP NaiveBayesPredict (SEXP NBT_in, SEXP unknown_in)
{
    //BEGIN_RCPP
    List NBT (NBT_in);
    std::vector<std::string> unknown =
        as<std::vector<std::string> > (unknown_in);
    std::vector<std::string>::iterator unknown_it;
    List wordsList = NBT["wordsList"];
    int categoryCount = as<int> (NBT["categorySize"]);
    std::vector<long double> scores (categoryCount);
    int factor = as<int> (NBT["totalCorpusSize"]);

    for (int i = 0; i < categoryCount; ++i) {
        List wordsFreqTable = wordsList[i];
        int corpusSize = as<int> (wordsFreqTable["corpusSize"]);
        scores[i] = (long double) corpusSize;
        List wordsFrequencies = wordsFreqTable["wordsFrequencies"];
        StringVector nam = wordsFrequencies.names();
        std::vector<std::string> words = as<std::vector<std::string> > (nam);
        // NOTE if unknown.size() is large enough, accuracy issue will file.
        for (unknown_it = unknown.begin();
                unknown_it != unknown.end(); ++unknown_it) {
            for (int j = 0; j < words.size(); ++j) {
                if (words[j].compare (*unknown_it) == 0) {
                    scores[i] *= (as<int> (wordsFrequencies[j]));
                    break;
                }
            }
            scores[i] /= corpusSize;
        }
        scores[i] /= factor;
    }
//     List z = List::create (_["scores"] = scores);
    return wrap(scores);
    //END_RCPP
}

/**
 * @brief A predictor
 *
 * @param NBT_in A model return by NaiveBayesTrain
 * @param unknown_in A corpus or a text
 * @param isCorpus_in As is
 * @return SEXP A list of scores for each case
 **/

SEXP NaiveBayesPredict (SEXP NBT_in, SEXP unknown_in, SEXP isCorpus_in)
{
    //BEGIN_RCPP
    int isCorpus = as<int> (isCorpus_in);
    if (isCorpus) {
        List cases (unknown_in);
        List predictList (cases.size());
        for (int i = 0; i < cases.size(); i++) {
            predictList[i] = NaiveBayesPredict (NBT_in, cases[i]);
        }
        return predictList;
    } else {
        List z = NaiveBayesPredict (NBT_in, unknown_in);
        return z;
    }
    //END_RCPP
}

// kate: indent-mode cstyle; indent-width 4; replace-tabs on; ;


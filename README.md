# TEXTUAL DATA CLASSIFICATION
In this work we will use an online newsgroup text dataset. This dataset contains text articles posted to each four online newsgroups, for a total of 400 articles. For documentation and complete details please see the following website: http://www-2.cs.cmu.edu/afs/cs/project/theo-11/www/naive-bayes.html This data is useful for a variety of text classification projects. The target label of each article is the newsgroup it bellows to.
## Exploration of the dataset
First task is to get a feel of the data that we will be dealing with in the rest of the Project.
- Used tokenization and compute the top 200 most popular words (total occurrences)
- Repeated the previous step, but now filtered tokens by length (min. size = 4 and max. size = 20). 
- Indicate the total occurrences of the words.
## Basic evaluation:
- Trained and evaluateed different classifiers using 70% of the dataset from training and 30% for testing. Tried Naïve Bayes implementation, knn and Random Forest (use frequency counts for knn and Random Forest). 
- Compare the results of the three classifiers.
## Robust evaluation 
In this section we are interested in more rigorous techniques by implementing more sophisticated techniques.
- Hold-out and cross-validation.
- More classification algorithms, e.g., decision trees, and SVM.
- Hyper-parameter tuning.
- Feature selection and engineering.
- Performance metrics.
- Pre-processing techniques (with basic techniques, e.g., converting everything to lower-case, removal of punctuation, etc).

# Data-Analysis-using-R
Did the analysis of the mobile price data set using R language.
As the problem is based on multiclass classification I divided the dataset into four parts as there are four classes given in the data set.Then for each part remove the outliers using the box plot method and combine all the parts together . After that I remove the NA values but as there were no NA values so not any data is removed.Now an exciting outcome came during correlation analysis,feature RAM was highly correlated(0.92) with the price range that is the target and no redundant features were found.
Randomly split the data set into 2:1 ratio into training and testing data respectively, then used the rpart library to train and plot the model.
As we are splitting the data set randomly we will get different results in every instance.
Predictive Accuracy - 83.30%
Precision - 0.95, 0.73, 0.77, 0.85
Recall - 0.88, 0.85, 0.75, 0.83
F1 Scores - 0.91, 0.78, 0.76, 0.84

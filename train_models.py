import pandas as pd
import numpy as np
from sklearn.model_selection import StratifiedKFold
from sklearn.linear_model import LogisticRegression
np.random.seed(123)

def train_k_fold_eval(df_train, df_eval, clf, var):

    """ This function takes a dataframe, a classifier, and a list of predictor variables to select. 
    Models are trained using k-fold cross-validation on the list of selected predictor variables 
    and the function returns the dataframe with a new column for the predicted probabilities, 
    bearing the name of the list of predictor variables, and a column with the cross-validation folds. """
    
    X = df_train[var]
    y = df_train[['ID_binary']]

    idx = X.index[~np.isnan(X).any(axis=1)].tolist()

    # K-fold within training dataset, stratified for outcome
    skf = StratifiedKFold(n_splits=10, shuffle=True, random_state=123)
    df_train['fold'] = np.nan
    df_train['mod_' + '+'.join(var)] = np.nan

    for i, (train_index, test_index) in enumerate(skf.split(X, y)):
        
        # Select data
        df_train.loc[test_index,'fold'] = i
        train_index = list(set(train_index).intersection(idx))
        test_index = list(set(test_index).intersection(idx))
        X_train = X.iloc[train_index]
        y_train = y.iloc[train_index].values.ravel()
        X_test = X.iloc[test_index]

        # Fit
        clf.fit(X_train, y_train)

        # Predict
        df_train.loc[test_index,'mod_' + '+'.join(var)] = clf.predict_proba(X_test)[:,1]

    # Re-train on full training dataset and predict on validation dataset
    train_index = idx
    X_train = X.iloc[train_index]
    y_train = y.iloc[train_index].values.ravel()

    X_test = df_eval[var]
    test_index = X_test.index[~np.isnan(X_test).any(axis=1)].tolist()
    X_test = X_test.iloc[test_index]

    # Fit
    clf.fit(X_train, y_train)

    # Predict
    df_eval['fold'] = 'validation'
    df_eval['mod_' + '+'.join(var)] = np.nan
    if X_test.shape[0] >= 1: 
        df_eval.loc[test_index,'mod_' + '+'.join(var)] = clf.predict_proba(X_test)[:,1]
    else:
        df_eval.loc[test_index,'mod_' + '+'.join(var)] = np.nan

    # Join the training and validation samples and predictions
    df = pd.concat([df_train, df_eval]).reset_index()

    return df
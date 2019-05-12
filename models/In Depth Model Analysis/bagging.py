#!/usr/bin/env python
# coding: utf-8

# In[45]:


#get_ipython().run_line_magic('run', 'main.ipynb')
#!/usr/bin/env python
# coding: utf-8

# <h1 style="color:royalblue; font-size:3em"> This serves as a baseline notebook to be imported by other notebooks </h1>

# In[3]:


import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.model_selection import GridSearchCV
from sklearn.model_selection import RandomizedSearchCV
import warnings
warnings.simplefilter(action='ignore', category=FutureWarning)
warnings.simplefilter("ignore", category=DeprecationWarning)
warnings.simplefilter("ignore", UserWarning)
warnings.simplefilter("ignore", RuntimeWarning)

pd.set_option('display.max_rows', 500)
pd.set_option('display.max_columns', 500)
pd.set_option('display.width', 1000)


# In[3]:


train=pd.read_csv('seminar-dmc/data/train.csv',sep='|')


# In[4]:


print(train.fraud.value_counts())
print(train.fraud.value_counts() / len(train))


# # Feature Engineering

# In[5]:


train['scannedLineItems'] = train['scannedLineItemsPerSecond'] * train['totalScanTimeInSeconds']
train['pricePerScannedLineItem'] = train['grandTotal'] / train['scannedLineItems']
train['scansWithoutRegistrationPerScannedLineItem'] = train['scansWithoutRegistration'] / train['scannedLineItems']
train['quantityModificationsPerScannedLineItem'] = train['quantityModifications'] / train['scannedLineItems']
train['lineItemVoidsPerSecond'] = train['lineItemVoids'] / train['totalScanTimeInSeconds']
train['scansWithoutRegistrationPerSecond'] = train['scansWithoutRegistration'] / train['totalScanTimeInSeconds']
train['quantityModificationsPerSecond'] = train['quantityModifications'] / train['totalScanTimeInSeconds']
train['secondsPerEuro'] = train['totalScanTimeInSeconds'] / train['grandTotal']
train['lineItemVoidsPerEuro'] = train['lineItemVoids'] / train['grandTotal']
train['scansWithoutRegistrationPerEuro'] = train['scansWithoutRegistration'] / train['grandTotal']
train['quantityModificationsPerEuro'] = train['quantityModifications'] / train['grandTotal']


# # Declare global variables

# In[6]:


from sklearn.model_selection import StratifiedKFold
from sklearn.model_selection import LeaveOneOut

# Cross validation
skf = StratifiedKFold(n_splits=10)
loo = LeaveOneOut()

# Split *train* dataset to feature and target sets
X = train.drop('fraud',axis=1)
Y = train['fraud']


# # Custom loss function

# In[7]:


from sklearn.metrics import confusion_matrix
from sklearn.metrics import make_scorer

def my_custom_loss_func(y_true, y_pred):
    cm = confusion_matrix(y_true, y_pred, labels=[0,1])
    tn, fp, fn, tp = cm.ravel()
    score = ((-25)*fp + (-5)*fn + 5*tp) / len(y_true)
    return (score)

my_custom_score = make_scorer(my_custom_loss_func, greater_is_better=True)


# # Function to run loops for training and cross validation

# In[8]:


import time
from sklearn.feature_selection import SelectKBest
from sklearn.feature_selection import chi2
from sklearn.feature_selection import f_classif

from sklearn.feature_selection import RFECV
from sklearn.feature_selection import RFE

from sklearn.preprocessing import MinMaxScaler
from sklearn.preprocessing import StandardScaler
from sklearn.preprocessing import FunctionTransformer

result_table = pd.DataFrame(columns=["Model", "Data Preparation", "Feature Count", "Features", "Optimal Parameters", "Monetary Value Per Instance - Mean", "Monetary Value Per Instance - Standard Deviation", "Raw Model"])
model_name = ''

def run():
    global result_table, model_name
    for data_preparation_strategy in range(1,5):
        if (data_preparation_strategy == 1):
            X_scaled = X
            data_preparation = "No Scaling"
        elif (data_preparation_strategy == 2):
            feature_scaler = MinMaxScaler()
            X_scaled = pd.DataFrame(feature_scaler.fit_transform(X.values), columns=X.columns, index=X.index)
            data_preparation = "MinMaxScaler"
        elif (data_preparation_strategy == 3):
            feature_scaler = StandardScaler()
            X_scaled = pd.DataFrame(feature_scaler.fit_transform(X.values), columns=X.columns, index=X.index)
            data_preparation = "StandardScaler"
        elif (data_preparation_strategy == 4):
            transformer = FunctionTransformer(np.log1p, validate=True)
            X_scaled = pd.DataFrame(transformer.transform(X), columns=X.columns, index=X.index)
            data_preparation = "LogScaler"

        for model in model_tuning_factory:   # replace with model_tuning_factory_randomized for faster results
            model_name = model.estimator.__class__.__name__

            for feature_count in range(1,len(list(X))+1):

                model.seed = 42
                start_time = time.time()


                # Solution with SelectKBest
                best_features = SelectKBest(f_classif, k=feature_count).fit(X_scaled,Y)
                best_feature_list = X.columns[best_features.get_support()]
                X_selected_features = X[best_feature_list]

                model.fit(X_selected_features,Y)
                model_name = model.best_estimator_.__class__.__name__
                score_mean = model.cv_results_['mean_test_score'][model.best_index_]
                score_std = model.cv_results_['std_test_score'][model.best_index_]

                result_table = result_table.append({
                 "Model": model_name,
                 "Data Preparation": data_preparation,
                 "Feature Count": feature_count,
                 "Feature Selection Technique": "SelectKBest",
                 "Features": best_feature_list.values,
                 "Optimal Parameters": model.best_params_,
                 "Monetary Value Per Instance - Mean":  score_mean,
                 "Monetary Value Per Instance - Standard Deviation": score_std,
                 "Raw Model": model.best_estimator_
                  }, ignore_index=True)


                # Solution with Recursive Feature Elimination -> only works for some models

                if (model.estimator.__class__.__name__ == 'LogisticRegression'
                 or model.estimator.__class__.__name__ == 'DecisionTreeClassifier'
                 or model.estimator.__class__.__name__ == 'ExtraTreeClassifier'
                 or model.estimator.__class__.__name__ == 'ExtraTreesClassifier'
                 or model.estimator.__class__.__name__ == 'RandomForestClassifier'
                 or model.estimator.__class__.__name__ == 'BernoulliNB'
                 or model.estimator.__class__.__name__ == 'AdaBoostClassifier'
                 or model.estimator.__class__.__name__ == 'GradientBoostingClassifier'
                 or model.estimator.__class__.__name__ == 'Perceptron'
                 or model.estimator.__class__.__name__ == 'LinearDiscriminantAnalysis'):

                   # Traditional RFE
                    rfe = RFE(model.estimator, n_features_to_select = feature_count)
                    rfe = rfe.fit(X,Y)
                    best_feature_list = np.array(list(X))[np.array(rfe.support_)]
                    X_selected_features = X[best_feature_list]

                    model.fit(X_selected_features,Y)
                    model_name = model.best_estimator_.__class__.__name__
                    score_mean = model.cv_results_['mean_test_score'][model.best_index_]
                    score_std = model.cv_results_['std_test_score'][model.best_index_]


                    result_table = result_table.append({
                     "Model": model_name,
                     "Data Preparation": data_preparation,
                     "Feature Count": feature_count,
                     "Feature Selection Technique": "RFE",
                     "Features": best_feature_list,
                     "Optimal Parameters": model.best_params_,
                     "Monetary Value Per Instance - Mean":  score_mean,
                     "Monetary Value Per Instance - Standard Deviation": score_std,
                     "Raw Model": model.best_estimator_
                      }, ignore_index=True)

                end_time = time.time()

                print("Finished " + model.best_estimator_.__class__.__name__ + " with " + data_preparation + " and " + str(feature_count) + " features after " + str(round(end_time - start_time, 2)) + " seconds")



            if (model.estimator.__class__.__name__ == 'LogisticRegression'
             or model.estimator.__class__.__name__ == 'DecisionTreeClassifier'
             or model.estimator.__class__.__name__ == 'ExtraTreeClassifier'
             or model.estimator.__class__.__name__ == 'ExtraTreesClassifier'
             or model.estimator.__class__.__name__ == 'RandomForestClassifier'
             or model.estimator.__class__.__name__ == 'BernoulliNB'
             or model.estimator.__class__.__name__ == 'AdaBoostClassifier'
             or model.estimator.__class__.__name__ == 'GradientBoostingClassifier'
             or model.estimator.__class__.__name__ == 'Perceptron'
             or model.estimator.__class__.__name__ == 'LinearDiscriminantAnalysis'):

                # RFE with Cross Validation -> determines the optimum feature count automatically
                rfecv = RFECV(model.estimator, cv = skf, scoring = my_custom_score)
                rfecv = rfe.fit(X,Y)
                best_feature_list = np.array(list(X))[np.array(rfecv.support_)]
                X_selected_features = X[best_feature_list]

                model.fit(X_selected_features,Y)
                model_name = model.best_estimator_.__class__.__name__
                score_mean = model.cv_results_['mean_test_score'][model.best_index_]
                score_std = model.cv_results_['std_test_score'][model.best_index_]


                result_table = result_table.append({
                 "Model": model_name,
                 "Data Preparation": data_preparation,
                 "Feature Count": len(best_feature_list),
                 "Feature Selection Technique": "RFECV",
                 "Features": best_feature_list,
                 "Optimal Parameters": model.best_params_,
                 "Monetary Value Per Instance - Mean":  score_mean,
                 "Monetary Value Per Instance - Standard Deviation": score_std,
                 "Raw Model": model.best_estimator_
                  }, ignore_index=True)

    result_table.sort_values(by = "Monetary Value Per Instance - Mean", ascending = False)


# In[ ]:


def pickle_it():
    result_table.to_pickle("result_table_" + str(model_name) + ".pkl")
    #result_table = pd.read_pickle("result_table_Random_Forest.pkl")


# # Plot number of features against monetary value

# In[9]:


def plot_number_features():
    plt.rcParams['figure.figsize'] = (10,10)

    plt.scatter(result_table["Feature Count"], result_table["Monetary Value Per Instance - Mean"])
    plt.xlabel('Number of features', fontsize=16)
    plt.ylabel('Monetary Value Per Instance - Mean', fontsize=16)


# # Class to store the best model

# In[10]:


class BestModel:
    best_model = None
    best_model_features = None
    rank = 0

    def __init__(self, rank):
        self.rank = rank

    def set(self):
        global result_table
        self.best_model = result_table.loc[self.rank,]["Raw Model"]
        self.best_model_features = result_table.loc[self.rank,]["Features"]

    def predict(self):
        self.set()
        return self.best_model.predict(X[self.best_model_features])

    def print_best_model(self):
        print(self.best_model)
        print(self.best_model_features)


# # Calculate performance of the best model

# In[11]:


def get_monetary_value(best_model):
        cm = confusion_matrix(Y , best_model.predict())
        tn, fp, fn, tp = cm.ravel()
        size = tn + fp + fn + tp
        print("True negative: ", tn)
        print("False positive: ", fp)
        print("False negative: ", fn)
        print("True positive: ", tp)
        score = (-25)*fp + (-5)*fn + 5*tp
        print(score, 'for ', size, ' instances in the test set')
        print(score/size, ' per instance in the test set')
        return score


# In[46]:


# Table for training results
result_table = pd.DataFrame(columns=["Model", "Data Preparation", "Feature Count", "Features",
                                     "Optimal Parameters", "Monetary Value Per Instance - Mean",
                                     "Monetary Value Per Instance - Standard Deviation",
                                     "Time needed", "Raw Model"])


# In[47]:


from itertools import chain
def get_dict_concat(d1, d2):
    return dict(chain.from_iterable(d.items() for d in (d1, d2)))


# # Model factory -> only KNeighborsClassifier for in-depth analysis

# In[ ]:


from sklearn.ensemble.bagging import BaggingClassifier
from sklearn.tree import DecisionTreeClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.neighbors.classification import KNeighborsClassifier

from sklearn.pipeline import make_pipeline

skf = StratifiedKFold(n_splits=10)

# model = {'model': BaggingClassifier,
#          'kwargs':
#              {'base_estimator': DecisionTreeClassifier(),


#              },
#          'parameters': {
#              'name__base_estimator__max_leaf_nodes': [5,10,20,30]
#          }}
# pipeline = Pipeline([('name', model['model'](**model['kwargs']))])
# cv_model = GridSearchCV(pipeline, param_grid=model['parameters'], cv=cv, scoring=scorer)

param_bagging = {
#         'bootstrap': [True, False],
#     'bootstrap_features': [True, False],    
    'n_estimators': [50, 120],
#         'warm_start': [True, False],
    'oob_score': [True, False],
}

param_dt = {
    'base_estimator__max_leaf_nodes': [5,10,20,30]
}

param_knn = {
    'base_estimator__n_neighbors' : [15],
    'base_estimator__weights' : ['distance'],
    'base_estimator__p' : [1]
}

param_logistics = {
    'base_estimator__solver' : ['newton-cg'], 
    'base_estimator__C' : [1.2],
    'base_estimator__fit_intercept' : [True],
    'base_estimator__class_weight' : [None, 'balanced'],
    'base_estimator__max_iter': [100000]
}


model_tuning_factory = [
#     GridSearchCV(BaggingClassifier(DecisionTreeClassifier()),
#                  param_grid = get_dict_concat(param_bagging, param_dt), 
#                  cv = skf,
#                  scoring = my_custom_score,
#                  n_jobs = 4
#                  ),
        
    GridSearchCV(BaggingClassifier(LogisticRegression(n_jobs = 4)),
                 param_grid = get_dict_concat(param_bagging, param_logistics), 
                 cv = skf,
                 scoring = my_custom_score,
                 n_jobs = 8
                 ),
    
#     GridSearchCV(BaggingClassifier(KNeighborsClassifier()),
#                  param_grid = get_dict_concat(param_bagging, param_knn), 
#                  cv = skf,
#                  scoring = my_custom_score,
#                  n_jobs = 4
#                  )
]           
   


# iterations = 10

# model_tuning_factory_randomized = [
#     RandomizedSearchCV(BaggingClassifier(),
#                  dict(n_estimators = range(1,150)),
#                  cv = skf,
#                  scoring = my_custom_score,
#                  n_iter = iterations)
# ]


# In[ ]:


run()


# In[ ]:


result_table = result_table.sort_values(by = "Monetary Value Per Instance - Mean", ascending = False)
result_table.index = range(0,result_table.shape[0])
result_table


# In[ ]:


pickle_it()


# # Print performance of best 10 models

# In[ ]:


for rank in range(0,11):
    best = BestModel(rank)
    monetary_value = get_monetary_value(best)
    print()
    best.print_best_model()
    print("-----------------------------------------------------------------------------------------------")

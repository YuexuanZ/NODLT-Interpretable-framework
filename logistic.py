import lightgbm as lgb
import pandas as pd
import numpy as np
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import mean_squared_error
from sklearn.model_selection import GridSearchCV
from sklearn.metrics import roc_auc_score,average_precision_score,recall_score,accuracy_score
from pre_data_balance import data_partition_random_XGB
from pre_data_balance_nonval import data_partition_random_XGB_nonval

origdata = pd.read_csv('D:/NODLT/Data_for_ML/Input_uni_White.csv',header=0)
ncol = origdata.shape[1]
data = origdata.iloc[:,:ncol-1]
label = origdata.iloc[:,-1]

classList = [1,0]
val=0
# smote/random/BorderlineSMOTE/ADASYN/SVMSMOTE
if val==1:
    X_train,X_val,X_test,y_train,y_val,y_test = data_partition_random_XGB(origdata,classList,Sampler="random")
else:
    X_train, X_test, y_train, y_test = data_partition_random_XGB_nonval(origdata, classList, propertion=0.7,Sampler="BorderlineSMOTE")

learning_rate=0.1
n_estimators=1000

gbm = LogisticRegression(random_state=1)
gbm.fit(X_train, y_train)

print('Start predicting...')

y_pred = gbm.predict(X_test)
pred_onehot = np.array([0]*len(y_pred))
for i in range(len(y_pred)):
    if y_pred[i]>np.mean(y_pred):
        pred_onehot[i]=1

print('The rmse of prediction is:', mean_squared_error(y_test, pred_onehot) ** 0.5)

auc_score = roc_auc_score(pred_onehot, y_test)
ap_score = average_precision_score(pred_onehot, y_test)
acc_score = accuracy_score(pred_onehot, y_test)
def calculate(inputs, targets):
    TP = sum(inputs * targets)
    FP = sum((1-targets)*inputs)
    FN = sum(targets*(1-inputs))
    TN = sum((1-inputs)*(1-targets))
    return TP, FP, FN, TN

sensitivity = recall_score(pred_onehot, y_test)

TP, FP, FN, TN = calculate(pred_onehot, y_test)
specificity = TN/(TN+FP)

print("learning_rate=", learning_rate)
print("epochs=", n_estimators)
print("accuracy=", acc_score)
print("AUC=", auc_score)
print("AP=", ap_score)
print("Sensitivity=", sensitivity)
print("Specificity=", specificity)


# Grid search, parameter optimization
estimator = lgb.LGBMRegressor(num_leaves=31)

param_grid = {
    'learning_rate': [0.01, 0.1, 1],
    'n_estimators': [200,1000]
}

gbm = GridSearchCV(estimator, param_grid)

gbm.fit(X_train, y_train)

print('Best parameters found by grid search are:', gbm.best_params_)
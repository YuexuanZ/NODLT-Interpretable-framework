import numpy as np
import pandas as pd
from sklearn import svm
from sklearn.metrics import roc_auc_score,average_precision_score,recall_score
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

clf1=svm.SVC(kernel='linear',decision_function_shape='ovr').fit(X_train,y_train)

#clf2=svm.SVC(kernel='rbf',gamma=1).fit(X_train,y_train)

#clf3=svm.SVC(kernel='poly').fit(X_train,y_train)

#clf4=svm.SVC(kernel='sigmoid').fit(X_train,y_train)


def calculate(inputs, targets):
    TP = sum(inputs * targets)
    FP = sum((1-targets)*inputs)
    FN = sum(targets*(1-inputs))
    TN = sum((1-inputs)*(1-targets))
    return TP, FP, FN, TN


cl1_1=clf1.score(X_train,y_train)
cl1_2=clf1.score(X_test,y_test)
ypred = clf1.predict(X_test)

ap_score = average_precision_score(y_test,ypred)
auc_score = roc_auc_score(y_test,ypred)
sensitivity = recall_score(y_test,ypred)
TP, FP, FN, TN = calculate(ypred,y_test)
specificity = TN/(TN+FP)
print("AUC=", auc_score)
print("AP=", ap_score)
print("Sensitivity=", sensitivity)
print("Specificity=", specificity)
from pre_data_balance import data_partition_random_XGB
from pre_data_balance_nonval import data_partition_random_XGB_nonval

import pandas as pd
from sklearn.ensemble import AdaBoostClassifier
from sklearn.metrics import roc_auc_score
from sklearn.metrics import average_precision_score,recall_score

origdata = pd.read_csv('D:/NODLT/Data_for_ML/Input_uni_White.csv', header=0)

ncol = origdata.shape[1]
data = origdata.iloc[:,:ncol-1]
label = origdata.iloc[:,-1]

classList = [1,0]
val=0
# smote/random/BorderlineSMOTE/ADASYN/SVMSMOTE
if val==1:
    X_train,X_val,X_test,y_train,y_val,y_test = data_partition_random_XGB(origdata,classList,Sampler="random")
else:
    X_train, X_test, y_train, y_test = data_partition_random_XGB_nonval(origdata, classList, propertion=0.8,Sampler="SVMSMOTE")

learning_rate=0.1
epochs = 500

model = AdaBoostClassifier(n_estimators=epochs,learning_rate=learning_rate,random_state=123)
model.fit(X_train, y_train)
ypred = model.predict(X_test)

def calculate(inputs, targets):
    TP = sum(inputs * targets)
    FP = sum((1-targets)*inputs)
    FN = sum(targets*(1-inputs))
    TN = sum((1-inputs)*(1-targets))
    return TP, FP, FN, TN

ap_score = average_precision_score(y_test,ypred)
auc_score = roc_auc_score(y_test,ypred)
sensitivity = recall_score(y_test,ypred)
TP, FP, FN, TN = calculate(ypred,y_test)
specificity = TN/(TN+FP)

print("learning_rate=", learning_rate)
print("epochs=", epochs)
print("AUC=", auc_score)
print("AP=", ap_score)
print("Sensitivity=", sensitivity)
print("Specificity=", specificity)


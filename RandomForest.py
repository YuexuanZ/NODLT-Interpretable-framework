from sklearn.ensemble import RandomForestClassifier
import pandas as pd
from sklearn.model_selection import GridSearchCV
from sklearn.metrics import roc_auc_score
from sklearn.metrics import average_precision_score,recall_score
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
    X_train, X_test, y_train, y_test = data_partition_random_XGB_nonval(origdata, classList, propertion=0.8,Sampler="SVMSMOTE")

learning_rate = 0.1
epochs = 1000

model = RandomForestClassifier(n_estimators=10,max_features=5)
model.fit(X_train, y_train)
ypred = model.predict(X_test)

def calculate(inputs, targets):
    TP = sum(inputs * targets)
    FP = sum((1-targets)*inputs)
    FN = sum(targets*(1-inputs))
    TN = sum((1-inputs)*(1-targets))
    return TP, FP, FN, TN

# 计算准确率
#accuracy = accuracy_score(y_test,ypred)
ap_score = average_precision_score(y_test,ypred)
auc_score = roc_auc_score(y_test,ypred)
sensitivity = recall_score(y_test,ypred)
TP, FP, FN, TN = calculate(ypred,y_test)
specificity = TN/(TN+FP)
#print("accuracy=", accuracy)
print("learning_rate=", learning_rate)
print("epochs=", epochs)
print("AUC=", auc_score)
print("AP=", ap_score)
print("Sensitivity=", sensitivity)
print("Specificity=", specificity)

# Grid search, parameter optimization
estimator = RandomForestClassifier()

param_grid = {
    #'learning_rate': [0.01,0.05, 0.1, 1],
    'n_estimators': [200,500,1000],
    'max_features': [5,10,15]
}

rf = GridSearchCV(estimator, param_grid,scoring='roc_auc',cv=3)

rf.fit(X_train, y_train)

print('Best parameters found by grid search are:', rf.best_params_)   # 'max_features': 5, 'n_estimators': 1000

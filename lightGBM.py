import lightgbm as lgb
import pandas as pd
import numpy as np
import shap
import matplotlib.pyplot as plt
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
    X_train, X_test, y_train, y_test = data_partition_random_XGB_nonval(origdata, classList, propertion=0.8,Sampler="SVMSMOTE")

learning_rate=0.1
n_estimators=1000
num_leaves = 16

lgbm = lgb.LGBMClassifier(num_leaves=num_leaves,learning_rate=learning_rate,n_estimators=n_estimators)
lgbm.fit(X_train, y_train)
y_pred = lgbm.predict(X_test)

auc_score = roc_auc_score(y_pred, y_test)
ap_score = average_precision_score(y_pred, y_test)
acc_score = accuracy_score(y_pred, y_test)


def calculate(inputs, targets):
    TP = sum(inputs * targets)
    FP = sum((1-targets)*inputs)
    FN = sum(targets*(1-inputs))
    TN = sum((1-inputs)*(1-targets))
    return TP, FP, FN, TN


sensitivity = recall_score(y_pred, y_test)

TP, FP, FN, TN = calculate(y_pred, y_test)
specificity = TN/(TN+FP)

print("learning_rate=", learning_rate)
print("epochs=", n_estimators)
print("num_leaves=", num_leaves)
print("accuracy=", acc_score)
print("AUC=", auc_score)
print("AP=", ap_score)
print("Sensitivity=", sensitivity)
print("Specificity=", specificity)
print('Feature importances:', list(lgbm.feature_importances_))

explainer = shap.TreeExplainer(lgbm)
shap_values = explainer.shap_values(X_test)

plt.figure(figsize=(22,6))
lgb.plot_importance(lgbm, height=0.5,max_num_features=20)
plt.title("Feature importance")
plt.tight_layout()
plt.savefig('Feature importance.pdf')

# summary plot
fig, ax = plt.subplots()
shap.summary_plot(shap_values[1], X_test,show=False)
plt.tight_layout()
plt.savefig('summary plot.pdf')

# Sample distribution of significant risk factors
Vars = X_test.columns.tolist()
for i in Vars:
    fig, ax = plt.subplots()
    plt.rcParams['axes.unicode_minus'] = False
    shap.dependence_plot(i, shap_values[1], X_test, interaction_index=None, show=False,
                         title='SHAP value for {}'.format(i))
    plt.tight_layout()
    plt.savefig('OneVar_{}.pdf'.format(i))

# SHAP Dependence Plots (Interaction)
for i in Vars:
    for j in Vars:
        if j!=i:
            fig, ax = plt.subplots()
            shap.dependence_plot(i, shap_values[1], X_test, interaction_index=j, show=False)
            plt.tight_layout()
            plt.savefig('{}_{}.pdf'.format(i, j))

# Contribution of risk factors for single samples
sample_ind = np.where(y_test.values==1)[0]
ind_random = np.random.choice(sample_ind, 50, replace=False)
for i in ind_random:
    fig, ax = plt.subplots()
    shap.force_plot(explainer.expected_value[1], shap_values[1][i, :], X_test.iloc[i, :], matplotlib=True,
                    show=False)
    plt.tight_layout()
    plt.savefig('TestSample_{}.pdf'.format(i))

# Grid search, parameter optimization
estimator = lgb.LGBMClassifier()

param_grid = {
    'learning_rate': [0.01, 0.1, 1],
    'n_estimators': [500,1000,2000],
    'num_leaves': [16,32,64]
}

gbm = GridSearchCV(estimator, param_grid,scoring='roc_auc',cv=3)

gbm.fit(X_train, y_train)
print('Best parameters found by grid search are:', gbm.best_params_)
import random
from imblearn.over_sampling import RandomOverSampler, SMOTE, BorderlineSMOTE, ADASYN, KMeansSMOTE, SVMSMOTE
import numpy as np

def data_partition_random_XGB(data,cluster,Sampler):
    np.random.seed(123)
    val_set_n = int(data.shape[0]*0.2)
    n = data.shape[0]    # samples
    k = len(cluster)     # cluster
    f = data.shape[1]    # features + cluster
    features = data.iloc[:,0:f-1]
    labels = data.iloc[:,-1]    # cluster of samples

    # balance
    if Sampler=="random":
        ros = RandomOverSampler(random_state=0)
        X_oversampled, y_oversampled = ros.fit_resample(features, labels)
    elif Sampler=="smote":
        smote = SMOTE(random_state=0)
        X_oversampled, y_oversampled = smote.fit_resample(features, labels)
    elif Sampler == "BorderlineSMOTE":
        smo=BorderlineSMOTE(kind='borderline-1', random_state=0)
        X_oversampled, y_oversampled = smo.fit_resample(features, labels)
    elif Sampler == "ADASYN":
        ana = ADASYN(random_state=0)
        X_oversampled, y_oversampled = ana.fit_resample(features, labels)
    elif Sampler == "KMeansSMOTE":
        kms = KMeansSMOTE(random_state=0)
        X_oversampled, y_oversampled = kms.fit_resample(features, labels)
    elif Sampler == "SVMSMOTE":
        svmm = SVMSMOTE(random_state=0)
        X_oversampled, y_oversampled = svmm.fit_resample(features, labels)

    labels = np.array(y_oversampled)

    train_mask = np.zeros(n).astype(bool)  # n
    val_mask = np.zeros(n).astype(bool)
    test_mask = np.zeros(n).astype(bool)

    label_n_per_class = np.zeros(k,dtype=int)
    class_index_dict = {}

    if len(cluster) == 4:
       class_index_dict[0] = np.where(labels == '0')[0]
       label_n_per_class[0] = int(len(class_index_dict[0]) * 0.7)
       class_index_dict[1] = np.where(labels == '1')[0]
       label_n_per_class[1] = int(len(class_index_dict[1]) * 0.7)
       class_index_dict[2] = np.where(labels == '2')[0]
       label_n_per_class[2] = int(len(class_index_dict[2]) * 0.7)
       class_index_dict[3] = np.where(labels == '3')[0]
       label_n_per_class[3] = int(len(class_index_dict[3]) * 0.7)
    else:
        class_index_dict[1] = np.where(labels == 1)[0]
        label_n_per_class[1] = int(len(class_index_dict[1]))
        class_index_dict[0] = np.where(labels == 0)[0]
        label_n_per_class[0] = int(len(class_index_dict[0]))

    train_index = np.random.choice(class_index_dict[1], int(label_n_per_class[1] * 0.6), replace=False)
    train_index = list(train_index)
    test_val_potential_index = list(set(class_index_dict[1]) - set(train_index))
    val_index = np.random.choice(test_val_potential_index, int(label_n_per_class[1] * 0.2), replace=False)
    potential_val_index = list(set(test_val_potential_index) - set(val_index))
    test_num = label_n_per_class[1] - len(train_index) - len(val_index)
    test_index = np.random.choice(potential_val_index, test_num, replace=False)

    # false sample
    train_index_false = np.random.choice(class_index_dict[0], len(train_index), replace=False)
    train_index_false = list(train_index_false)
    test_val_false_potential_index = list(set(class_index_dict[0]) - set(train_index_false))
    val_index_false = np.random.choice(test_val_false_potential_index, len(val_index), replace=False)
    potential_test_index = list(set(test_val_false_potential_index) - set(val_index_false))
    test_index_false = np.random.choice(potential_test_index, len(test_index), replace=False)

    train_all = np.hstack((train_index, train_index_false))
    val_all = np.hstack((val_index, val_index_false))
    test_all = np.hstack((test_index, test_index_false))
    random.shuffle(train_all)
    random.shuffle(val_all)
    random.shuffle(test_all)
    data_train = X_oversampled.iloc[train_all,]
    data_val = X_oversampled.iloc[val_all,]
    data_test = X_oversampled.iloc[test_all,]
    y_train = y_oversampled.iloc[train_all,]
    y_val = y_oversampled.iloc[val_all,]
    y_test = y_oversampled.iloc[test_all,]

    return data_train, data_val, data_test,y_train,y_val,y_test


def masked_accuracy(preds, labels):
    """Accuracy with masking."""
    correct_prediction = np.equal(preds,labels)
    correct_num = np.where(correct_prediction == True)
    accuracy_all = len(correct_num[0]) / len(preds)
    return accuracy_all



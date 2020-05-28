from numpy import array_split
from itertools import combinations
from random import shuffle


def kfold_splits(indices, K):
    """
    Shuffles and computes indices for kfold validation
    
    """
    shuffle(indices)

    folds = array_split(indices, K)
    kset = set(range(K))
    fold_train_indices = list(combinations(kset, K - 1))
    fold_test_indices = [kset.difference(x).pop() for x in fold_train_indices]

    kfolds = []
    for x, y in zip(fold_train_indices, fold_test_indices):
        train = list(folds[x[0]:x[1]][0])
        test = list(folds[y])
        kfolds.append([train, test])

    return kfolds

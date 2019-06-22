# -*- coding: utf-8 -*-
"""
Spyder Editor

Dies ist eine temporäre Skriptdatei.
"""

import pandas as pd
import numpy as np

train=pd.read_csv('../../1. Task and Data/train.csv',sep='|')
test=pd.read_csv('../../1. Task and Data/test.csv',sep='|')

test['fraud'] = -1

train_test_combined = pd.concat([train, test])

from sklearn.semi_supervised import LabelSpreading
from sklearn.metrics import accuracy_score
from sklearn.model_selection import train_test_split

Y = train['fraud']
X = train.drop('fraud',axis=1)
x_train, x_holdout, y_train, y_holdout = train_test_split(X, Y, test_size=0.2, random_state=324)

model= LabelSpreading()
model.fit(x_train,y_train)
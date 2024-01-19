import numpy as np
import pandas as pd
from sklearn.datasets import load_wine
from sklearn.linear_model import LogisticRegression

wine = load_wine()

lr = LogisticRegression(penalty="elasticnet", solver="saga", l1_ratio=0.5)
model = lr.fit(wine.data, wine.target)

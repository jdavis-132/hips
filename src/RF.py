import pandas as pd
import numpy as np
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import mean_squared_error
from sklearn import preprocessing
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import (cross_val_score,
    RepeatedKFold,
    RandomizedSearchCV,
    KFold,
    train_test_split,
    GridSearchCV,
    GroupKFold,
    train_test_split,GroupShuffleSplit)                                
from sklearn.metrics import mean_squared_error
# from sklearn.exceptions import ConvergenceWarning
# Suppress ConstantInputWarning
# warnings.filterwarnings("ignore", category=stats.ConstantInputWarning)
# warnings.filterwarnings("ignore", category=ConvergenceWarning)
import base

class RF(base):

    """" Random Forest Regression Model """
    
    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self.features_processed = self.preProcess_features(rescale_type=kwargs.get("rescale_type"))


    def grid_search(self, **kwargs):
        n_estimators = [int(x) for x in np.linspace(start=100, stop=1000, num=10)]
        max_features = [1, 2, 3, 4,5,6,7,8,9,10]
        max_depth = [int(x) for x in np.linspace(10, stop=100, num=11)]
        max_depth.append(None)
        min_samples_split = [2, 5, 10]
        min_samples_leaf = [10, 15, 20]
        bootstrap = [True, False]
        random_grid = {
            "n_estimators": n_estimators,
            "max_features": max_features,
            "max_depth": max_depth,
            "min_samples_split": min_samples_split,
            "min_samples_leaf": min_samples_leaf,
            "bootstrap": bootstrap}
       
        rf = RandomForestRegressor()
       
        rf_random = RandomizedSearchCV(
            estimator=rf,
            param_distributions=random_grid,
            n_iter=100,
            cv=3,
            verbose=0,
            random_state=42,
            n_jobs=-1)
        rf_random.fit(self.features_processed, self.response)
        # print(rf_random.best_params_)

        self.ran_params = rf_random.best_params_

    def train_rf(self, **kwargs):
        model = RandomForestRegressor(
            n_estimators=self.ran_params["n_estimators"],
            min_samples_leaf=self.ran_params["min_samples_leaf"],
            min_samples_split=self.ran_params["min_samples_split"],
            max_features=self.ran_params["max_features"],
            max_depth=self.ran_params["max_depth"],
            bootstrap=self.ran_params["bootstrap"])

        model.fit(self.features_processed, self.response)
        self.model = model
        
        return model

    def run_CVs(self, **kwargs):
        df, fold_indices = super().run_CVs(
            model_type=RandomForestRegressor(
                n_estimators=self.ran_params["n_estimators"],
                min_samples_leaf=self.ran_params["min_samples_leaf"],
                min_samples_split=self.ran_params["min_samples_split"],
                max_features=self.ran_params["max_features"],
                max_depth=self.ran_params["max_depth"],
                bootstrap=self.ran_params["bootstrap"]),
            features=kwargs["features"],
            response=kwargs["response"],
            n_folds=kwargs["n_folds"],
            title=kwargs["title"],
            visualize=kwargs["visualize"],
            rescale_type=kwargs["rescale_type"])

        return df, fold_indices

    def feature_importance(self, **kwargs):

        importances = self.model.feature_importances_
        self.feature_importance_std = np.std([tree.feature_importances_ for tree in self.model.estimators_], axis=0)
        return importances
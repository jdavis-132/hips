import pandas as pd
import numpy as np
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import GroupKFold
from sklearn.metrics import mean_squared_error
from sklearn import preprocessing
import base
import RF

# Number of folds for CV
k = 5

# Load your dataset
# Assuming you have a DataFrame named 'data' with columns: plantDensity, kernelRowNumber, kernelsPerRow, hundredKernelMass, yieldPerAcre
# Adjust the file path or loading mechanism as needed
data = pd.read_csv("../analysis/HYBRIDS_2022_2023_SPATIALLYCORRECTED.csv")
data = data[['yieldPerAcre.sp', 'genotype', 'environment', 'plotNumber', 'plantDensity.sp', 'kernelRowNumber.sp', 'kernelsPerRow.sp', 'hundredKernelMass.sp']]
data = data.dropna()

environments = data['environment'].unique()

predictions_ds = pd.DataFrame(columns = ['environment', 'plotNumber', 'predictedYieldRF'])
importance_ds = pd.DataFrame(columns = ['environment', 'plantDensity.sp', 'kernelRowNumber.sp', 'kernelsPerRow.sp', 'hundredKernelMass.sp'])

for env in environments:
    env_ds = data[data['environment']==env]
    genotype = env_ds['genotype']
    group_kfold = GroupKFold(n_splits = k)
    
    splits = group_kfold.split(X = env_ds, groups = genotype)
    
    for train_idx, test_idx in splits:
        train_ds = env_ds[train_idx]
        test_ds = env_ds[test_idx]
        
        train_features = train_ds[['plantDensity.sp', 'kernelRowNumber.sp', 'kernelsPerRow.sp', 'hundredKernelMass.sp']]
        train_response = train_ds['yieldPerAcre.sp']
        
        test_features = test_ds[['plantDensity.sp', 'kernelRowNumber.sp', 'kernelsPerRow.sp', 'hundredKernelMass.sp']]
        test_features = preprocessing.StandardScaler().fit_transform(test_features)
        test_response = test_ds['yieldPerAcre.sp']
        test_plotNumbers = test_ds['plotNumber']
        
        model = RF(response = train_response, features = train_features, rescale_type = 'norm')
        model.grid_search()
        model = model.train_rf(response = train_response, features = train_features)
        
        predictions = model.predict(test_features)
        predictions = predictions.flatten()
        
        fold_predictions = pd.DataFrame({'environment': [env]*len(predictions),
                                       'plotNumber':test_plotNumbers,
                                       'predictedYield': predictions})
        predictions.append(fold_predictions)
        
        importance = model.feature_importance()
        importance = np.concatenate(([env], importance))
        
        importance_ds.append(importance, ignore_index=True)
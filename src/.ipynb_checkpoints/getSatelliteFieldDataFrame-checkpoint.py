import pandas as pd

# Get vectors of possible range and row values for each field section
cf4351Ranges = range(2, 24)
cf4351Rows = range(13, 21)
cf4352Ranges = cf4351Ranges
cf4352Rows = range(2, 10)
cf4353Ranges = range(3, 37)
cf4353Rows = range(42, 47)
ames4231Ranges = range(2, 32)
ames4231Rows = range(2, 8)
ames4232Ranges = ames4231Ranges
ames4232Rows = range(17, 23)
ames4233Ranges = range(3, 25)
ames4233Rows = range(3, 11)
mvRanges = range(2, 46)
mvRows = [23, 25, 27, 29]
lnkRanges = range(2, 16)
lnkRows = range(2, 40)
np1Rows = range(1, 17)
np1Ranges = range(1, 36)
np2Rows = np1Rows
np2Ranges = np1Ranges
np3Rows = range(1, 23)
np3Ranges = range(1, 30)
sbRanges = range(3, 28)
sbLowRows = range(1, 8)
sbMedRows = range(11, 18)
sbHighRows = range(20, 27)

# Get possible plot coordinates for each field section as string in the format range-row
cf4351Plots = []
for i in cf4351Ranges:
	for j in cf4351Rows:
		plotCoordinates = str(i) + '-' + str(j)
		cf4351Plots.append(plotCoordinates)
cf4352Plots = []
for i in cf4352Ranges:
	for j in cf4352Rows: 
		plotCoordinates = str(i) + '-' + str(j)
		cf4352Plots.append(plotCoordinates)
cf4353Plots = []
for i in cf4353Ranges:
	for j in cf4353Rows:
		plotCoordinates = str(i) + '-' + str(j)
		cf4353Plots.append(plotCoordinates)
ames4231Plots = []
for i in ames4231Ranges:
	for j in ames4231Rows:
		plotCoordinates = str(i) + '-' + str(j)
		ames4231Plots.append(plotCoordinates)
ames4232Plots = []
for i in ames4232Ranges:
	for j in ames4232Rows:
		plotCoordinates = str(i) + '-' + str(j)
		ames4232Plots.append(plotCoordinates)
ames4233Plots = []
for i in ames4233Ranges:
	for j in ames4233Rows:
		plotCoordinates = str(i) + '-' + str(j)
		ames4233Plots.append(plotCoordinates)
mvPlots = []
for i in mvRanges:
	for j in mvRows:
		plotCoordinates = str(i) + '-' + str(j)
		mvPlots.append(plotCoordinates)
lnkPlots = []
for i in lnkRanges:
	for j in lnkRows:
		plotCoordinates = str(i) + '-' + str(j)
		lnkPlots.append(plotCoordinates)
np1Plots = []
for i in np1Ranges:
	for j in np1Rows:
		plotCoordinates = str(i) + '-' + str(j)
		np1Plots.append(plotCoordinates)
np2Plots = []
for i in np2Ranges:
	for j in np2Rows:
		plotCoordinates = str(i) + '-' + str(j)
		np2Plots.append(plotCoordinates)
np3Plots = []
for i in np3Ranges:
	for j in np3Rows:
		plotCoordinates = str(i) + '-' + str(j)
		np3Plots.append(plotCoordinates)
sbLowPlots = []
sbMedPlots = []
sbHighPlots = []
for i in sbRanges:
	for j in sbLowRows:
		plotCoordinates = str(i) + '-' + str(j)
		sbLowPlots.append(plotCoordinates)
	for j in sbMedRows:
		plotCoordinates = str(i) + '-' + str(j)
		sbMedPlots.append(plotCoordinates)
	for j in sbHighRows:
		plotCoordinates = str(i) + '-' + str(j)
		sbHighPlots.append(plotCoordinates)
		
# Read in hybrid HIPS data
hybridData = pd.read_csv('../outData/HIPS_2022_V3.5_HYBRIDS.csv', header=0, index_col=False)

# Create column with same syntax as possible plot coordinate lists
hybridData['rangeRow'] = hybridData['range'].astype(str) + '-' + hybridData['row'].astype(str)

# Get lists of plots in the data frame for each field section.
expCF4351Plots = hybridData[(hybridData['location']=='Crawfordsville') & (hybridData['experiment']=='LC_4351')]
expCF4352Plots = hybridData[(hybridData['location']=='Crawfordsville') & (hybridData['experiment']=='LC_4352')]
expCF4353Plots = hybridData[(hybridData['location']=='Crawfordsville') & (hybridData['experiment']=='LC_4353')]
expAmes4231Plots = hybridData[(hybridData['location']=='Ames') & (hybridData['experiment']=='LC_4231')]
expAmes4232Plots = hybridData[(hybridData['location']=='Ames') & (hybridData['experiment']=='LC_4232')]
expAmes4233Plots = hybridData[(hybridData['location']=='Ames') & (hybridData['experiment']=='LC_4233')]
expMVPlots = hybridData[hybridData['location']=='Missouri Valley']
expLNKPlots = hybridData[hybridData['location']=='Lincoln']
expNP1Plots = hybridData[hybridData['location']=='North Platte1']
expNP2Plots = hybridData[hybridData['location']=='North Platte2']
expNP3Plots = hybridData[hybridData['location']=='North Platte3']
expSBLowPlots = hybridData[(hybridData['location']=='Scottsbluff') & (hybridData['nitrogenTreatment']=='Low')]
expSBMedPlots = hybridData[(hybridData['location']=='Scottsbluff') & (hybridData['nitrogenTreatment']=='Medium')]
expSBHighPlots = hybridData[(hybridData['location']=='Scottsbluff') & (hybridData['nitrogenTreatment']=='High')]

# Get list of which plots are missing for each field section
cf4351MissingPlots = [i for i in cf4351Plots if i not in expCF4351Plots['rangeRow'].values]
cf4352MissingPlots = [i for i in cf4352Plots if i not in expCF4352Plots['rangeRow'].values]
cf4353MissingPlots = [i for i in cf4353Plots if i not in expCF4353Plots['rangeRow'].values]
ames4231MissingPlots = [i for i in ames4231Plots if i not in expAmes4231Plots['rangeRow'].values]
ames4232MissingPlots = [i for i in ames4232Plots if i not in expAmes4232Plots['rangeRow'].values]
ames4233MissingPlots = [i for i in ames4233Plots if i not in expAmes4233Plots['rangeRow'].values]
mvMissingPlots = [i for i in mvPlots if i not in expMVPlots['rangeRow'].values]
lnkMissingPlots = [i for i in lnkPlots if i not in expLNKPlots['rangeRow'].values]
np1MissingPlots = [i for i in np1Plots if i not in expNP1Plots['rangeRow'].values]
np2MissingPlots = [i for i in np2Plots if i not in expNP2Plots['rangeRow'].values]
np3MissingPlots = [i for i in np3Plots if i not in expNP3Plots['rangeRow'].values]
sbLowMissingPlots = [i for i in sbLowPlots if i not in expSBLowPlots['rangeRow'].values]
sbMedMissingPlots = [i for i in sbMedPlots if i not in expSBMedPlots['rangeRow'].values]
sbHighMissingPlots = [i for i in sbHighPlots if i not in expSBHighPlots['rangeRow'].values]

# Get dataframes of missing plots with location and field section if applicable
cf4351MissingPlots = pd.DataFrame(data = {'location':'Crawfordsville', 'experiment':'LC_4351', 'rangeRow':cf4351MissingPlots})
cf4351MissingPlots['range'] = cf4351MissingPlots['rangeRow'].str.split('-').str.get(0).astype(int)
cf4351MissingPlots['row'] = cf4351MissingPlots['rangeRow'].str.split('-').str.get(1).astype(int)
cf4352MissingPlots = pd.DataFrame(data = {'location':'Crawfordsville', 'experiment':'LC_4352', 'rangeRow':cf4352MissingPlots})
cf4352MissingPlots['range'] = cf4352MissingPlots['rangeRow'].str.split('-').str.get(0).astype(int)
cf4352MissingPlots['row'] = cf4352MissingPlots['rangeRow'].str.split('-').str.get(1).astype(int)
cf4353MissingPlots = pd.DataFrame(data = {'location':'Crawfordsville', 'experiment':'LC_4353', 'rangeRow':cf4353MissingPlots})
cf4353MissingPlots['range'] = cf4353MissingPlots['rangeRow'].str.split('-').str.get(0).astype(int)
cf4353MissingPlots['row'] = cf4353MissingPlots['rangeRow'].str.split('-').str.get(1).astype(int)
ames4231MissingPlots = pd.DataFrame(data = {'location':'Ames', 'experiment':'LC_4231', 'rangeRow':ames4231MissingPlots})
ames4231MissingPlots['range'] = ames4231MissingPlots['rangeRow'].str.split('-').str.get(0).astype(int)
ames4231MissingPlots['row'] = ames4231MissingPlots['rangeRow'].str.split('-').str.get(1).astype(int)
ames4232MissingPlots = pd.DataFrame(data = {'location':'Ames', 'experiment':'LC_4232', 'rangeRow':ames4232MissingPlots})
ames4232MissingPlots['range'] = ames4232MissingPlots['rangeRow'].str.split('-').str.get(0).astype(int)
ames4232MissingPlots['row'] = ames4232MissingPlots['rangeRow'].str.split('-').str.get(1).astype(int)
ames4233MissingPlots = pd.DataFrame(data = {'location':'Ames', 'experiment':'LC_4233', 'rangeRow':ames4233MissingPlots})
ames4233MissingPlots['range'] = ames4233MissingPlots['rangeRow'].str.split('-').str.get(0).astype(int)
ames4233MissingPlots['row'] = ames4233MissingPlots['rangeRow'].str.split('-').str.get(1).astype(int)
mvMissingPlots = pd.DataFrame(data = {'location':'Missouri Valley', 'experiment':'LC_4211', 'rangeRow':mvMissingPlots})
mvMissingPlots['range'] = mvMissingPlots['rangeRow'].str.split('-').str.get(0).astype(int)
mvMissingPlots['row'] = mvMissingPlots['rangeRow'].str.split('-').str.get(1).astype(int)
lnkMissingPlots = pd.DataFrame(data = {'location':'Lincoln', 'rangeRow':lnkMissingPlots})
lnkMissingPlots['range'] = lnkMissingPlots['rangeRow'].str.split('-').str.get(0).astype(int)
lnkMissingPlots['row'] = lnkMissingPlots['rangeRow'].str.split('-').str.get(1).astype(int)
np1MissingPlots = pd.DataFrame(data = {'location':'North Platte1', 'rangeRow':np1MissingPlots})
np1MissingPlots['range'] = np1MissingPlots['rangeRow'].str.split('-').str.get(0).astype(int)
np1MissingPlots['row'] = np1MissingPlots['rangeRow'].str.split('-').str.get(1).astype(int)
np2MissingPlots = pd.DataFrame(data = {'location':'North Platte2', 'rangeRow':np2MissingPlots})
np2MissingPlots['range'] = np2MissingPlots['rangeRow'].str.split('-').str.get(0).astype(int)
np2MissingPlots['row'] = np2MissingPlots['rangeRow'].str.split('-').str.get(1).astype(int)
np3MissingPlots = pd.DataFrame(data = {'location':'North Platte3', 'rangeRow':np3MissingPlots})
np3MissingPlots['range'] = np3MissingPlots['rangeRow'].str.split('-').str.get(0).astype(int)
np3MissingPlots['row'] = np3MissingPlots['rangeRow'].str.split('-').str.get(1).astype(int)
sbLowMissingPlots = pd.DataFrame(data = {'location':'Scottsbluff', 'nitrogenTreatment':'Low', 'rangeRow':sbLowMissingPlots})
sbLowMissingPlots['range'] = sbLowMissingPlots['rangeRow'].str.split('-').str.get(0).astype(int)
sbLowMissingPlots['row'] = sbLowMissingPlots['rangeRow'].str.split('-').str.get(1).astype(int)
sbMedMissingPlots = pd.DataFrame(data = {'location':'Scottsbluff', 'nitrogenTreatment':'Medium', 'rangeRow':sbMedMissingPlots})
sbMedMissingPlots['range'] = sbMedMissingPlots['rangeRow'].str.split('-').str.get(0).astype(int)
sbMedMissingPlots['row'] = sbMedMissingPlots['rangeRow'].str.split('-').str.get(1).astype(int)
sbHighMissingPlots = pd.DataFrame(data = {'location':'Scottsbluff', 'nitrogenTreatment':'High', 'rangeRow':sbHighMissingPlots})
sbHighMissingPlots['range'] = sbHighMissingPlots['rangeRow'].str.split('-').str.get(0).astype(int)
sbHighMissingPlots['row'] = sbHighMissingPlots['rangeRow'].str.split('-').str.get(1).astype(int)

# Concatenate dataframes of missing plots onto hybridData
allPlots = pd.concat([hybridData, cf4351MissingPlots, cf4352MissingPlots, cf4353MissingPlots, ames4231MissingPlots, ames4232MissingPlots,
					 ames4233MissingPlots, mvMissingPlots, lnkMissingPlots, np1MissingPlots, np2MissingPlots, np3MissingPlots, 
					  sbLowMissingPlots, sbMedMissingPlots, sbHighMissingPlots])

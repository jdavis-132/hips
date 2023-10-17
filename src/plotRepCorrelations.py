import pandas as pd
import matplotlib as mlp
import matplotlib.pyplot as plt

hybridData = pd.read_csv('../outData/HIPS_2022_V3.5_HYBRIDS.csv', index_col = False)

def plotRepCorrelations(data, trait):
    # Select only the columns we need and create an identifier column
    df = data[['location', 'nitrogenTreatment', 'genotype', 'block', trait]]
    df['locationNitrogenGenotype'] = df['location'].astype(str) + '-' + df['nitrogenTreatment'].astype(str) + '-' + df['genotype'].astype(str)
    # Create a dataframe for block1 and block 2; drop the block column
    block1 = df[df['block']==1]
    block1 = block1[['locationNitrogenGenotype', trait]]
    block1.set_index('locationNitrogenGenotype')
    block2 = df[df['block']==2]
    block2 = block2[['locationNitrogenGenotype', trait]]
    block2.set_index('locationNitrogenGenotype')
    # Join the two block dataframes so we have trait1 and trait2 columns
    dfWide = block1.join(block2, lsuffix = '1', rsuffix = '2', validate = 'many_to_many')
    r = dfWide[(trait + '1')].corr(dfWide[(trait + '2')])
    r2 = r * r
    # Make the plot
    fig, ax = plt.subplots()
    ax.scatter(dfWide[(trait + '1')], dfWide[(trait + '2')], marker = '.')
    ax.set_xlabel(trait + '1', fontsize = 16)
    ax.set_ylabel(trait + '2', fontsize = 16)
    ax.set_title(trait + '\n' + '$R^2$:' + str(r2))
    plt.show()
    
    

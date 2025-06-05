# -*- coding: utf-8 -*-
"""
Python main script for the HIPS project (specifically inbreds), a collaboration between the UNL & ISU Schnable Labs.
For documentation of the dataset, please see the file under Github repository: "jdavis-132/hips/src/README_HYBRID_HIPS2022.html".
For the latest version of the dataset, please see the folder "jdavis-132/hips/outData" and choose the latest version.

Please follow this order of steps when running each function. Its important to keep each step in order and check the output .csv
file as each input file might be different depending if pre-requisites were followed correctly. Additionally, please familiarze 
yourself with the purpose and pre-requisites of all the functions in "inbred_HIPS_functions.py"

@author: RJGrove
"""
# Import all functions individually from 'inbred_HIPS_functions.py'
import pandas as pd
from inbred_HIPS_functions import (
    full_join_pedigreeID, copyandpaste_pedigreeID, convert_csv_dates,
    process_and_average_phenotypes, order_columns, calculate_dates,
    calculate_kernelMass_plantDensity, extract_qrCode_elements, extract_all,
    ensure_columns
)

def main():
    meta_df_path = "___________" # Add path to the meta dataset file you wish to save
    raw_df_path = "___________" # Add path to raw data file

    columns = [
        'qrCode', 'year', 'location', 'sublocation', 'irrigationProvided', 'nitrogenTreatment', 
        'poundsOfNitrogenPerAcre', 'experiment', 'plotLength', 'totalStandCount', 'block', 'row', 
        'range', 'plotNumber', 'genotype', 'pedigreeID', 'plantingDate', 'anthesisDate', 'silkDate', 
        'daysToAnthesis', 'daysToSilk', 'anthesisSilkingInterval', 'GDDToAnthesis', 'GDDToSilk', 
        'anthesisSilkingIntervalGDD', 'earHeight', 'flagLeafHeight', 'plantDensity', 'earLength', 
        'earFillLength', 'earWidth', 'shelledCobWidth', 'kernelsPerRow', 'kernelRowNumber', 
        'kernelsPerEar', 'hundredKernelMass', 'kernelMassPerEar', 'shelledCobMass', 'percentMoisture', 
        'percentStarch', 'percentProtein', 'percentOil', 'percentFiber', 'percentAsh', 'kernelColor', 
        'percentLodging', 'harvestDate', 'notes'
    ]

    # Step 1: Create and write into a new dataframe (meta_df)
    meta_df = pd.DataFrame(columns=columns)
    meta_df.to_csv(meta_df_path, index=False)

    # Step 2: Extract uniqueIDs from raw_df and input into meta_df
    raw_df = pd.read_csv(raw_df_path)
    raw_df['qrCode'] = raw_df['uniqueID'].astype(str)
    
    # Initialize meta_df with the qrCode column
    meta_df = pd.DataFrame(raw_df['qrCode'])
    meta_df.to_csv(meta_df_path, index=False)
  
    extract_qrCode_elements(meta_df_path)
    meta_df = pd.read_csv(meta_df_path) 
    ensure_columns(meta_df, columns)
    meta_df.to_csv(meta_df_path, index=False)

    # Step 3: Extract all possible information from the raw_df and input into meta_df
    extract_all(meta_df_path, raw_df_path)
    meta_df = pd.read_csv(meta_df_path)

    # Step 4: Capitalize 'qrCode' and 'genotype'
    meta_df['qrCode'] = meta_df['qrCode'].str.upper()
    meta_df['genotype'] = meta_df['genotype'].astype(str)
    meta_df['genotype'] = meta_df['genotype'].str.upper()
    meta_df.to_csv(meta_df_path, index=False)

    # Step 5: Format all dates in the meta_df
    convert_csv_dates(meta_df_path)    
    meta_df = pd.read_csv(meta_df_path)

    # Step 6: Calculate all averages for the ear phenotype data taken from raw_df and input them into meta_df
    process_and_average_phenotypes(meta_df_path)    
    meta_df = pd.read_csv(meta_df_path) 

    # Step 7: Calculate 'daysToAnthesis' and 'daysToSilk'
    calculate_dates(meta_df_path)
    meta_df = pd.read_csv(meta_df_path) 

    # Step 8: Calculate 'kernelMassPerEar' and 'plantDensity'
    calculate_kernelMass_plantDensity(meta_df_path)
    meta_df = pd.read_csv(meta_df_path)
    
    # Step 9: Assign 'pedigreeID' based off of similar values in 'genotype' across the entire dataframe
    full_join_pedigreeID(raw_df_path, meta_df_path)
    meta_df = pd.read_csv(meta_df_path) 
    copyandpaste_pedigreeID(meta_df_path)
    meta_df = pd.read_csv(meta_df_path) 

    # Step 10: Order columns to ensure standardization across all the years
    order_columns(meta_df_path)
    meta_df = pd.read_csv(meta_df_path) 

if __name__ == "__main__":
    main()


    




    

# -*- coding: utf-8 -*-
"""
Python functions script for the HIPS project (specifically inbreds), a collaboration between the UNL & ISU Schnable Labs.
For documentation of the dataset, please see the file under Github repository: "jdavis-132/hips/src/README_HYBRID_HIPS2022.html".
For the latest version of the dataset, please see the folder "jdavis-132/hips/outData" and choose the latest version.

@author: RJGrove
"""

import pandas as pd
from datetime import datetime
import re

#######################################################################################################################################################
"""
Purpose: Complete a full join of the "pedigreeID" column for the raw data to the meta data.
Pre-requisites: Meta and raw dataset in .csv format with the columns qrCode and pedigreeID
"""
def full_join_pedigreeID(raw_df_path, meta_df_path):
    try:
        raw_df = pd.read_csv(raw_df_path)
        meta_df = pd.read_csv(meta_df_path)

        joined_df = pd.merge(meta_df, raw_df, on='qrCode', how='left')
        print("Full join of 'pedigreeID' between raw data and meta data is complete.")
        return joined_df

    except FileNotFoundError:       
        print(f"Error: One of the files '{raw_df_path}' or '{meta_df_path}' does not exist. Please double-check file path and try again.")
        return None

#######################################################################################################################################################
"""
Purpose: Use the "pedigreeID" values tied to the genotypes in the "Ames" 2023 location to paste to similar genotypes
         in all the 2022 dataset.          
Pre-requisites: Meta dataset in .csv format with the following columns: genotypes, pedigreeID, location, and year
"""
def copyandpaste_pedigreeID(meta_df_path):    
    try:        
        df = pd.read_csv(meta_df_path)
        
        ames_2023 = df[(df['location'] == 'Ames') & (df['year'] == 2023)]
        
        def fill_pedigree(row):
            if row['year'] == 2022 and pd.isna(row['pedigreeID']):
                match = ames_2023[ames_2023['genotype'] == row['genotype']]
                
                if not match.empty:
                    return match['pedigreeID'].values[0]
            
            return row['pedigreeID']
        
        df['pedigreeID'] = df.apply(fill_pedigree, axis=1)
        df.to_csv(meta_df_path, index=False)
        print("'pedigreeID' copied from 2023 to 2022 successfully.")
        
    except FileNotFoundError:        
        print(f"Error: The file '{meta_df_path}' does not exist or has an error.")
    
#######################################################################################################################################################
"""
Purpose: Standardize all the date formats to "yyyy-mm-dd". Its important to note that when loading the file
         in Excel, the date format will go off of your computer system rather than the file specifics. To
         check if the dates are properly formatted, please refer to the printed output in the Console rather
         than loading the file itself.         
Pre-requisites: Meta dataset in .csv format    
""" 
def convert_csv_dates(meta_df_path):    
    try:
        def convert_date(date_str):
            if date_str.strip():  
                try:
                    return datetime.strptime(date_str, "%m/%d/%Y").strftime("%Y-%m-%d")
                except ValueError:
                    return date_str
            else:
                return date_str 

        df = pd.read_csv(meta_df_path)

        df = df.applymap(str)

        for column in df.columns:
            if pd.api.types.is_string_dtype(df[column]):
                df[column] = df[column].apply(convert_date)

        df = df.replace('nan', '')

        df.to_csv(meta_df_path, index=False)
        print("All dates standardized to 'yyyy-mm-dd' successfully. Please refer to output in the Console to validate the results.")
   
    except FileNotFoundError:
        print(f"Error: The file '{meta_df_path}' does not exist.")
    
    except Exception as e:
        print(f"An error occurred: {e}")


#######################################################################################################################################################
"""
Purpose: Processes a CSV file to average specific columns for each unique QR code. It saves the averaged results 
         along with the first observed 'kernelColor' for each QR code into a new CSV file.         
Pre-requisites: Meta dataset in .csv format with the following columns: qrCode, kernelColor, earWidth, earFillLength, kernelRowNumber, earMass,
                shelledCobWidth, earLength, shelledCobMass, hundredKernelMass, kernelsPerEar
"""
def process_and_average_phenotypes(meta_df_path):
    try:
        df = pd.read_csv(meta_df_path)

        qr_column = 'qrCode'
        columns_to_average = ['earWidth', 'earFillLength', 'kernelRowNumber', 'kernelsPerRow', 
                              'earMass', 'shelledCobWidth', 'earLength', 'shelledCobMass', 
                              'hundredKernelMass', 'kernelsPerEar']

        grouped = df.groupby(qr_column)
        
        for col in columns_to_average:
            df[col] = grouped[col].transform('mean')
        
        df['kernelColor'] = grouped['kernelColor'].transform('first')
        
        df.to_csv(meta_df_path, index=False)
        print(f"Data has been processed and saved to '{meta_df_path}'.")

    except FileNotFoundError:
        print(f"Error: The file '{meta_df_path}' does not exist.")
        
    except Exception as e:
        print(f"An error occurred: {e}")

#######################################################################################################################################################
"""
Purpose: Reorder all the data columns to standardize across years.
Pre-requisites: Meta dataset in .csv format
"""
def order_columns(meta_df_path):
    try:
        df = pd.read_csv(meta_df_path)
    
        # Define the desired column order
        column_order = [
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
    
        df = df.reindex(columns=column_order)
        df.to_csv(meta_df_path, index=False)
    
        print(f"Data columns have been reordered and saved to '{meta_df_path}'.")
        
    except FileNotFoundError:        
        print(f"Error: The file '{meta_df_path}' does not exist or has an error.")
        
#######################################################################################################################################################
"""
Purpose: Calculate days for 'daysToAnthesis' and 'daysToSilk' using the number of days between 'anthesisDate'/'silkDate' and 'plantingDate'.
Pre-requisties: Meta dataset in .csv format with the following columns: anthesisDate, silkDate, daysToAnthesis, plantingDate, and daysToSilk
"""
def calculate_dates(meta_df_path):
    try:
        df = pd.read_csv(meta_df_path)
        date_format = "%Y-%m-%d"

        df['anthesisDate'] = pd.to_datetime(df['anthesisDate'], format=date_format, errors='coerce')
        df['silkDate'] = pd.to_datetime(df['silkDate'], format=date_format, errors='coerce')
        df['plantingDate'] = pd.to_datetime(df['plantingDate'], format=date_format, errors='coerce')

        df['daysToAnthesis'] = (df['anthesisDate'] - df['plantingDate']).dt.days
        df['daysToSilk'] = (df['silkDate'] - df['plantingDate']).dt.days
        
        df.to_csv(meta_df_path, index=False)
        print(f"daysToAnthesis and daysToSilk have been saved in '{meta_df_path}' successfully.")

    except FileNotFoundError:        
        print(f"Error: The file '{meta_df_path}' does not exist or has an error.")
    
    except Exception as e:
        print(f"An error occurred: {e}")

#######################################################################################################################################################
"""
Purpose: Calculate the values for 'kernelMassPerEar' as well as 'plantDensity'
Pre-requisties: Meta dataset in .csv format with the following columns: earWeight, shelledCobMass, kernelMassPerEar, plantDensity,
                totalStandCount, and plotLength
"""
def calculate_kernelMass_plantDensity(meta_df_path):
    try:
        df = pd.read_csv(meta_df_path)

        df['kernelMassPerEar'] = df['earWeight'] - df['shelledCobMass']
        
        df['plantDensity'] = ((df['totalStandCount'] / 2) / (df['plotLength'] / 17.5)) * 1000
        
        df.to_csv(meta_df_path, index=False)
        print(f"kernelMassPerEar and plantDensity have been saved in '{meta_df_path}' successfully.")

    except FileNotFoundError:        
        print(f"Error: The file '{meta_df_path}' does not exist or has an error.")
    
    except Exception as e:
        print(f"An error occurred: {e}")

#######################################################################################################################################################
"""
Purpose: Extract uniqueIDs from raw_df and input into meta_df; additionally, add location based on qrCode
Pre-requisties: Meta dataset with qrCode column and raw dataset with uniqueIDs column, both in .csv format 
"""
def extract_qrCode_elements(meta_df_path):
    try:
        df = pd.read_csv(meta_df_path)

        # Add location based on value in qrCode
        def get_location(qr_code):
            if 'lincoln' in qr_code.lower():
                return 'Lincoln'
            elif '-a-' in qr_code.lower():
                return 'Ames'
            elif '-c-' in qr_code.lower():
                return 'Crawfordsville'
            elif '-m-' in qr_code.lower():
                return 'Missouri Valley'
            else:
                return ''

        df['location'] = df['qrCode'].apply(get_location)

        # Add block as the number after "REP" in qrCode
        def extract_rep_number(qr_code):
            match = re.search(r'REP(\d+)', qr_code)
            return match.group(1) if match else None

        df['block'] = df['qrCode'].apply(extract_rep_number)

        # Add row as the number after "ROW" in qrCode
        def extract_row_number(qr_code):
            match = re.search(r'ROW(\d+)', qr_code)
            return match.group(1) if match else None
        
        df['row'] = df['qrCode'].apply(extract_row_number)
        
        # Add range as the number after "RANGE" in qrCode
        def extract_range_number(qr_code):
            match = re.search(r'RANGE(\d+)', qr_code)
            return match.group(1) if match else None

        df['range'] = df['qrCode'].apply(extract_range_number)
        
        # Add plotNumber as the number after "PLOT" in qrCode
        def extract_plot_number(qr_code):
            match = re.search(r'PLOT(\d+)', qr_code)
            return match.group(1) if match else None

        df['plotNumber'] = df['qrCode'].apply(extract_plot_number)

        df.to_csv(meta_df_path, index=False)
        print(f"All locations, blocks, rows, ranges, and plotNumbers have been extracted from the qrCode in '{meta_df_path}' successfully.")

    except ValueError as ve:
        print(f"ValueError: {ve}")
    
    except Exception as e:
        print(f"An unexpected error occurred: {e}")

#######################################################################################################################################################
"""
Purpose: Extract all information from the raw_df and input into meta_df
Pre-requisties: Meta and raw dataset, both in .csv format
Important Note: All functions below have been specifically written to use on Iowa 2023 raw data. To use on a different location,
                file, or year, make sure to change the column names to what your data specifies. Only change the column in the 
                situations/lines that call from the raw_df - do not change the column names for meta-df.
"""
def extract_all(meta_df_path, raw_df_path):
    try:
        meta_df = pd.read_csv(meta_df_path)
        raw_df = pd.read_csv(raw_df_path)

        # Add nitrogen treatment from raw_df to meta_df
        meta_df['nitrogenTreatment'] = raw_df['N level']
        
        # Add flagLeafHeight and earHeight from raw_df to meta_df
        meta_df['flagLeafHeight'] = raw_df['Plant height (cm)']
        meta_df['earHeight'] = raw_df['Ear height (cm)']          
        
        # Add plantingDate from raw_df to meta_df
        meta_df['plantingDate'] = raw_df['Planting Date']
        
        for genotype in raw_df['Genotype']:
            
            if genotype not in meta_df['genotype'].values:
                meta_df['genotype'] = raw_df['Genotype']
        
        meta_df.to_csv(meta_df_path, index=False)
        print(f"All information from the '{raw_df_path}' has been inputted into '{meta_df_path}' successfully.")

    except FileNotFoundError:        
        print(f"Error: The file '{meta_df_path}' or '{raw_df_path}' does not exist or has an error.")
    
    except Exception as e:
        print(f"An unexpected error occurred: {e}")
        
#######################################################################################################################################################
"""
Purpose: Ensure all the columns in the meta_df are still present and in order
Pre-requisties: Meta dataset, both in .csv format
"""
def ensure_columns(meta_df, columns):

   for col in columns:
       if col not in meta_df.columns:
           meta_df[col] = pd.NA
   return meta_df[columns] 

#######################################################################################################################################################

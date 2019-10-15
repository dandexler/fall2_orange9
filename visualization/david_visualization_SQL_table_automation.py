##############################################################
#
# David Andexler
# Institute for Advanced Analytics - NCSU
# Visualization Project - Automated SQL Database Generator
#
##############################################################

import os
import pandas as pd
import sqlite3 as sql
from sqlite3 import Error
from tkinter import filedialog
from tkinter import *


# Create list of year range for data
# Adjust based on data needs
# This will be used to create the table names
years = [x for x in range(2000, 2018)]


def main():
    # Explicitly sets Connection object to null
    Connection = None

    # Local database selection prompt
    # Select the database that is getting new tables
    try:
        root = Tk()
        root.withdraw()
        db_file = filedialog.askopenfilename(title='Select database: ')
    except:
        print('DBImport error.')
        quit()

    # Establish connection with DB
    try:
        Connection = sql.connect(db_file)
    except Error as e:
        print('Invalid connection.')
        print('Error reference:', e)
    print('Connection initialized.')
    # Connection object cursor
    c = Connection.cursor()

    # Data directory prompt
    try:
        directory = filedialog.askdirectory(title='Select directory with ALL '
                                                  'data:') + '/'
    except:
        print('Unknown error.')
        quit()

    # Gets names of all files in the selected data directory
    data = [directory + filename for filename in os.listdir(directory) if filename.endswith('.csv')]
    print("CSV filenames read in:", data)

    # Variable selection. Group-agreed method: Intersection of all tables.
    # All SQL tables will be initialized using common_variables,
    # the variables that all tables have.
    variables = []
    for i in range(len(data)):
        print('Reading in av_{}'.format(years[i]))
        df = pd.read_csv(data[i], encoding='latin')
        variables.append(set(df.columns))
    for i in range(len(variables)):
        print('\n')
        print('Year', years[i])
        print('Variable Count:', len(variables[i]))
        print(variables[i])
    print('Intersection of all variables:')
    common_variables = set.intersection(*variables)
    print('Length of intersection:', len(common_variables))
    print(common_variables)
    common_variables = list(common_variables)

    # Extracts column names from common_variables
    print('Columns read in:', common_variables)

    # Reads in each CSV data file, converts to pandas data frame, then reads
    # it to SQL database
    for i in range(len(years)):
        df = pd.read_csv(data[i], encoding='latin')
        df = df[common_variables]
        print('Reading file:', data[i])
        df.to_sql(name='av_{}'.format(years[i]), con=Connection,
                  if_exists='replace')

    # Close connection with DB
    print('Process complete.')
    Connection.close()

main()
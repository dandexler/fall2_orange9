import sqlite3 as sql
from sqlite3 import Error
import csv
from tkinter import filedialog
from tkinter import *


def main():
    # Explicitly sets Connection object to null
    Connection = None

    # Location of local DB
    try:
        root = Tk()
        root.withdraw()
        db_file = filedialog.askopenfilename(title="Select database: ")
        print(db_file)
    except:
        print("FileImport error.")
        quit()

    # Establish connection with DB
    try:
        Connection = sql.connect(db_file)
        print("Connection initialized.")
    except Error as e:
        print("Invalid connection.")
        print("Error reference:", e)



    # Reads in CSV file, reads first line to extract column names
    try:
        first_file = filedialog.askopenfilename(title="Select file to read "
                                                  "columns:")
    except:
        print("Error.")
        quit()

    with open(first_file, 'r') as file:
        reader = csv.reader(file)
        column_names = next(reader)
        column_names = ','.join(map(str, column_names))

    # Assumes that we want to create tables for 2000-2017 inclusive. Must
    # change below line if subsetting data
    years = [x for x in range(2000, 2018)]
    print("Columns read in:", column_names)
    print("Years:", years)

    # Create table for each year, reading in column names
    c = Connection.cursor()

    # Creates Tables for each year using the column names extracted previously

    # Delete table if name exists, uncomment and use this code if for some
    # reason you run this multiple times
    #for year in years:
    #   c.execute("DROP TABLE av_{}".format(year))

    # Creates table for each year
    for year in years:
        c.execute("CREATE TABLE av_{} ({})".format(year, column_names))

    # Close connection with DB
    Connection.close()

main()
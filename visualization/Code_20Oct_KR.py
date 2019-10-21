#!/usr/bin/env python
# coding: utf-8

# In[2]:


import pandas as pd
import numpy as np
import os
import csv

os.chdir(r"C:\Users\karud\Documents\MSA_Fall2\Visualization")

tblAcc = pd.read_csv("Accident_EDITED.csv")
tblAcc.head(10)
print(tblAcc)

# only want NC and years 2010-2017
#TODO: how to combine both filters in one step?
tblAccSub = tblAcc[tblAcc.STATE == 'North Carolina'] 
tblAccSub = tblAccSub[tblAccSub.YEAR > 2009]
tblAccSub.head(10)
print(tblAccSub)

# check unique values for columns that need binning

tblAccSub.WEATHER.unique()

# combine similar values

tblAccSub['WEATHER'] = tblAccSub['WEATHER'].replace(['Not Reported', 'Unknown'], 'NA')

tblAccSub.WEATHER.unique()

# check unique values for columns that need binning

tblAccSub.WRK_ZONE.unique()

# combine similar values

tblAccSub['WRK_ZONE'] = tblAccSub['WRK_ZONE'].replace(['Not Reported', 'None', 'Work Zone, Type Unknown'], 'NA')

tblAccSub.WRK_ZONE.unique()

tblAccSub.head(10)


# now to add the county and city names

# step 1: add table with values

tblCodes = pd.read_csv("city_codes.csv")

# only care about NC

tblCodesSub = tblCodes[tblCodes.State_Name == 'NORTH CAROLINA'] 

# get county

tblCodesCounty = tblCodesSub[['COUNTY', 'County_Name']]
tblCodesCounty.drop_duplicates()

tblCodesCounty.head(10)

# get city

tblCodesCity = tblCodesSub[['CITY', 'City_Name']]

tblCodesCity.head(10)

# merge those tables

tblCodesAll = pd.merge(tblAccSub, tblCodesCity.drop_duplicates(), on='CITY', how='left')
tblCodesAll.head(100)

tblCodesAll = pd.merge(tblCodesAll, tblCodesCounty.drop_duplicates(), on='COUNTY', how='left')
tblCodesAll.head(100)

print(tblCodesAll)
# Consider: might want to delete the codes in CITY and COUNTY now or later
# Idea: Add number for population ranking of county and category for region

# output a CSV file

# tblCodesAll.to_csv(r"C:\Users\karud\Documents\MSA_Fall2\Visualization\AccidentsEdit_Oct17b.csv")


# In[40]:


# add lists for each region to be referenced later when we add a new column

WESTERN = ['BUNCOMBE', 'CHEROKEE', 'CLAY', 'GRAHAM', 'HAYWOOD', 'HENDERSON', 'JACKSON', 'MACON', 'MADISON', 'POLK', 'RUTHERFORD', 'SWAIN', 'TRANSYLVANIA']


NORTHWEST= ['ALLEGHANY', 'ASHE', 'ALEXANDER', 'AVERY', 'BURKE', 'CALDWELL', 'CATAWBA', 'MCDOWELL', 'MITCHELL', 'WATAUGA', 'WILKES', 'YANCEY']

SOUTHWEST =['ANSON', 'CABARRUS', 'CLEVELAND', 'GASTON', 'IREDELL', 'LINCOLN', 'MECKLENBURG', 'ROWAN', 'STANLY', 'UNION']

TRIAD =['ALAMANCE', 'CASWELL', 'DAVIDSON', 'DAVIE', 'FORSYTH', 'GUILFORD', 'RANDOLPH', 'ROCKINGHAM', 'STOKES', 'SURRY', 'YADKIN']

NORTHCENTRAL=['CHATHAM', 'DURHAM', 'EDGECOMBE', 'FRANKLIN', 'GRANVILLE', 'HARNETT', 'JOHNSTON', 'LEE', 'NASH', 'ORANGE', 'PERSON', 'VANCE', 'WAKE', 'WARREN', 'WILSON']

SANDHILLS= ['BLADEN', 'COLUMBUS', 'CUMBERLAND', 'HOKE', 'MONTGOMERY', 'MOORE', 'RICHMOND', 'ROBESON', 'SAMPSON', 'SCOTLAND']

NORTHEAST = ['BEAUFORT', 'BERTIE', 'CAMDEN', 'CHOWAN', 'CURRITUCK', 'DARE', 'GATES', 'HALIFAX', 'HERTFORD', 'HYDE', 'MARTIN', 'NORTHAMPTON', 'PASQUOTANK', 'PERQUIMANS', 'PITT', 'TYRRELL', 'WASHINGTON']

SOUTHEAST=['BRUNSWICK', 'CARTERET', 'CRAVEN', 'DUPLIN', 'GREENE', 'JONES', 'LENOIR', 'NEW HANOVER', 'ONSLOW', 'PAMLICO', 'PENDER', 'WAYNE']


# function to add region as a column

def label(row):
    if row['County_Name'] in WESTERN:
        return 'WESTERN'
    if row['County_Name'] in NORTHWEST:
        return 'NORTHWEST'
    if row['County_Name'] in SOUTHWEST:
        return 'SOUTHWEST'
    if row['County_Name'] in TRIAD:
        return 'TRIAD'
    if row['County_Name'] in NORTHCENTRAL:
        return 'NORTHCENTRAL'
    if row['County_Name'] in SANDHILLS:
        return 'SANDHILLS'
    if row['County_Name'] in NORTHEAST:
        return 'NORTHEAST'
    if row['County_Name'] in SOUTHEAST:
        return 'SOUTHEAST'


tblCodesAll['Region'] = tblCodesAll.apply(lambda row: label(row), axis=1)
tblCodesAll.head(100)


# In[1]:


# import datetime as dt
#dt.datetime.as_datetime(string)


# In[41]:


# Bin Weather for clear and unclear

tblCodesAll.WEATHER.unique()

# combine similar values

tblCodesAll['WEATHER'] = tblCodesAll['WEATHER'].replace(['Fog, Smog, Smoke', 'Cloudy', 'Severe Crosswinds', 'Rain',
       'Snow', 'Sleet, Hail (Freezing Rain or Drizzle)','Blowing Sand, Soil, Dirt', 'Sleet or Hail',
       'Freezing Rain or Drizzle'], 'Not Clear')

tblCodesAll['WEATHER'] = tblCodesAll['WEATHER'].replace(['Other','NA'], 'Other or NA')
                                     
tblCodesAll.WEATHER.unique()



# In[43]:


# function to time between notification and arrival

def timestart(row):
    if row['HOUR'].find(':') >0:
        pos = row['HOUR'].find(':') 
        if row['HOUR'].find('am') >0:
            return row['HOUR'][0:pos]
        if row['HOUR'].find('pm') >0:
            time = int(row['HOUR'][0:pos])
            return time+12
    else:
        return 'UNKNOWN'



#error at unknowns
tblCodesAll['Hour_New'] = tblCodesAll.apply(lambda row: timestart(row), axis=1)
tblCodesAll.head(100)


# In[4]:


# add Cathy's info

import pandas as pd
import numpy as np
import os
import csv

os.chdir(r"C:\Users\karud\Documents\MSA_Fall2\Visualization")

df1 = pd.read_csv("new_Oct20_CT.csv")

# add lists for each region to be referenced later when we add a new column

WESTERN = ['BUNCOMBE', 'CHEROKEE', 'CLAY', 'GRAHAM', 'HAYWOOD', 'HENDERSON', 'JACKSON', 'MACON', 'MADISON', 'POLK', 'RUTHERFORD', 'SWAIN', 'TRANSYLVANIA']


NORTHWEST= ['ALLEGHANY', 'ASHE', 'ALEXANDER', 'AVERY', 'BURKE', 'CALDWELL', 'CATAWBA', 'MCDOWELL', 'MITCHELL', 'WATAUGA', 'WILKES', 'YANCEY']

SOUTHWEST =['ANSON', 'CABARRUS', 'CLEVELAND', 'GASTON', 'IREDELL', 'LINCOLN', 'MECKLENBURG', 'ROWAN', 'STANLY', 'UNION']

TRIAD =['ALAMANCE', 'CASWELL', 'DAVIDSON', 'DAVIE', 'FORSYTH', 'GUILFORD', 'RANDOLPH', 'ROCKINGHAM', 'STOKES', 'SURRY', 'YADKIN']

NORTHCENTRAL=['CHATHAM', 'DURHAM', 'EDGECOMBE', 'FRANKLIN', 'GRANVILLE', 'HARNETT', 'JOHNSTON', 'LEE', 'NASH', 'ORANGE', 'PERSON', 'VANCE', 'WAKE', 'WARREN', 'WILSON']

SANDHILLS= ['BLADEN', 'COLUMBUS', 'CUMBERLAND', 'HOKE', 'MONTGOMERY', 'MOORE', 'RICHMOND', 'ROBESON', 'SAMPSON', 'SCOTLAND']

NORTHEAST = ['BEAUFORT', 'BERTIE', 'CAMDEN', 'CHOWAN', 'CURRITUCK', 'DARE', 'GATES', 'HALIFAX', 'HERTFORD', 'HYDE', 'MARTIN', 'NORTHAMPTON', 'PASQUOTANK', 'PERQUIMANS', 'PITT', 'TYRRELL', 'WASHINGTON']

SOUTHEAST=['BRUNSWICK', 'CARTERET', 'CRAVEN', 'DUPLIN', 'GREENE', 'JONES', 'LENOIR', 'NEW HANOVER', 'ONSLOW', 'PAMLICO', 'PENDER', 'WAYNE']


# function to add region as a column

def label(row):
    if row['County_Name'] in WESTERN:
        return 'WESTERN'
    if row['County_Name'] in NORTHWEST:
        return 'NORTHWEST'
    if row['County_Name'] in SOUTHWEST:
        return 'SOUTHWEST'
    if row['County_Name'] in TRIAD:
        return 'TRIAD'
    if row['County_Name'] in NORTHCENTRAL:
        return 'NORTHCENTRAL'
    if row['County_Name'] in SANDHILLS:
        return 'SANDHILLS'
    if row['County_Name'] in NORTHEAST:
        return 'NORTHEAST'
    if row['County_Name'] in SOUTHEAST:
        return 'SOUTHEAST'


df1['Region'] = tblCodesAll.apply(lambda row: label(row), axis=1)
df1.head(100)

df1.to_csv((r"C:\Users\karud\Documents\MSA_Fall2\Visualization\AccidentsEdit_Oct20.csv"))


# In[98]:


# summary table by year

import pandas as pd
import numpy as np
import os
import csv

os.chdir(r"C:\Users\karud\Documents\MSA_Fall2\Visualization")

s = pd.read_csv(r"pop_by_county_CT.csv")
#delete County_Name 
s = s.drop(["County_Name", "STATE"],axis=1)
#rename County_Name.1 and Year
s = s.rename(columns={"County_Name.1":"County_Name", "Year" : "YEAR"})

# add lists for each region to be referenced later when we add a new column

WESTERN = ['BUNCOMBE', 'CHEROKEE', 'CLAY', 'GRAHAM', 'HAYWOOD', 'HENDERSON', 'JACKSON', 'MACON', 'MADISON', 'POLK', 'RUTHERFORD', 'SWAIN', 'TRANSYLVANIA']


NORTHWEST= ['ALLEGHANY', 'ASHE', 'ALEXANDER', 'AVERY', 'BURKE', 'CALDWELL', 'CATAWBA', 'MCDOWELL', 'MITCHELL', 'WATAUGA', 'WILKES', 'YANCEY']

SOUTHWEST =['ANSON', 'CABARRUS', 'CLEVELAND', 'GASTON', 'IREDELL', 'LINCOLN', 'MECKLENBURG', 'ROWAN', 'STANLY', 'UNION']

TRIAD =['ALAMANCE', 'CASWELL', 'DAVIDSON', 'DAVIE', 'FORSYTH', 'GUILFORD', 'RANDOLPH', 'ROCKINGHAM', 'STOKES', 'SURRY', 'YADKIN']

NORTHCENTRAL=['CHATHAM', 'DURHAM', 'EDGECOMBE', 'FRANKLIN', 'GRANVILLE', 'HARNETT', 'JOHNSTON', 'LEE', 'NASH', 'ORANGE', 'PERSON', 'VANCE', 'WAKE', 'WARREN', 'WILSON']

SANDHILLS= ['BLADEN', 'COLUMBUS', 'CUMBERLAND', 'HOKE', 'MONTGOMERY', 'MOORE', 'RICHMOND', 'ROBESON', 'SAMPSON', 'SCOTLAND']

NORTHEAST = ['BEAUFORT', 'BERTIE', 'CAMDEN', 'CHOWAN', 'CURRITUCK', 'DARE', 'GATES', 'HALIFAX', 'HERTFORD', 'HYDE', 'MARTIN', 'NORTHAMPTON', 'PASQUOTANK', 'PERQUIMANS', 'PITT', 'TYRRELL', 'WASHINGTON']

SOUTHEAST=['BRUNSWICK', 'CARTERET', 'CRAVEN', 'DUPLIN', 'GREENE', 'JONES', 'LENOIR', 'NEW HANOVER', 'ONSLOW', 'PAMLICO', 'PENDER', 'WAYNE']


# function to add region as a column

def label(row):
    if row['County_Name'] in WESTERN:
        return 'WESTERN'
    if row['County_Name'] in NORTHWEST:
        return 'NORTHWEST'
    if row['County_Name'] in SOUTHWEST:
        return 'SOUTHWEST'
    if row['County_Name'] in TRIAD:
        return 'TRIAD'
    if row['County_Name'] in NORTHCENTRAL:
        return 'NORTHCENTRAL'
    if row['County_Name'] in SANDHILLS:
        return 'SANDHILLS'
    if row['County_Name'] in NORTHEAST:
        return 'NORTHEAST'
    if row['County_Name'] in SOUTHEAST:
        return 'SOUTHEAST'


s['Region'] = s.apply(lambda row: label(row), axis=1)
s = s.sort_values(by=['Region', 'County_Name', 'YEAR'])
s.head(100)


# In[68]:


# updated table with only cols we want to add
df = pd.read_csv(r"AccidentsEdit_Oct20.csv")
df = df[['YEAR', 'County_Name', 'Region', 'FATALS']]
#try this a different way... get everything as a narrow column.
numDeaths1 = df.groupby(['Region', 'County_Name', 'YEAR'], as_index=False).agg({"FATALS":"sum"})
n =pd.DataFrame(numDeaths1)
n
# this work


# In[174]:


c = pd.merge(n, s, on=['Region', 'County_Name', 'YEAR'], how='right')
# replace NaN with 0 (those are obs with no deaths!)
c['FATALS'].fillna(0, inplace=True)
c['Standardized_Fatalities'] = (c['FATALS'] / (c['Population']/100000))


#get long version 
c = c.rename(columns={"FATALS": "Number_of_Fatalities"})
clong = pd.melt(c, id_vars=['Region', 'County_Name', 'YEAR', 'Population','Population_Ranking_By_Year'], var_name = 'Type',value_name='Fatalities')
#clong.to_csv(r"longCounties.csv")
clong


# In[176]:


#let's look at standardized population by region...

Reg = c.groupby(['Region','YEAR'], as_index=False).agg({"FATALS":"sum", "Population":"sum"})
Reg['Standardized_Fatalities'] = (Reg['FATALS'] / (Reg['Population']/100000))
Reg = Reg.rename(columns={"FATALS": "Number_of_Fatalities"})

#def popgrowth(row):
    #if row['Region'] == row['Region'].shift():
    #return Reg['Population'] - Reg['Population'].shift(1)

#Reg['Population_Growth'] = Reg.apply(lambda row: popgrowth(row), axis=1)    
#Reg

#Reg.to_csv(r"AccidentPopByRegion.csv")

#make table long so we can use a filter

long = pd.melt(Reg, id_vars=['Region', 'YEAR', 'Population'], var_name = 'Type',value_name='Fatalities')
#long.to_csv(r"long.csv")


# In[150]:


#combine tables

df = pd.read_csv(r"AccidentsEdit_Oct20.csv")
dfall = pd.merge(df, Reg, on=['Region', 'YEAR'], how='left')
dfall = dfall.rename(columns={"Population_x":"Population_County", "Population_y" : "Population_Region", "FATALS_y":"FATALS_Region", "Standardized_Fatalities": "St._Fatalties_Region"})
dfallc = pd.merge(dfall, c, on=['Region', 'County_Name', 'YEAR'], how='left')
dfallc= dfallc.rename(columns = {"FATALS":"FATALS_County","Standardized_Fatalities": "St._Fatalities_County", "Population_Ranking_By_Year_y": "Population_Ranking_By_Year"})
dfallc = dfallc.drop(["Population_Ranking_By_Year_x", "Population", "STATE"],axis=1)
dfallc
dfallc.to_csv(r"final1.csv")


# In[161]:


#add a variable for time of day to use as X-axis...
# FYI - Not the best 

def timestart(row):
    if row['NOT_HOUR'].find(':') >0:
        pos = row['NOT_HOUR'].find(':') 
        if row['NOT_HOUR'].find('12') > 0:
                return 12
        else:
            if row['NOT_HOUR'].find('am') >0:
                return row['NOT_HOUR'][0:pos]
            if row['NOT_HOUR'].find('pm') >0:
                time = int(row['NOT_HOUR'][0:pos])
                return time+12
            else:
                return 'UNKNOWN'



#error at unknowns
dfallc['Hour_Notified'] = dfallc.apply(lambda row: timestart(row), axis=1)
dfallc=dfallc.drop(["Hour_New"], axis=1)
dfallc.to_csv(r"final3.csv")
dfallc


# In[184]:


# make a super long table with EVERYTHING.
# must be careful to use proper aggregation

longboi = pd.merge(long,clong, on=['Region', 'YEAR', 'Type'], how='right')
longboi=longboi.rename(columns={"Population_x" : "Population_Region", "Fatalities_x" :"Fatalities_Region", 
                               "Population_y":"Population_County", "Fatalities_y":"Fatalities_County"})
longboi.to_csv(r"plswork.csv")


# In[ ]:





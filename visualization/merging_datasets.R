# Install packages and library
install.packages("plyr")
library("plyr")
library(data.table)
# Set the working directory
# Accident
# Run all of the files related to accident before resetting the wd to vehicle
setwd("/Users/CathyTran/Documents/Fall II/Visualization/VIsualization Project/accident")

# Read CSV into R
# Accident Tables
accident2000 <- read.csv(file="accident2000.csv", header=TRUE, sep=",")
accident2001 <- read.csv(file="accident2001.csv", header=TRUE, sep=",")
accident2002 <- read.csv(file="accident2002.csv", header=TRUE, sep=",")
accident2003 <- read.csv(file="accident2003.csv", header=TRUE, sep=",")
accident2004 <- read.csv(file="accident2004.csv", header=TRUE, sep=",")
accident2005 <- read.csv(file="accident2005.csv", header=TRUE, sep=",")
accident2006 <- read.csv(file="accident2006.csv", header=TRUE, sep=",")
accident2007 <- read.csv(file="accident2007.csv", header=TRUE, sep=",")
accident2008 <- read.csv(file="accident2008.csv", header=TRUE, sep=",")
accident2009 <- read.csv(file="accident2009.csv", header=TRUE, sep=",")
accident2010 <- read.csv(file="accident2010.csv", header=TRUE, sep=",")
accident2011 <- read.csv(file="accident2011.csv", header=TRUE, sep=",")
accident2012 <- read.csv(file="accident2012.csv", header=TRUE, sep=",")
accident2013 <- read.csv(file="accident2013.csv", header=TRUE, sep=",")
accident2014 <- read.csv(file="accident2014.csv", header=TRUE, sep=",")
accident2015 <- read.csv(file="accident2015.csv", header=TRUE, sep=",")
accident2016 <- read.csv(file="accident2016.csv", header=TRUE, sep=",")
accident2017 <- read.csv(file="accident2017.csv", header=TRUE, sep=",")

# don't convert character vectors to factors
options(stringsAsFactors = FALSE)

# These are the cols in accident 2000 but not in accident 2001
# "LATITUDE" "LONGITUD"
setdiff(names(accident2000),names(accident2001))

# These are the cols in accident 2001 but not in accident 2000
# "latitude" "longitud"
setdiff(names(accident2015),names(accident2014))

# New columns will be added at the end with values of NA
accident <- rbind.fill(accident2000,accident2001)

# Should contain accidents from 2000 to 2002 - var (YEAR)
# 53 vars
accident <- rbind.fill(accident,accident2002)

# Should contain accidents from 2000 to 2003 - var (YEAR)
accident <- rbind.fill(accident,accident2003)

# Should contain accidents from 2000 to 2004 - var (YEAR)
accident <- rbind.fill(accident,accident2004)

# Should contain accidents from 2000 to 2005 - var (YEAR)
accident <- rbind.fill(accident,accident2005)

# Should contain accidents from 2000 to 2006 - var (YEAR)
accident <- rbind.fill(accident,accident2006)

# Should contain accidents from 2000 to 2007 - var (YEAR)
accident <- rbind.fill(accident,accident2007)

# Should contain accidents from 2000 to 2008 - var (YEAR)
accident <- rbind.fill(accident,accident2008)

# Should contain accidents from 2000 to 2009 - var (YEAR)
accident <- rbind.fill(accident,accident2009)

# Should contain accidents from 2000 to 2010 - var (YEAR)
accident <- rbind.fill(accident,accident2010)

# Should contain accidents from 2000 to 2011 - var (YEAR)
accident <- rbind.fill(accident,accident2011)

# Should contain accidents from 2000 to 2012 - var (YEAR)
accident <- rbind.fill(accident,accident2012)

# Should contain accidents from 2000 to 2013 - var (YEAR)
accident <- rbind.fill(accident,accident2013)

# Should contain accidents from 2000 to 2014 - var (YEAR)
accident <- rbind.fill(accident,accident2014)

# Should contain accidents from 2000 to 2015 - var (YEAR)
accident <- rbind.fill(accident,accident2015)

# Should contain accidents from 2000 to 2016 - var (YEAR)
accident <- rbind.fill(accident,accident2016)

# Should contain accidents from 2000 to 2017 - var (YEAR)
accident <- rbind.fill(accident,accident2017)

# accident has 624,129 obs 67 cols (YEAR AND ST_CASE)
# ST_CASE is unique for each year

accident <- as.data.table(accident)


# Set the working directory
# Vehicle
setwd("/Users/CathyTran/Documents/Fall II/Visualization/VIsualization Project/vehicle")

# Import Vehicle Tables (Year by Year from 2000 to 2017)
vehicle2000 <- read.csv(file="vehicle2000.csv", header=TRUE, sep=",")
vehicle2001 <- read.csv(file="vehicle2001.csv", header=TRUE, sep=",")
vehicle2002 <- read.csv(file="vehicle2002.csv", header=TRUE, sep=",")
vehicle2003 <- read.csv(file="vehicle2003.csv", header=TRUE, sep=",")
vehicle2004 <- read.csv(file="vehicle2004.csv", header=TRUE, sep=",")
vehicle2005 <- read.csv(file="vehicle2005.csv", header=TRUE, sep=",")
vehicle2006 <- read.csv(file="vehicle2006.csv", header=TRUE, sep=",")
vehicle2007 <- read.csv(file="vehicle2007.csv", header=TRUE, sep=",")
vehicle2008 <- read.csv(file="vehicle2008.csv", header=TRUE, sep=",")
vehicle2009 <- read.csv(file="vehicle2009.csv", header=TRUE, sep=",")
vehicle2010 <- read.csv(file="vehicle2010.csv", header=TRUE, sep=",")
vehicle2011 <- read.csv(file="vehicle2011.csv", header=TRUE, sep=",")
vehicle2012 <- read.csv(file="vehicle2012.csv", header=TRUE, sep=",")
vehicle2013 <- read.csv(file="vehicle2013.csv", header=TRUE, sep=",")
vehicle2014 <- read.csv(file="vehicle2014.csv", header=TRUE, sep=",")
vehicle2015 <- read.csv(file="vehicle2015.csv", header=TRUE, sep=",")
vehicle2016 <- read.csv(file="vehicle2016.csv", header=TRUE, sep=",")
vehicle2017 <- read.csv(file="vehicle2017.csv", header=TRUE, sep=",")

# Create y, y1 vars that repeat the year value for the total obs that data set
# For example, for vehicle2000, repeat the value 2000 for 57594 times
# The purpose is to create a Year variable for all of the vehicle data sets

y<-rep(2000, nrow(vehicle2000))
y1<-rep(2001, nrow(vehicle2001))
y2<-rep(2002, nrow(vehicle2002))
y3<-rep(2003, nrow(vehicle2003))
y4<-rep(2004, nrow(vehicle2004))
y5<-rep(2005, nrow(vehicle2005))
y6<-rep(2006, nrow(vehicle2006))
y7<-rep(2007, nrow(vehicle2007))
y8<-rep(2008, nrow(vehicle2008))
y9<-rep(2009, nrow(vehicle2009))
y10<-rep(2010, nrow(vehicle2010))
y11<-rep(2011, nrow(vehicle2011))
y12<-rep(2012, nrow(vehicle2012))
y13<-rep(2013, nrow(vehicle2013))
y14<-rep(2014, nrow(vehicle2014))
y15<-rep(2015, nrow(vehicle2015))
y16<-rep(2016, nrow(vehicle2016))
y17<-rep(2017, nrow(vehicle2017))

# Creating the year variable for each year in the vehicle tables
vehicle2000$YEAR <- y
vehicle2001$YEAR <- y1
vehicle2002$YEAR <- y2
vehicle2003$YEAR <- y3
vehicle2004$YEAR <- y4
vehicle2005$YEAR <- y5
vehicle2006$YEAR <- y6
vehicle2007$YEAR <- y7
vehicle2008$YEAR <- y8
vehicle2009$YEAR <- y9
vehicle2010$YEAR <- y10
vehicle2011$YEAR <- y11
vehicle2012$YEAR <- y12
vehicle2013$YEAR <- y13
vehicle2014$YEAR <- y14
vehicle2015$YEAR <- y15
vehicle2016$YEAR <- y16
vehicle2017$YEAR <- y17

# Combine the vehicle tables year by year
vehicle <- rbind.fill(vehicle2000,vehicle2001)
vehicle <- rbind.fill(vehicle,vehicle2002)
vehicle <- rbind.fill(vehicle,vehicle2003)
vehicle <- rbind.fill(vehicle,vehicle2004)
vehicle <- rbind.fill(vehicle,vehicle2005)
vehicle <- rbind.fill(vehicle,vehicle2006)
vehicle <- rbind.fill(vehicle,vehicle2007)
vehicle <- rbind.fill(vehicle,vehicle2008)
vehicle <- rbind.fill(vehicle,vehicle2009)
vehicle <- rbind.fill(vehicle,vehicle2010)
vehicle <- rbind.fill(vehicle,vehicle2011)
vehicle <- rbind.fill(vehicle,vehicle2012)
vehicle <- rbind.fill(vehicle,vehicle2013)
vehicle <- rbind.fill(vehicle,vehicle2014)
vehicle <- rbind.fill(vehicle,vehicle2015)
vehicle <- rbind.fill(vehicle,vehicle2016)
vehicle <- rbind.fill(vehicle,vehicle2017)

vehicle <- as.data.table(vehicle)

# Rename year to YEAR
colnames(vehicle)[colnames(vehicle)=="Year"] <- "YEAR"

# Set the working directory
# Person
setwd("/Users/CathyTran/Documents/Fall II/Visualization/VIsualization Project/person")

# Importing each Person table from 2000 to 2017
person2000 <- read.csv(file="person2000.csv", header=TRUE, sep=",")
person2001 <- read.csv(file="person2001.csv", header=TRUE, sep=",")
person2002 <- read.csv(file="person2002.csv", header=TRUE, sep=",")
person2003 <- read.csv(file="person2003.csv", header=TRUE, sep=",")
person2004 <- read.csv(file="person2004.csv", header=TRUE, sep=",")
person2005 <- read.csv(file="person2005.csv", header=TRUE, sep=",")
person2006 <- read.csv(file="person2006.csv", header=TRUE, sep=",")
person2007 <- read.csv(file="person2007.csv", header=TRUE, sep=",")
person2008 <- read.csv(file="person2008.csv", header=TRUE, sep=",")
person2009 <- read.csv(file="person2009.csv", header=TRUE, sep=",")
person2010 <- read.csv(file="person2010.csv", header=TRUE, sep=",")
person2011 <- read.csv(file="person2011.csv", header=TRUE, sep=",")
person2012 <- read.csv(file="person2012.csv", header=TRUE, sep=",")
person2013 <- read.csv(file="person2013.csv", header=TRUE, sep=",")
person2014 <- read.csv(file="person2014.csv", header=TRUE, sep=",")
person2015 <- read.csv(file="person2015.csv", header=TRUE, sep=",")
person2016 <- read.csv(file="person2016.csv", header=TRUE, sep=",")
person2017 <- read.csv(file="person2017.csv", header=TRUE, sep=",")


# Creating the year variable for each year in the person tables
x<-rep(2000, nrow(person2000))
x1<-rep(2001, nrow(person2001))
x2<-rep(2002, nrow(person2002))
x3<-rep(2003, nrow(person2003))
x4<-rep(2004, nrow(person2004))
x5<-rep(2005, nrow(person2005))
x6<-rep(2006, nrow(person2006))
x7<-rep(2007, nrow(person2007))
x8<-rep(2008, nrow(person2008))
x9<-rep(2009, nrow(person2009))
x10<-rep(2010, nrow(person2010))
x11<-rep(2011, nrow(person2011))
x12<-rep(2012, nrow(person2012))
x13<-rep(2013, nrow(person2013))
x14<-rep(2014, nrow(person2014))
x15<-rep(2015, nrow(person2015))
x16<-rep(2016, nrow(person2016))
x17<-rep(2017, nrow(person2017))

person2000$YEAR <- x
person2001$YEAR <- x1
person2002$YEAR <- x2
person2003$YEAR <- x3
person2004$YEAR <- x4
person2005$YEAR <- x5
person2006$YEAR <- x6
person2007$YEAR <- x7
person2008$YEAR <- x8
person2009$YEAR <- x9
person2010$YEAR <- x10
person2011$YEAR <- x11
person2012$YEAR <- x12
person2013$YEAR <- x13
person2014$YEAR <- x14
person2015$YEAR <- x15
person2016$YEAR <- x16
person2017$YEAR <- x17


person <- rbind.fill(person2000,person2001)
person <- rbind.fill(person,person2002)
person <- rbind.fill(person,person2003)
person <- rbind.fill(person,person2004)
person <- rbind.fill(person,person2005)
person <- rbind.fill(person,person2006)
person <- rbind.fill(person,person2007)
person <- rbind.fill(person,person2008)
person <- rbind.fill(person,person2009)
person <- rbind.fill(person,person2010)
person <- rbind.fill(person,person2011)
person <- rbind.fill(person,person2012)
person <- rbind.fill(person,person2013)
person <- rbind.fill(person,person2014)
person <- rbind.fill(person,person2015)
person <- rbind.fill(person,person2016)
person <- rbind.fill(person,person2017)

# Convert to a data table
person <- as.data.table(person)

# Rename year to YEAR
colnames(person)[colnames(person)=="Year"] <- "YEAR"

# Originally, vehicle and person don't have the var YEAR
# Now,
# vehicle has 941,415 obs 157 cols (YEAR AND ST_CASE)
# accident has 64129,163 obs 67 cols (YEAR AND ST_CASE)
# person has 1,586,993 obs 101 cols (YEAR AND ST_CASE)

# needs to make st_case unique for it's only unique for each year

# Left Join vehicle and accident
# df<-merge(x=vehicle,y=accident,by="ST_CASE, YEAR")

# Export the accident df to csv 
write.csv(accident,'/Users/CathyTran/Documents/Fall II/Visualization/VIsualization Project\\Accident.csv', row.names = TRUE)

# Export the person df to csv 
write.csv(person,'/Users/CathyTran/Documents/Fall II/Visualization/VIsualization Project\\Person.csv', row.names = TRUE)

# Export the vehicle df to csv 
write.csv(vehicle,'/Users/CathyTran/Documents/Fall II/Visualization/VIsualization Project\\Vehicle.csv', row.names = TRUE)



# Install packages and library
install.packages("plyr")
library("plyr")

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

# Export the Accident df to csv 
write.csv(accident,'/Users/CathyTran/Documents/Fall II/Visualization/VIsualization Project\\Accident.csv', row.names = TRUE)

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

# Create y, y1 vars that repeat year value for the total obs that data set
# For example, for vehicle2000, repeat the value 2000 for 57594 times
# The purpose is to create a Year variable for all of the vehicle data sets
y<-lapply(rep(2000, nrow(vehicle2000)), as.numeric)
y<-lapply(rep(2000, nrow(vehicle2000)), as.numeric)
nrow(vehicle2000)


y1<-rep(2001, count(vehicle2001))
y2<-rep(2002, count(vehicle2002))
y3<-rep(2003, count(vehicle2003))
y4<-rep(2004, count(vehicle2004))
y5<-rep(2005, count(vehicle2005))
y6<-rep(2006, count(vehicle2006))
y7<-rep(2007, count(vehicle2007))
y8<-rep(2008, count(vehicle2008))
y9<-rep(2009, count(vehicle2009))
y10<-rep(2010, count(vehicle2010))
y11<-rep(2011, count(vehicle2011))
y12<-rep(2012, count(vehicle2012))
y13<-rep(2013, count(vehicle2013))
y14<-rep(2014, count(vehicle2014))
y15<-rep(2015, count(vehicle2015))
y16<-rep(2016, count(vehicle2016))
y17<-rep(2017, count(vehicle2017))

# Creating the year variable for each year in the vehicle tables
vehicle2000$Year <- y
vehicle2001$Year <- y1
vehicle2002$Year <- y2
vehicle2003$Year <- y3
vehicle2004$Year <- y4
vehicle2005$Year <- y5
vehicle2006$Year <- y6
vehicle2007$Year <- y7
vehicle2008$Year <- y8
vehicle2009$Year <- y9
vehicle2010$Year <- y10
vehicle2011$Year <- y11
vehicle2012$Year <- y12
vehicle2013$Year <- y13
vehicle2014$Year <- y14
vehicle2015$Year <- y15
vehicle2016$Year <- y16
vehicle2017$Year <- y17

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
x<-rep(2000, count(person2000))
x1<-rep(2001, count(person2001))
x2<-rep(2002, count(person2002))
x3<-rep(2003, count(person2003))
x4<-rep(2004, count(person2004))
x5<-rep(2005, count(person2005))
x6<-rep(2006, count(person2006))
x7<-rep(2007, count(person2007))
x8<-rep(2008, count(person2008))
x9<-rep(2009, count(person2009))
x10<-rep(2010, count(person2010))
x11<-rep(2011, count(person2011))
x12<-rep(2012, count(person2012))
x13<-rep(2013, count(person2013))
x14<-rep(2014, count(person2014))
x15<-rep(2015, count(person2015))
x16<-rep(2016, count(person2016))
x17<-rep(2017, count(person2017))

person2000$Year <- x
person2001$Year <- x1
person2002$Year <- x2
person2003$Year <- x3
person2004$Year <- x4
person2005$Year <- x5
person2006$Year <- x6
person2007$Year <- x7
person2008$Year <- x8
person2009$Year <- x9
person2010$Year <- x10
person2011$Year <- x11
person2012$Year <- x12
person2013$Year <- x13
person2014$Year <- x14
person2015$Year <- x15
person2016$Year <- x16
person2017$Year <- x17


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

# Originally, vehicle and person don't have the var YEAR
# Now,
# vehicle has 941,415 obs 157 cols (YEAR AND ST_CASE)
# accident has 726,163 obs 67 cols (YEAR AND ST_CASE)
# person has 1586,993 obs 101 cols (YEAR AND ST_CASE)

# needs to make st_case unique for it's only unique for each year

# Merge Accident and 
df<-merge(x=vehicle,y=accident,by="ST_CASE")

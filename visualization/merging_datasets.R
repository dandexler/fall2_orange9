# Install packages and library
install.packages("plyr")
library("plyr")

# Set the working directory
# Accident
setwd("/Users/CathyTran/Documents/Fall II/Visualization/VIsualization Project/accident")
# Vehicle
setwd("/Users/CathyTran/Documents/Fall II/Visualization/VIsualization Project/vehicle")
# Person
setwd("/Users/CathyTran/Documents/Fall II/Visualization/VIsualization Project/person")

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

list=ls()

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

# Vehicle
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


# Person
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

install.packages('tidyverse')
library("tidyverse")

install.packages('Jmisc')
library(Jmisc)

year=seq(2000,2017)
list=c(person2000,person2001,person2002,person2003,person2004,person2005,person2006,person2007,person2008,person2009,person2010,person2011,person2012,person2013,person2014,person2015,person2016,person2017)

x<-rep(2013, 74331)

person2013$Year<-x

for (i in length(list)){
  rep(year[i]), value=year[i])
  }

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

# vehicle has 941,415 obs 156 cols (ST_CASE)
# accident has 726,163 obs 67 cols (YEAR AND ST_CASE)
# person has 1586,993 obs 100 cols (ST_CASE)

# needs to make st_case unique for it's only unique for each year
summary(vehicle)
vehicle$ST_CASE
combine <- cbind(ST_CASE,YEAR)

colnames(person)
colnames(accident)
colnames(vehicle)

new <- paste(ST_CASE,YEAR)

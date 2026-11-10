#' ---
#' title: GHE data: data management of the new GHE 2023 estimates
#' author: Mathieu Bastard
#' date: 24/07/2026


# This script was developed to perform data cleaning on GHE 2023 csv files
# Data received from WHO DDA/DSA on 9th July 2026
#
# input: csv file estimated number of deaths by country, year and age groups
#        
# Process: Data cleaning and reshaping
# 
# Output: 1 Rda files ghe.rda




#'
#' Process GHE 2023 file
#'

rm(list=ls())

# Load libraries and data
library(data.table)
library(imputeTS)
library(haven) # read_dta
library(readxl)
library(stringr)
library(here)

#Function to import db from GTB databases
source(here("import/load_gtb.R"))



ghe <-  fread('data/mortality/GHE2023_allcause.csv')
ghe=as.data.table(ghe)
setkey(ghe,iso3)

# Keep only country data
cty <- load_gtb("cty",convert_dashes = TRUE)
ghe=merge(ghe,cty,by="iso3")

# Remove duplicate counts of  <1years : Age==0 removed as age==0 = age==0.1 (<1month) +age==0.11 (1-11 months)
ghe=ghe[age!=0,]

# Keep only all sex: sex==3
ghe=ghe[sex==3,]

# Calculate total per year
ghe2=aggregate(cbind(dths=ghe$dths,dths.lo=ghe$dths.low, dths.hi=ghe$dths.up), by=list(iso3=ghe$iso3,year=ghe$year), FUN=sum)
ghe2=as.data.table(ghe2)
setkey(ghe2,iso3)
dim(ghe2)

ghe2023=copy(ghe2)

attr(ghe2023, "timestamp") <- Sys.Date() #set date
save(ghe2023, file = 'inc_mort/analysis/ghe2023.rda')









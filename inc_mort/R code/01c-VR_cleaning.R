#' ---
#' title: Cleaning of the VR data
#'        VR data Received from DDI on May 26th 2026
#' Author: Mathieu Bastard
#' Date: 01/06/2026
#' ---

#' This scripts pre-processes VR data from the global WHO mortality database, 
#' stored in multiple excel files using ICD8 ICD9 and ICD10 codes
#' 
#' 
#' Deps: libraries data.table, imputeTS, haven, readxl, stringr, here
#' 
#' 
#' Input: excel file in ~/data/mortality (VR data from WHO data division)
#' 
#'        VR quality codes published by WHO in WHS: ~/input/VR/vrqual.csv
#'        (four quality codes prepared by the data division and published in WHS: 
#'        high, medium, low, very low (or no VR) - only data with the top
#'        two quality codes are retained (flag: keep.vr==TRUE), 
#'        the other variables in the vrqual 
#'        dataset are not used in these scripts)
#'        
#'        IHME estimates (2021 GBD) used in selected countries: 
#'           ihmetb.rda (TB estimates), 
#'           ihmeall.rda (total deaths)
#'        
#'        GHE envelope estimates: input/GHE/dths_total.csv, obtained from 
#'        the data division (Cao Bochen or Doris Mafat)
#'        Last update up to 2023
#' 
#' Output: vr.rda and related timestamped csv file
#' 
#' 


### 2026 Edits

# VR datasets now in 4 seperate files
#
# VR quality assessment updated in 2025, from DDI, in mortality method document
# Brazil, use their VR data total deaths as enveloppe for 2022 2023 in absence of new GHE estimates


rm(list=ls())

# Load libraries and data
library(data.table)
library(imputeTS)
library(haven) # read_dta
library(readxl)
library(stringr)
library(here)
library(dplyr)

#Function to import db from GTB databases
source(here("import/load_gtb.R"))


# load data
load(here('inc_mort/estimates2025/vr.rda'))   
ovr=copy(vr)
rm(vr)

tb <- load_gtb("tb",convert_dashes = TRUE)
cty <- load_gtb("cty",convert_dashes = TRUE)
pop <- load_gtb("pop",convert_dashes = TRUE)

#VR from GTB database
ovrgtb <- load(here('inc_mort/estimates2025/vrgtb.rda'))   
ovrgtb <- copy(vrgtb)
rm(vrgtb)
vrgtb <- load_gtb("tbdeaths",convert_dashes = TRUE)


#load UNDAIS data
#load(here('inc_mort/analysis/unaids.rda'))

source(here('inc_mort/R code/fun.R'))


#load GHE2023
load(here('inc_mort/analysis/ghe2023.rda'))


#Import datasets

icd8 <- 'data/mortality/icd8_condensed.xlsx'
icd9 <- 'data/mortality/icd9_condensed.xlsx'
icd101 <- 'data/mortality/icd10_condensed.xlsx'
icd103 <- 'data/mortality/icd10_detailed.xlsx'

# m set to 100 000 for rates calculation
m <- 1e5

# Current year of estimates
yr <- 2025


# convert VR data

#
# function for reformatting
refrm <- function(indat) {
  # rename & aggregate
  indat <- indat[, .(
    Country,
    name,
    Year,
    icd,
    Cause,
    cause1,
    Sex,
    `0-4` = rowSums(cbind(
      Deaths2, Deaths3, Deaths4, Deaths5, Deaths6
    ), na.rm = T),
    `5-14` = rowSums(cbind(Deaths7, Deaths8), na.rm = T),
    `15-24` = rowSums(cbind(Deaths9, Deaths10), na.rm = T),
    `25-34` = rowSums(cbind(Deaths11, Deaths12), na.rm = T),
    `35-44` = rowSums(cbind(Deaths13, Deaths14), na.rm = T),
    `45-54` = rowSums(cbind(Deaths15, Deaths16), na.rm = T),
    `55-64` = rowSums(cbind(Deaths17, Deaths18), na.rm = T),
    `65plus` = rowSums(
      cbind(
        Deaths19,
        Deaths20,
        Deaths21,
        Deaths22,
        Deaths23,
        Deaths24,
        Deaths25
      ),na.rm = T))
    ]

  # Sequelae
  seq <- c("B90", "B900", "B901", "B902", "B908", "B909", "B077")
  indat[Cause %in% seq, cause1 := "tbseq"]
  indat[, Cause := NULL]

  ## reshape
  MM <-
    melt(indat, id = c("Country", "name", "Year", "icd", "cause1", "Sex"))
  MM$sex <- c('M', 'F')[as.numeric(MM$Sex)]
  MM[is.na(sex), sex := 'U']
  MM$sex <- factor(MM$sex)
  MM$age <- factor(MM$variable, levels = agz3, ordered = TRUE)
  MM[, age_group := gsub('-', '_', age)]
  MM[, age := NULL]
  MM
}

# Some useful age range vectors:
agz <-
  c('04', '514', '1524', '2534', '3544', '4554', '5564', '65') #for extract
agz2 <-
  c('0_4',
    '5_14',
    '15_24',
    '25_34',
    '35_44',
    '45_54',
    '55_64',
    '65plus') #for labels
agz3 <- gsub('_', '-', agz2)
agz4 <- c(rev(rev(agz3)[-1]), "\u2265 65")
kds <- c('0_4', '5_14')
kdz <- c('04', '514')
AA <-
  data.table(
    a1 = agz,
    age_group = agz2,
    age = agz3,
    agegt = agz4
  ) #for conversion


typz <- c('text',
          'text',
          'text',
          'text',
          rep('text', 5),
          rep('numeric', 26)) #specify column types to avoid warning





## -------- ICD 10.1
M1 <-
  as.data.table(read_excel(
    icd101,
    col_names = TRUE,
    col_types = typz
  ))
M1 <- refrm(M1)


# ## -------- ICD 10.3 and 10.4
M2 <-
  as.data.table(read_excel(
    icd103,
    col_names = TRUE,
    col_types = typz
  ))
M2 <- refrm(M2)


## -------- ICD 9
M3 <-
  as.data.table(read_excel(
    icd9,
    col_types = typz
  ))

# remove double counted B02
M3[, dble := sums(str_count(Cause, 'B02[0-9]')), by = .(name, Year, Sex)]
M3[Cause == 'B02' & dble > 0, drop := TRUE]
(M3[, table(drop)])
M3 <- M3[is.na(drop)]
M3[, drop := NULL]
M3[, dble := NULL]
M3 <- refrm(M3)


## -------- ICD 8
M4 <-
  as.data.table(read_excel(
    icd8,
    col_types = typz
  ))
M4 <- refrm(M4)


## --- join ---
VR <- rbind(M1, M2, M3, M4)


## Differences in names:

setDT(pop)

(vrbad <- setdiff(VR[, unique(name)],
                  pop[, unique(country)]))


## renaming country name if bad match

vrbad

(newnm <- grep('Czech', pop[, unique(country)], value = TRUE))
VR[name == grep('Czech', vrbad, value = TRUE), name := newnm]
(newnm <- grep('Serbia', pop[, unique(country)], value = TRUE)[1])
#VR[name == grep('Serb', vrbad, value = TRUE), name := newnm]

(newnm <-grep('Macedonia', pop[, unique(country)], value = TRUE)[1])
VR[name == grep('Mace', vrbad, value = TRUE), name := newnm]
(newnm <- grep('Vincent', pop[, unique(country)], value = TRUE)[1])
VR[name == grep('Vincent', vrbad, value = TRUE), name := newnm]
(newnm <- grep('Libya', pop[, unique(country)], value = TRUE))
VR[name == grep('Libya', vrbad, value = TRUE), name := newnm]
(newnm <- grep('Palestinian', pop[, unique(country)], value = TRUE))
VR[name == grep('Palestinian', vrbad, value = TRUE), name := newnm]

## sub-countries
VR[name %in% c(
  "French Guiana",
  "Martinique",
  "Reunion",
  "Mayotte",
  "Guadeloupe",
  "Saint Pierre and Miquelon"
), name := "France"]
VR[name %in% c("Rodrigues"), name := "Mauritius"]

VR[name %in% c("Virgin Islands (USA)"), name := "United States of America"]

## check
(dropname <- setdiff(VR[, unique(name)], cty[, unique(country)]))

VR[name=='Turkey', name:='Türkiye']

(newnm <- grep('Hong', pop[, unique(country)], value = TRUE))
VR[name == grep('Hong', vrbad, value = TRUE), name := newnm]

(newnm <-grep('Macedonia', pop[, unique(country)], value = TRUE)[1])
VR[name == grep('Macedonia', vrbad, value = TRUE), name := newnm]

VR[name=='Netherlands', name:='Netherlands (Kingdom of the)']
VR[name=='United Kingdom', name:='United Kingdom of Great Britain and Northern Ireland']
VR[name=='Venezuela', name:='Venezuela (Bolivarian Republic of)']

(newnm <-grep('Bolivia', pop[, unique(country)], value = TRUE)[1])
VR[name == grep('Bolivia', vrbad, value = TRUE), name := newnm]

#Check
(dropname <- setdiff(VR[, unique(name)], cty[, unique(country)]))

VR <- VR[name %ni% dropname]

VR[, year := as.integer(Year)]
VR[, Year := NULL]


## Add iso3, and tidy up
VR <- merge(VR,
            cty[, .(iso3, country)],
            by.x = 'name',
            by.y = "country",
            all.x = TRUE)
(VR[is.na(iso3), unique(name)])

VR[, Sex := NULL]
VR[, age_group := variable]

VR$age_group <-
  factor(gsub("-", "_", VR$age_group),
         levels = agz2,
         ordered = TRUE)
VR[, variable := NULL]

setkey(VR, iso3)
rm(M1, M2, M3, M4)

# save(VR, file = 'data/VR.rda')



# aggregate and reshape long to wide
#
vr <- VR[, .(value = sums(value)),
         by = .(iso3, year, cause1)]

vr <- dcast(vr, ... ~ cause1)
setkey(vr, iso3)
setnames(vr, c('iso3','year','hiv','tb','ill_def','tb_seq','total'))





# # Country data additions to WHO database
#

# * Azerbaijan additions
#
# source: epi review, the Hague 2017
#

vr['AZE']
NewYEARS <- 2010:2015
addAZE <- as.data.frame(matrix(nrow=length(NewYEARS),ncol=ncol(vr)))
names(addAZE) <- names(vr)
addAZE$iso3 <- "AZE"
addAZE$year[1:nrow(addAZE)] <- NewYEARS

dim(vr)
vr2 <- rbind(vr, addAZE, use.names = TRUE)
setkey(vr2, iso3)
sel <- vr2$iso3 == 'AZE' & vr2$year %in% 2010:2015

# vr2$vr.coverage[sel] <- rep(1, 6)
vr2$total[sel] <- c(53580, 53726, 55017, 54383, 55648, 54697)
vr2$tb[sel] <- c(709, 577, 373, 378, 372, 485)
vr2$ill_def[sel] <- c(1343, 1771, 1836, 1892, 2440, 1864)
vr2['AZE']

# checking that each combination iso3-year appears only once in the database
tCheck <- as.data.frame(table(paste(vr2$iso3,vr2$year)))
nrow(tCheck[ tCheck[,2]>1 , ])==0



# check that the additions do not end-up duplicating country-year entries
#
sum(duplicated(vr[,.(iso3,year)]))==0



##
## VR from GTB database
##


# Up to 2024, from the old system, use ovrgtb

setDT(ovrgtb)

### 2024 RUS correction, add total deaths using the temporary: to be checked next year
ovrgtb[iso3=="RUS" & year==2024 & is.na(total.deaths.vr), total.deaths.vr:= 1862626]


#At least Total death and TB deaths available

ovrgtb=subset(ovrgtb, !is.na(total.deaths.vr)  & !is.na(tbdeaths.vr))


#Cleaning according to remarks (changing year of VR data)
ovrgtb[iso3=='BEL' & year==2019, year:=2017]
ovrgtb[iso3=='BEL' & year==2020, year:=2018]
ovrgtb[iso3=='BEL' & year==2021, year:=2019]
ovrgtb[iso3=='BEL' & year==2022, year:=2020]

ovrgtb[iso3=='FRA' & year==2021, year:=2017]


#Remove line with total death==0 (unreliable)
ovrgtb=subset(ovrgtb,total.deaths.vr>0)


#Renaming to match VR dataset
ovrgtb$total=ovrgtb$total.deaths.vr
ovrgtb$ill_def=ovrgtb$r00.r99.deaths.vr
ovrgtb$tb=ovrgtb$tbdeaths.vr


ovrgtb=subset(ovrgtb, select=c("iso3","year","total","ill_def","tb"))
ovrgtb$tb_seq=NA
ovrgtb$hiv=NA

vr5=subset(vr2,select=c("iso3","year"))
vr5$alreadyinvr=1

ovrgtb=merge(ovrgtb,vr5,by=c("iso3","year"),all.x = TRUE)
ovrgtb[is.na(alreadyinvr),alreadyinvr:=0]


#Observations to keep to import to main VR data

vrlst=c("GEO","KAZ","ROU","RUS","CZE","POL")

ovrgtb$tokeep.ovrgtb=ifelse(ovrgtb$alreadyinvr==1,0,1)

ovrgtb=subset(ovrgtb, tokeep.ovrgtb==1)
ovrgtb$alreadyinvr=NULL
ovrgtb$tokeep.ovrgtb=NULL

#Drop or set to NA unreliable data or inconsistent data
#Including countries with non reliable CRVS
#Manual review, to repeat every year

vrlst=c("ALB","CPV","CYM","DMA","GUM","ISR",
        "LBR","MDR","NIC","RWA","SAU","SLV",
        "STP","SVK","SVN","SYR","TKM","TZA","VCT")
ovrgtb=ovrgtb[iso3 %ni% vrlst, ]


# Check if ill_def==tb OR total==tb OR total==Ill_def
ovrgtb[ill_def==tb,.(iso3,year,ill_def,tb)]
ovrgtb[total==tb,.(iso3,year,total,tb)]
ovrgtb[ill_def==total,.(iso3,year,ill_def,total)]

#AZE 2023, set to NA
ovrgtb[ill_def==total,ill_def:=NA]

vr5 <- rbind(vr2, ovrgtb, use.names = TRUE)

setkey(vr5, iso3)

# Duplicates in iso/years
sel=duplicated(vr5[,c("iso3","year")])
vr5[sel==TRUE]

vr <- copy(vr5)
(dim(vr))
setkey(vr, iso3)




### Country reporting VR data from the new system WDIP in vrgtb
### Only for year 2025
#At least Total death and TB deaths available
setDT(vrgtb)
vrgtb=subset(vrgtb, year==2025 & !is.na(total.deaths.vr)  & !is.na(tbdeaths.vr))



#countries to drop: data checked ad-hoc
vr_todrop <- c("SLV","GRD","GUY","KEN","MUS","MNE","PSE","RWA","SVK","ALB")
vrgtb <- vrgtb[iso3 %ni% vr_todrop]

#Ill_def to NA as non plausible, then impute on last years
vr_illdeftoNA <- c("SYC")
vrgtb[iso3 %in% vr_illdeftoNA, r00.r99.deaths.vr := NA]

vrgtb <- vrgtb %>% 
  select(
    iso3,
    year,
    tb = tbdeaths.vr,
    ill_def = r00.r99.deaths.vr,
    total = total.deaths.vr
  ) %>% 
  mutate(
    hiv = NA,
    tb_seq = NA
  )
  


vr6 <- rbind(vr, vrgtb, use.names = TRUE)
setkey(vr6, iso3)

# Duplicates in iso/years
sel=duplicated(vr6[,c("iso3","year")])
vr6[sel==TRUE]

vr <- copy(vr6)
(dim(vr))
setkey(vr, iso3)



# Fix with data from review
# PHL, MNG, THA, BRA

tofix <- c("PHL","BRA","THA","MNG")
vr[iso3 %in% tofix, ]

# Brazil data sent on may 27th for 2024, 2025 data reported in database

vr <- vr %>% 
  add_row(
    iso3 = "BRA",
    year = 2024,
    hiv = NA,
    tb = 6944,
    ill_def = 69088,
    tb_seq = NA,
    total = 1532014
  )
  

# Thailand, correct 2024 using data sent by DTB and the Strategic and Planning Division

vr <- vr %>% 
  mutate(
    tb = if_else(iso3 == "THA" & year == 2024, 5907, tb),
    ill_def = if_else(iso3 == "THA" & year == 2024, 131421, ill_def),
    total = if_else(iso3 == "THA" & year == 2024, 556874, total)
  )


vr <- vr %>% 
  add_row(
    iso3 = "THA",
    year = 2020,
    hiv = NA,
    tb = 5967,
    ill_def = 112170,
    tb_seq = 25,
    total = 489717
  )


# Philippines, PSA data obtained from NTP for 2024 
# https://psa.gov.ph/content/registered-deaths-philippines-2024

vr <- vr %>% 
  mutate(
    tb = if_else(iso3 == "PHL" & year == 2024, 24917, tb),
    ill_def = if_else(iso3 == "PHL" & year == 2024,  32934 , ill_def),
    total = if_else(iso3 == "PHL" & year == 2024, 701884, total)
  )


# Mongolia, data collected during Epi Review by Fukushi Morishita


vr <- vr %>% 
  add_row(
    iso3    = "MNG",
    year    = 2010:2015,
    hiv     = NA,
    tb      = c(325, 261, 274, 268, 256, 259),
    ill_def = c(111, 99, 103, 102, 129, 118),
    tb_seq  = NA,
    total   = c(17276, 17127, 16923, 16192, 16495, 16374)
  )


# Fix with data from direct country communication
#RUS, CHN

tofix2 <- c("RUS","CHN")
vr[iso3 %in% tofix2, ]




###
# Process raw VR data, after cleaning and merging DDI + GTB VR data
###


(dim(vr))

# Missing TB deaths
# Description and imputation

miss.tb <- unique(vr$iso3[is.na(vr$tb)])

vr[iso3 %in% miss.tb, tb.nm := sum(!is.na(tb)), by = iso3]
vr[, table(tb.nm)]
vr[tb.nm == 0]

# Drop countries with TB deaths always missing
(dim(vr))
vr <- vr[tb.nm > 0 | iso3 %ni% miss.tb]
(dim(vr))

# If one Value of TB death available
# Not possible to impute
vr[tb.nm == 1]

# Proportion of TB deaths among observed
vr[, prop.tb := tb / total]

#Impute TB deaths if >= 2 data points (2 observed values of TB deaths)
vr[tb.nm >= 2, tb.imp := na_interpolation(round(prop.tb * total)), by =
     iso3]
(summary(vr$tb.imp))

#Replace TB deaths in the new imputed var tb.imp by the observed value in tb

vr[!is.na(tb) & is.na(tb.imp), tb.imp := tb]
(vr['ZAF'])
(vr['RUS'])
(vr['KAZ'])



# clean up
#
vr[, tb.nm := NULL]
rm(miss.tb)



# Missing ill-defined
# Same process, impute if >=2 data points

miss.ill <- unique(vr$iso3[is.na(vr$ill_def)])

vr[iso3 %in% miss.ill, ill.nm := sum(!is.na(ill_def)), by = iso3]
(vr[, table(ill.nm)])

# Garbage code = proportion of ill defined cause of deaths among all deaths

vr[, garbage := ill_def / total]

vr[ill.nm >= 2, garbage.imp := na_interpolation(garbage), by = iso3]
(summary(vr$garbage.imp))
vr[!is.na(garbage) & is.na(garbage.imp), garbage.imp := garbage]
# (vr['ZAF'])
# (vr['RUS'])
# (vr['KAZ'])
# (vr['MDA'])
# (vr['BRA'])



# clean up
#
vr[, ill.nm := NULL]
rm(miss.ill)


# missing tb_seq
#
vr[, sum(is.na(tb_seq))]


# total TB deaths
#
vr[, totaltb := rowSums(cbind(vr$tb.imp, vr$tb_seq), na.rm = TRUE)]

# proportion sequelae
#
vr[, seq.prop := tb_seq / totaltb]


setkey(vr, iso3, year)



# proportion of TB deaths out of well documented deaths
#
vr[, tb.prop := totaltb / (total - ill_def)]

#If missing ill-defined, use imputed ill-defined proportion
vr[is.na(tb.prop), tb.prop := totaltb / (total - (garbage.imp*total))]


# check
#
try(vr[!is.na(tb.prop), test.isbinom(tb.prop)], silent=T)
vr[tb.prop>1]
vr[is.na(tb.prop)]

# Drop AZE 2023 data, not reliable
# Drop also tb.prop missing because of lack of information for imputation, GRL and MAC

vr=vr[!(tb.prop>1)]
vr[!is.na(tb.prop), test.isbinom(tb.prop)]
dim(vr)


(vr['RUS'])
(vr['KAZ'])
(vr['MDA'])
(vr['BRA'])


# Duplicates in iso/years
sel=duplicated(vr[,c("iso3","year")])
vr[sel==TRUE]


# # VR quality, new quality assessment published in 2024
vrqual <- fread('data/mortality/vrquality2024.csv')
vrqual$country=vrqual$`Country territory or area`

vrqual <- vrqual[country != ""]
(vrqual)


## Differences in country names:
(vrqbad <- setdiff(vrqual[, unique(country)],
                  cty[, country]))

#Replace country name in vrqual from the corresponding correct name in cty

(newnm <- grep('Macedonia', cty[, country], value = TRUE))
vrqual[country == grep('The former Yugoslav Republic of Macedonia', vrqbad, value = TRUE), country := newnm]
(newnm <- grep('Britain', cty[, country], value = TRUE))
vrqual[country == grep('United Kingdom', vrqbad, value = TRUE), country := newnm]
(newnm <- grep('Türkiye', cty[, country], value = TRUE))
vrqual[country == grep('Turkyie', vrqbad, value = TRUE), country := newnm]

vrqual[country == "Netherlands", country := "Netherlands (Kingdom of the)"]

(newnm <- grep('Czech', cty[, country], value = TRUE))
vrqual[country == grep('Czech Republic', vrqbad, value = TRUE), country := newnm]




vrqual <- merge(vrqual, cty[,.(country, iso3)], by='country', all.x =TRUE)
sum(is.na(vrqual$iso3))==0 # check


#Merge VR and VR qual dataset
vr <-
  merge(vr, vrqual[, .(iso3,
                       codqual = Quality)], by = 'iso3')




#VR to keep if high or medium quality: generate a variable keep.vr
vr[codqual %in% c("high", "medium"), keep.vr := T]
vr[codqual %ni% c("high", "medium"), keep.vr := F]
vr[, summary(keep.vr)]

# #Remove the char % from the coverage
# remove.pc <- function(x) as.numeric(gsub("%", "", x))
# 
# vr[, min.coverage := remove.pc(min.coverage)]
# vr[, max.coverage := remove.pc(max.coverage)]
# vr[, min.usability := remove.pc(min.usability)]
# vr[, max.usability := remove.pc(max.usability)]


### Check VR imported from GTB

(vrgtb.lst <- unique(vrgtb$iso3))
vr[iso3=="BRA",.(year,tb,ill_def,total,prop.tb,tb.prop)]
vr[iso3=="GEO",.(year,tb,ill_def,total,prop.tb,tb.prop)]
vr[iso3=="KAZ",.(year,tb,ill_def,total,prop.tb,tb.prop)]
vr[iso3=="MDA",.(year,tb,ill_def,total,prop.tb,tb.prop)]
vr[iso3=="RUS",.(year,tb,ill_def,total,prop.tb,tb.prop)]
vr[iso3=="SRB",.(year,tb,ill_def,total,prop.tb,tb.prop)]



# GHE: use the updated 2023 GHE, saved in ghe2023.rda and processed in script 01b
#

ghe2023=ghe2023[!is.na(dths), ghe.env := dths]

vr2 <- merge(vr, ghe2023[,.(iso3, year, ghe.env)], by=c('iso3','year'), all.x=TRUE, all.y=FALSE)
(dim(vr))
(dim(vr2))

# default envelope
vr2[, env := ghe.env]  

vr2[, env.nm := sum(!is.na(env)), by=iso3]
vr2[, table(env.nm)]

#Impute missing envelope if more than 2 data points
vr2[env.nm >=2, env := na_interpolation(env), by=iso3]
vr2[, sum(is.na(env))]


vr2[, vr.coverage := pmin(1, total / env)] # VR coverage
vr2[is.na(vr.coverage), vr.coverage := total] # non-GHE estimate where missing GHE


# For Russia, use total death as enveloppe for 2020:yr, missing GHE

sel=vr2$iso3=="RUS" & vr2$year %in% 2020:yr
vr2[sel, ghe.env := total]
vr2[sel, env := total]

# For Brazil, use total death as enveloppe for 2022:yr, missing GHE

sel=vr2$iso3=="BRA" & vr2$year %in% 2022:yr
vr2[sel, ghe.env := total]
vr2[sel, env := total]


# proportion of TB deaths out of well documented deaths
#
vr2[, tb.prop := totaltb / (total - ill_def)]

#If missing ill-defined, use imputed ill-defined proportion
vr2[is.na(tb.prop), tb.prop := totaltb / (total - (garbage.imp*total))]



#
# adjusted TB deaths
#
vr2[, tb.adj := env * tb.prop]

vr <- copy(vr2)

# check
#
vr[!is.na(tb.prop), test.isbinom(tb.prop)]




# # SDs
#
# assume TB deaths between 0.5 and 1.5 times observed $t$ rate among
# garbage $g$ and non covered $c$
#
# $t_{adj} = \frac{t}{c(1-g)}$
#
# $\text{SD}(t_{adj}) = \frac{t}{4} \left(\frac{1}{c(1-g)} - 1\right)$
#
vr[, tb.adj.sd := (tb.adj / 4) * (1 / (vr.coverage * (1 - garbage)) - 1)]

vr[!is.na(tb.adj.sd), test.ispos(tb.adj.sd)]
vr[keep.vr == T & is.na(tb.adj.sd), tb.adj.sd := tb.adj * .2]
vr[keep.vr == T, summary(tb.adj.sd / tb.adj)]


vr2  <-
  merge(vr[iso3 %ni% 'VIR'], pop[, .(iso3, year, pop = e.pop.num)], by =
          c('iso3', 'year'), all.x = TRUE)
dim(vr[iso3 %ni% 'VIR'])
dim(vr2)
vr2[is.na(pop)]

# exclude MNE and SRB 2000:2004
vr <- copy(vr2[!is.na(pop)])

# Duplicates in iso/years
sel=duplicated(vr[,c("iso3","year")])
vr[sel==TRUE]


### Update to new IHME estimates

# Code correct from IHME (GBD 2021, accessed June 2024)

load(here('inc_mort/analysis/ihmetb.rda'))
load(here('inc_mort/analysis/ihmeall.rda'))


# envelope ratios GHO/IHME
#
ihme.all <-
  ihmeall[year >= 2000 &
         cause_name == 'All causes' & measure_name == 'Deaths']
ihme.all <- ihme.all[!is.na(iso3)]
gbd <-
  merge(ihme.all[, .(iso3,
                     year,
                     ihme = val,
                     ihme.lo = lower,
                     ihme.hi = upper)],
        ghe2023,
        by = c('iso3', 'year'),
        all.y = T)


# missing IHME envelopes?
#
gbd[, sapply(.SD, function(x)
  sum(is.na(x)))]


# WHO/IHME env ratio
#
gbd[, env.ratio := ghe.env / ihme]

vr2 <- merge(vr, gbd[!is.na(env.ratio),.(iso3,year,ihme.env=ihme,env.ratio)], by=c('iso3','year'), all.x=TRUE)
(dim(vr))
(dim(vr2))

vr <- copy(vr2)


# simple checks
ovr[year>=2010, table(year)]
vr[year>=2010, table(year)]

ovr[year>=2010, sum(tb, na.rm=TRUE), by=year]
vr[year>=2010, sum(tb, na.rm=TRUE), by=year]








### Last minute tweak: drop DOM data for 2023-2025
dim(vr)
vr <- vr[!(iso3 == "DOM" & year >= 2023)]
dim(vr)




#  save vr dataset
#

attr(vr, "timestamp") <- Sys.Date() #set date
save(vr, file = 'inc_mort/analysis/vr.rda')
#fwrite(vr, file = paste0('inc_mort/analysis/csv/vr_', Sys.Date(), '.csv'))






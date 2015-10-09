######################################################################################
## Download and Read Data                                                           ##
######################################################################################

# From: http://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-
#       and-Reports/Medicare-Provider-Charge-Data/Physician-and-Other-Supplier.html

# Couldn't figure out the URL needed to read the data directly from web into R
# because there is a user agreement before downloading

setwd( "C:/Users/David/Classes/NYCdataScienceAcademy/R-Intermediate/FinalProject" )
file = "MedicareProviderData/Medicare-Physician-and-Other-Supplier-PUF-CY2012.txt"
MPD = read.delim( file, stringsAsFactors=FALSE )

######################################################################################
## Preprocessing: Exploring and Preparing Data                                      ##
######################################################################################

# Look for variables not needed
names( MPD )

# Delete address variables (except for state)
MPD <- MPD[ -c(8:11, 13) ]
# Removed: nppes_provider_street1, nppes_provider_street2, nppes_provider_city
# nppes_provider_zip, nppes_provider_country

# Delete provider name variables (identify docs with npi)
names( MPD )
MPD <- MPD[ -c(2:4) ]
# Removed: "nppes_provider_last_org_name", "nppes_provider_first_name",
#          "nppes_provider_mi"

# Study the remaining variables for meaning, applicability, cleaning and reduction

unique( MPD$npi )
# All npi's are are 10 digits long except npi = "1"
# Look at all npi = 1 observations
MPD[ MPD$npi == 1, ]
# Only the first observation, and all data is missing from it, so delete
MPD <- MPD[ -1, ]

unique(MPD$nppes_credentials)
# Eliminate this variabel because there are any number of combinations of different
# credentials, which doesn't tell us anything. More useful is provider_type
MPD <- MPD[ , -2 ]

table(MPD$nppes_provider_gender)
# 380K Blank, 2.2M Female, 6.6M Male 
# Blank entries correspond to Organizations (next variable)

table( MPD$nppes_entity_code )
# 8.8 M individuals(I), 380K organizations(O), and 7 dots(.)?
# Look at the seven dot entity code observations
MPD[ MPD$nppes_entity_code == ".", ]
# The rest of the data is there. Delete.
MPD <- MPD[ !MPD$nppes_entity_code == ".", ]

unique( MPD$nppes_provider_state )
table( MPD$nppes_provider_state )

sum(is.na(MPD))
# No NA's in the whole set
length(unique( MPD$provider_type ))
# Top 10 most popular provider types
head( sort( table( MPD$provider_type ), decreasing=T ), 10 )
# Diagnostic Radiology, Internal Medicine, Family Practice, Cardiology, Orthopedic
# Surgery, Nurse Practitioner, Ophthalmology, Anesthesiology, Physician Assistant,
# Emergency Medicine 

table( MPD$medicare_participation_indicator )
# Only 4,468 N's, over 9 million Y's

table( MPD$place_of_service )
# 3.5 to 5.5 ratio of Facility(F) to Office(O)
# But we already have the variabel "entity_code" telling us if the provider
# is an individual or organization, so this seems redundant and will remove
names( MPD )
MPD <- MPD[ -7 ]

length(unique( MPD$hcpcs_code ))
# almost 6K
# Top 5 most frequent hcpcs codes?
sort( table( MPD$hcpcs_code ),decreasing=TRUE )[ 1:5 ]
# A: hcpcs code:  99213    99214    99232    99212    99204
#    frequency: 396,348  341,828  165,145  160,242  157,567

# Top 5 most frequent hcpcs descriptions
sort( table( MPD$hcpcs_description ),decreasing=TRUE )[ 1:5 ]
# Different numbers than for codes, but not relevant that descriptions vary

# Q: Is each observation(row) a unique provider, a unique patient visit,
#    or a unique service provided to a patient?

length( unique( MPD$npi ) )
nrow( MPD ) / length( unique( MPD$npi ) )
sort( table( MPD$npi ),decreasing=TRUE )[ 1 ]

# A: There are 880,645 unique provider id#s out of over 9 million
#    observations(rows), on average 10.4 observations per provider.
#    Npi #1538144910 is the most often occurring (622 times). So each
#    observation is not unique provider info. A date variable is not
#    provided, making it hard to tell if each obs is a unique visit.
#    But the fact that there is a 'hcpcs_code' variable shows that each
#    observation must be a unique service provided, and the line_srvc_cnt
#    variable shows that the service may have been provided more than once.
#    But the number of times performed in a unique visit, or in the year?

summary( MPD$line_srvc_cnt )
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    6      21      45     240     124 4579000       1 

# Since the line_srvc_cnt can be so hi (4.5M), it must represent the number of times
# the service is provided over the course of the year.

summary( MPD$bene_unique_cnt )

# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 11.0     17.0     33.0     91.2     77.0 604600.0        1 

# The number of unique people who received that service from that doctor over the
# course of the year.

summary( MPD$bene_day_srvc_cnt )

# Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
# 11.0      20.0      42.0     150.9     111.0 1227000.0         1 

# Same as bene_day_srvc_cnt, but removes extras if a patient had multiple of the same
# service done in the same day (ie, putting in 5 stints = 1 service)

# Now the statistical variables on dollar amounts:

summary( MPD$average_Medicare_allowed_amt )

# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 0.00    21.79    60.19    96.29   108.60 46270.00        1 

summary( MPD$average_submitted_chrg_amt )

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.0    50.0   122.8   298.2   252.5 98000.0       1 

summary( MPD$average_Medicare_payment_amt )

# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 0.00    17.47    44.58    74.90    82.33 37010.00        1 

# We can further reduce the variables by only keeping the statistical variables
# for amounts paid out to doctors by medicare, removing the variables for amounts
# allowed and submitted. What they're actually paid is what matters.
names( MPD )
MPD <- MPD[ -c( 12:15 ) ]

## We are left with these 13 variables to be used in our study:

# VARIABLE                            FORMAT  LABEL
# npi                                 Char    National Provider Identifier
# nppes_provider_gender               Char    Gender
# nppes_entity_code                   Char    Entity Code
# nppes_provider_state                Char    State Code
# provider_type                       Char    Provider Type
# medicare_participation_indicator    Char    Medicare Participation Indicator
# hcpcs_code                          Char    HCPCS Code
# hcpcs_description                   Char    HCPCS Description
# line_srvc_cnt                       Num     Number of Services
# bene_unique_cnt                     Num     Number of Medicare Beneficiaries
# bene_day_srvc_cnt                   Num     Number of Medicare Beneficiary/Day Services
# average_Medicare_payment_amt        Num     Average Medicare Payment Amount
# stdev_Medicare_payment_amt          Num     Standard Deviation Medicare Payment Amount


### VARIABLE DESCRIPTIONS ###

# "npi"
# National Provider Identifier for the performing provider on the claim.

# "nppes_provider_gender"
# Provider's gender When registered as an individual (entity type code='I'),
# Blank when registered as an organization (entity type code = 'O')

# "nppes_entity_code"
# Type of entity: Code of 'I' = individuals, code of 'O' = organizations.

# "nppes_provider_state"

# "provider_type"
# Derived from the provider specialty code reported on the claim. For providers that 
# reported more than one specialty code on their claims, this is the specialty code 
# associated with the largest number of services.

# "medicare_participation_indicator"
# 'Y' or 'N' indicates whether the provider participates in Medicare and/or accepts 
# assignment of Medicare allowed amounts.

# "hcpcs_code"
# HCPCS code for the specific medical service furnished by the provider.

# "hcpcs_description"
# Description of the HCPCS code / medical service furnished by the provider.

# "line_srvc_cnt"
# How many times that doctor has performed that service; note that the metrics used
# to count the number provided can vary from service to service.

# "bene_unique_cnt"
# Number of distinct Medicare beneficiaries receiving the service.

# "bene_day_srvc_cnt"
# Number of distinct Medicare beneficiary/per day services. Since a given beneficiary 
# may receive multiple services of the same type (e.g., single vs. multiple cardiac 
# stents) on a single day, this metric removes double-counting from the line service 
# count to identify whether a unique service occurred.

# average_Medicare_payment_amt
# Average amount that Medicare paid after deductible and coinsurance amounts have been
# deducted for the line item service.

# stdev_Medicare_payment_amt
# Standard deviation of the Medicare payment amount. The standard deviation indicates
# the amount of variation from the average Medicare payment amount that exists within 
# a single provider, HCPCS service, and place of service.



######################################################################################
##  DATA SUMMARY                                                                    ##
######################################################################################

# Each observation represents a unique doctor(npi) providing a particular         
# service(hcpcs), (line_srvc_cnt) many times, to (bene_unique_cnt) many unique    
# patients over the course of the year.                                           
                                                                                        
# The (average_Medicare_payment_amt) is the mean amount payed by medicare for the 
# particular service (hcpcs) across all providers who provided that service.      
# The (stdev_Medicare_payment_amt) is the variation from that average that        
# exists for that particular provider and HCPCS service.                          

# Since each observation represents a unique npi AND hcpcs, the MPD data set gives us
# a summary of each service that each doctor has performed in the year.

# Another useful data set would sum certain variables for each doctor across all
# services provided that year.


######################################################################################
## Data Transformation (MPD into DOC_TOTALS)                                        ##
######################################################################################

install.packages( "data.table" )
library( data.table )
# First copy the MPD data.frame to a data.table(MPD_T) because the following sums and
# subsetting of such a large data.frame will not run, but data.table can handle
MPD_T <- data.table( MPD )
DOC_TOTALS <- MPD_T[ ,  # doc's total number of services provided
                        list( num_services = sum( line_srvc_cnt ),
                              
                        # doc's total unique patients seen
                        num_patients = sum( bene_unique_cnt ),
                        
                        # doc's total income from medicare
                        total_payment = sum( average_Medicare_payment_amt
                                             * line_srvc_cnt ),
                        
                        # doc's average number of services per patient
                        serv_per_patient = sum( line_srvc_cnt ) # 
                                          / sum( bene_unique_cnt ),
                        ),
                        by = "npi" ]

######################################################################################
## Descriptive Statistics                                                           ##
######################################################################################

# Study based on each npi's average 'serv_per_patient' statistic because that is
# irrespective of the size of the doctor group and cost differeneces between services. 

summary(DOC_TOTALS$serv_per_patient)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.769    1.082    1.472    2.981    2.277 3669.000 


# Plot each doctor's average number of services provided to each patient in 2012
library( ggplot2 )
qplot( npi, serv_per_patient, data = DOC_TOTALS )
# 10 doctors with avg services/patient > 1,000!


nrow(DOC_TOTALS[DOC_TOTALS$serv_per_patient > 5,]) / nrow(DOC_TOTALS)
# Only 12.5% of doctors are providing an avg of over 5 services/patient


nrow(DOC_TOTALS[DOC_TOTALS$serv_per_patient > 10,]) / nrow(DOC_TOTALS)
# Only 5% of doctors are providing an avg of over 10 services/patient


nrow(DOC_TOTALS[DOC_TOTALS$serv_per_patient > 20,]) / nrow(DOC_TOTALS)
# Only 1% of doctors are providing an avg of over 20 services/patient


# Look at the highest one (>3500)
DOC_TOTALS[ DOC_TOTALS$serv_per_patient > 3500, ]
# npi=1538497235
MPD[ MPD$npi == "1538497235", ]
# Org, has only seen 14 patients, for only 1 sevice (hemophilia drug/treatment),
# but billed a total of 51,372 codes, and average of 3,670 codes/patient/year. 


# Look closely at the 10 docs with avg serv_per_patient > 1,000 for common theme.
DOC_TOTALS[ DOC_TOTALS$serv_per_patient > 1000, ]
# We need more info/variables brought over from MPD set.


# Bring gender, entity_code, state, and provider_type into DOC_TOTALS by npi
DOC_TOTALS <- as.data.frame( DOC_TOTALS ) # doesn't work on a data.table
add <- cbind( MPD$nppes_provider_gender, MPD$nppes_entity_code,
              MPD$nppes_provider_state, MPD$provider_type, MPD$npi )
library( data.table )
add <- as.data.table( add )
names( add ) <- c( "gender", "entity", "state", "specialty", "npi" )
# reduce the add dataset to unique npi's by subsetting non-duplicates
add_reduce <- subset(add, !duplicated(add$npi)) 
DOC_TOTALS$npi <- as.character( DOC_TOTALS$npi )
library( dplyr )
DOC_INFO <- left_join( DOC_TOTALS, add_reduce, by="npi", copy=T )



# Now look at the info on the 10 docs with avg serv_per_patient > 1,000
club_1000 <- DOC_INFO[ DOC_INFO$serv_per_patient > 1000, ]
qplot( npi, serv_per_patient, data = club_1000, color = specialty,
       shape = entity, size = 2, main = "The 1,000 Services Per Patient Club" )
# All are organizations and Suppliers



# Now look at the info on docs with avg serv_per_patient > 200
club_200 <- DOC_INFO[ DOC_INFO$serv_per_patient > 200, ]
qplot( npi, serv_per_patient, data = club_200, color = specialty,
       shape = entity, size = 2, main = "The 200 Services Per Patient Club" )
# Many are organizations (entity = O)
# Variety of specializations, but 17% of all specialties represented
length(unique(club_200$specialty)) / length(unique(DOC_INFO$specialty))


# Now look at the info on docs with avg serv_per_patient > 100
club_100 <- DOC_INFO[ DOC_INFO$serv_per_patient > 100, ]
qplot( npi, serv_per_patient, data = club_100, color = specialty,
       shape = entity, main = "The 100 Services Per Patient Club" )
# Many are organizations (entity = O)
# Variety of specializations, but still only 35% of all specialties represented
length(unique(club_100$specialty)) / length(unique(DOC_INFO$specialty))


# Create dataframe showing the .90 quantile of serv_per_patient for each specialty
docMelt <- melt( DOC_INFO, id = "specialty", measure.vars = "serv_per_patient" )
upper.10 <- dcast( docMelt, specialty ~ variable, quantile, .90 )


# Merge the upper.10 values into the DOC_INFO dataframe and call it "serv_tail_per_spec"
DOC_INFO <- merge(DOC_INFO, upper.10, by="specialty")
DOC_INFO$serv_per_patient <- DOC_INFO$serv_per_patient.x
DOC_INFO$serv_tail_per_spec <- DOC_INFO$serv_per_patient.y
DOC_INFO <- DOC_INFO[ , -c(6,10) ]


# Create a binary dummy variable, 1 = fraud and 0 = no fraud, based on being over
# or under the .90 quantile of serv_per_patient in each specialty(serv_tail_per_spec)
DOC_INFO$fraud <- ifelse(DOC_INFO$serv_per_patient > DOC_INFO$serv_tail_per_spec, 1, 0)
DOC_INFO$fraud <- factor(DOC_INFO$fraud)
(tb1 <- table(DOC_INFO$fraud))
prop.table(tb1)
# 88K (10%) of the npi's are tagged as fraudulant on the basis of being in the .90
# quartile for services per patient within their specialty in the year 2012.


######################################################################################
## Prediction Modeling                                                              ##
######################################################################################


# separate into training and test sets
library(caret)
trainIndex <- createDataPartition(DOC_INFO$fraud, p = 0.6, list = FALSE, times = 1)
Train <- DOC_INFO[trainIndex, ]
Test <- DOC_INFO[-trainIndex, ]

# make sure that 60% of obs tagged as fraudulant are in the Train set
sum( as.numeric( Train$fraud ) ) / sum( as.numeric( DOC_INFO$fraud ) )
# Yes

# Convert explanatory variables to factor
Train$gender <- factor(Train$gender)
Train$entity <- factor(Train$entity)
Train$state <- factor(Train$state)
Train$specialty <- factor(Train$specialty)
Test$gender <- factor(Test$gender)
Test$entity <- factor(Test$entity)
Test$state <- factor(Test$state)
Test$specialty <- factor(Test$specialty)


######################################################################################
## Decision Tree                                                                    ##
######################################################################################

library(rpart)
install.packages( "rattle" )
install.packages( "rpart.plot" )
library( rattle )

# Use all variables except: npi (just an identifier),and serv_per_patient and
#                           serv_tail_per_spec (used to develop fraud)
treemodel_1 <- rpart( fraud ~ total_payment + specialty + num_services + num_patients +
                    gender + entity + state,
                    data = Train,
                    control = rpart.control( minbucket = 1 ) )
# Model summary statistics
treemodel_1
# Fancy decision tree plot
fancyRpartPlot( treemodel_1 )
# Prediction 
newdoc1 <- data.frame( total_payment = 100000, specialty = 'Anesthesiology', 
                       num_services = 150, num_patients = 50, gender = 'M', 
                       entity = 'O', state = 'CA'
                       )
predict( treemodel_1, newdata = newdoc1 )
#         0          1
# 0.9295142 0.07048583

# So we predict with 92% confidence that newdoc did not engage in fraud



# Use only variables that do not go into developing "fraud"
treemodel_2 <- rpart( fraud ~ total_payment + gender + entity + state + specialty,
                 data = Train,
                 control = rpart.control( minbucket = 1 ) )
# Model summary statistics
treemodel_2
# Only one node that splits to the 9 to 1, "no-fraud" to "fraud", ratio 
# Prediction 
newdoc2 <- data.frame( total_payment = 100000, gender = 'M', entity = 'O', state = 'CA',
                       specialty = 'Anesthesiology')
predict( treemodel_2, newdata = newdoc2 )
#         0         1
# 0.8999953 0.1000047

# So we predict with 90% confidence that newdoc did not engage in fraud



# change cp control from default .01 to .005 to get more splitting/nodes
treemodel_3 <- rpart( fraud ~ total_payment + gender + entity + state + specialty,
                      data = Train,
                      control = rpart.control( cp = .005 ) )
# Model summary statistics
treemodel_3
# Fancy decision tree plot
fancyRpartPlot( treemodel_3 )
# Better: 2 splits on total_payment and specialty, so gender, entity, state still
#         not significant
# Prediction 
predict( treemodel_3, newdata = newdoc2 )
#         0          1
# 0.9160747 0.08392528

# So we predict with 92% confidence that newdoc did not engage in fraud



######################################################################################
## Logistic Regression                                                              ##
######################################################################################

# Estimates odds of success ( 1 = default )

logmodel_1 <- glm( fraud ~ total_payment + gender + entity + state + specialty,
                 data = Train, family = 'binomial' )
summary( logmodel_1 )
# (intercept), total_payment, gennderF, genderM and some specialties are significant 

logmodel_1.pre <- predict( logmodel_1,type = 'response' )
pre_1 <- ifelse( logmodel_1.pre > 0.5, 1, 0 )
(table1 <- table( pre_1, Train$fraud ))
# pre2      0      1
#    0 475211  52626
#    1    333    215

sum(diag(table1))/sum(table1)
# 90% success

# run the Test dataset on logmodel_1
log_pred_test1 <- predict( logmodel_1, newdata = Test )
pre_1 <- ifelse( log_pred_test1 > 0.5, 1, 0 )
(table1 <- table( pre_1, Test$fraud )) 
sum(diag(table1))/sum(table1)
# 90% success on Test data too



# Reduce to total_payment and gender
logmodel_2 <- glm( fraud ~ total_payment + gender,
                  data = Train, family = 'binomial' )
summary( logmodel_2 )
# all significant

logmodel_2.pre <- predict( logmodel_2, type = 'response' )
pre_2 <- ifelse( logmodel_2.pre > 0.5, 1, 0 )
(table2 <- table( pre_2, Train$fraud ))

# pre2      0      1
#    0 475254  52686
#    1    290    155

sum(diag(table2))/sum(table2)
# 90% success

# run the test dataset on logmodel_2
log_pred_test2 <- predict( logmodel_2, newdata = Test )
pre_2 <- ifelse( log_pred_test2 > 0.5, 1, 0 )
(table2 <- table( pre_2, Test$fraud )) 
sum(diag(table2))/sum(table2)
# 90% success on Test data too


######################################################################################
## Linear Regression                                                                ##
######################################################################################

lmodel_1 <- lm( formula = serv_per_patient ~ total_payment + gender + entity + state +
                    specialty, data = Train )
summary( lmodel_1 )
# significant: total_payment, gender, state=MP, and many specialties
plot( lmodel_1 )
# 118, 348, and 375 are outliers

# Performance
pred_1 <- predict( lmodel_1, newdata = Test )
rmse_1 <- sqrt( mean( ( Test$serv_per_patient - pred_1 )^2 ) )
rmse_1
# [1] 11.07031

mean(DOC_INFO$serv_per_patient)
# [1] 2.980543, so rmse = 11 is really bad



lmodel_2 <- lm( formula = serv_per_patient ~ total_payment + gender,
               data = Train )
summary( lmodel_2 )
# ALL significant

# Performance
pred_2 <- predict( lmodel_2, newdata = Test )
rmse_2 <- sqrt( mean( ( Test$serv_per_patient - pred_2 )^2 ) )
rmse_2
# [1] 11.55449
# it got worse

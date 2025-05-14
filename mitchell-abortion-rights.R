
## Project:  SOC 302 chi2 Project 
# Located:   Class folder on ELSA cluster
# File Name: mitchell-chi2.R
# Date:      3/31/25
# Who:       Lindsay Mitchell


####################################################################################
############              Pre-Analysis: settings, packages, and data    ############
####################################################################################


### Settings + Packages
library(dplyr)
library(psych)
options(max.print=999999)

### Load data 
GSS <- read.csv("GSS2022.csv")

####################################################################################
############              PHASE 1: CLEAN DATA FOR ANALYSIS              ############
####################################################################################


## Steps of cleaning variables Clear vars
# Step 1: Examine variable and coding schema: Table() / summary() 
table(GSS$abany)
# Step 2: Recode (if necessary/warrented): mutate(), ifelse(), etc
# Step 3: Confirm: table() / summary()



############                     DEPENDENT VARIABLE                     ############
############          belief of right to an abortion for any reason     ############

# STEP 1: Examine variable and coding schema 
table(GSS$abany)

# STEP 2: Create Dummy Variables for each category of belief of abortion or not
GSS <- mutate(GSS, yes_abany = ifelse(abany == 1, 1, 0)) 
GSS <- mutate(GSS, no_abany = ifelse(abany == 2, 1, 0)) 

# STEP 3: Confirm creation by looking at abany and each dummy variable
table(GSS$abany, GSS$yes_abany)
table(GSS$abany, GSS$no_abany)

############                  INDEPENDENT VARIABLE                 ############
############                  political affiliation                ############

# STEP 1: Examine variable and coding schema 
table(GSS$partyid)
# STEP 2: Dummy Variables for each party identification 
GSS <- mutate(GSS, democrat = ifelse(partyid == 0 | partyid == 1 | partyid == 2, 1, 0))
GSS <- mutate(GSS, independent = ifelse(partyid == 3, 1, 0))
GSS <- mutate(GSS, republican = ifelse(partyid == 4 | partyid == 5 | partyid == 6, 1, 0))
GSS <- mutate(GSS, political_other = ifelse(partyid == 7, 1, 0))

# STEP 3: Confirm creation (if necessary)
table(GSS$partyid, GSS$democrat)
table(GSS$partyid, GSS$independent)
table(GSS$partyid, GSS$republican)
table(GSS$partyid, GSS$political_other)

############                  INDEPENDENT VARIABLE                 ############
############          confidence in people running healthcare      ############

table(GSS$conmedic)

GSS <- mutate(GSS, great_deal = ifelse(conmedic == 1, 1, 0))
GSS <- mutate(GSS, only_some = ifelse(conmedic == 2, 1, 0))
GSS <- mutate(GSS, hardly_any = ifelse(conmedic == 3, 1, 0))

table(GSS$conmedic, GSS$great_deal)
table(GSS$conmedic, GSS$only_some)
table(GSS$conmedic, GSS$hardly_any)

############                  INDEPENDENT VARIABLE                 ############
############                        religion                       ############

table(GSS$relig)

GSS <- mutate(GSS, christian = ifelse(relig == 1 | relig == 2 | relig == 10 | 
                                        relig == 11 , 1, 0))
GSS <- mutate(GSS, religion_other = ifelse(relig == 3 | relig == 5 | relig == 6 |
                                             relig == 7 | relig == 8 | relig == 9 |
                                             relig == 12 | relig == 13, 1, 0))
GSS <- mutate(GSS, none = ifelse(relig == 4, 1, 0))

table(GSS$relig, GSS$christian)
table(GSS$relig, GSS$religion_other)
table(GSS$relig, GSS$none)

############                 INDEPENDENT VARIABLE                 ############
############   region of country respondent completed interview   ############

table(GSS$region)

GSS <- mutate(GSS, north_east = ifelse(region == 1 | region == 2, 1, 0))
GSS <- mutate(GSS, midwest = ifelse(region == 3 | region == 4, 1, 0))
GSS <- mutate(GSS, south = ifelse(region == 5 | region == 6 | region == 7, 1, 0))
GSS <- mutate(GSS, west = ifelse(region == 8 | region == 9, 1, 0))

table(GSS$region, GSS$north_east)
table(GSS$region, GSS$midwest)
table(GSS$region, GSS$south)
table(GSS$region, GSS$west)

############                 INDEPENDENT VARIABLE                 ############
############         highest level of education completed         ############
summary(GSS$educ)

#step 2: no recoding, treat as continuous variable 
#step 3: n/a

############                      CONTROL VARIABLE                 ############
############                           gender                      ############
 
#step 1: examine variable
table(GSS$sex)

#step 2: recode as a dummy variable
GSS <- mutate(GSS, man = ifelse(sex == 1, 1, 0))
GSS <- mutate(GSS, woman = ifelse(sex == 2, 1, 0))

#step 3: confirm 
table(GSS$sex, GSS$man)
table(GSS$sex, GSS$woman)

############                      CONTROL VARIABLE                 ############
############                           income                      ############

summary(GSS$realinc)

#step 2: no recoding, treat as continuous variable 
#step 3: n/a

####################################################################################
############              PHASE 2: CREATE MY DATASET                    ############
####################################################################################

### STEP 1: Create a list of variables to keep
my_varlist <- c("yes_abany", "no_abany", 
                "democrat", "independent", "republican", "political_other", 
                "none", "religion_other", "christian", "great_deal", "only_some", 
                "hardly_any", "south", "midwest", "north_east", "west", "educ")

### STEP 2: create a new dataset with only your variables and complete case
my_dataset <- GSS %>%
  select(all_of(my_varlist)) %>%
  filter(complete.cases(.))

### STEP 3: Gather Summary Statistics and confirm valid dataset construction
describe(my_dataset)


####################################################################################
############              PHASE 3: Descriptive Statistics     ############
####################################################################################
# TABLE 1: DESCRIPTIVE STATISTICS HERE
describe(my_dataset)


####################################################################################
############              PHASE 4: Correlation Matrix                   ############
####################################################################################

# Correlation Matrix Table

cor(my_dataset$abany, my_dataset$partyid)

cor(my_dataset)

####################################################################################
#######                     PHASE 5: REGRESSION                        ########
####################################################################################

# Model 1: social values
model1 <- glm(yes_abany ~ democrat + independent +
                political_other, 
               data = my_dataset)
summary(model1)


# Model 2: social position
model2 <- glm(yes_abany ~ religion_other + none +
                 north_east + west + midwest, 
              data = my_dataset)
summary(model2)

# Model 3: confidence in experts
model3 <- glm(yes_abany ~ great_deal + only_some +  educ, data = my_dataset)
summary(model3)


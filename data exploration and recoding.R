####################################################################
#                                                                  #
#     In this script, we are going to generate and recode some     #
#     variables that we are going to need to conduct our analysis  #
#                                                                  #
####################################################################

### First step is to set our working directory where we have our data
setwd("/Users/luissegura/Dropbox/Survey Workshop/2019/EPIC_rsurvey")

list.files() ### this command shows the files in the working directory. We are going to import the dataset "nsduh.RData"

### Importing dataset NSDUH 2002 - 2011
load("nsduh.RData")

#############################################
#                                           #
#           Exploring the dataset           #
#                                           #
#############################################

### It is always good practice to explore your dataset and understand what are you working with. 
### The characteristics of an object are important to work it in R.

str(nsduh) ### check the structure of the this dataset

### Q: What is the class of this object?

#---------------------------------------#
#     NOTE: from now on, we will call   #
#     datasets "dataframes"             #
#---------------------------------------#

### Q: How many observations are in this dataframe? 

### Q: How many variables are in this dataframe?

### Q: Can you name 3 variables that have different classes? and what are these classes?

head(nsduh) ### print the first 6 observations in the dataframe

tail(nsduh) ### print the last 6 observations in the dataframe

### Q: How would you print the first 30 observations in the dataframe?

### Q: Are there missing data in this dataframe?

summary(nsduh) ### provides a summary of the data. For categorical variables (factors), we get the N for each level of such variable.
               ### For numeric variables, we get some summary statistics (e.g. mean, median, quantiles). 
               ### If there are missings it will show the total of "NA's". 

#---------------------------------------#
#     NOTE: in R, missings are called   #
#           "NA's"                      #
#---------------------------------------#
 

### We can call on other packages to get a prettier and more informative  summary of the dataframe

library(skimr) ### we load the package skimr

skim(nsduh) ### skim is a useful function from the package skimr to get a better summary of dataframes. 
            ### spend some time visually inspecting the output of these command.


#############################################
#                                           #
#           Generating and Recoding         #
#                                           #
#############################################

### We are going to create new variables of race, age, period indicator, NMUPO, NMUPO frequency, and survey weights.

### Q: How would you find the names of the original variables in the nsduh dataframe? 

names(nsduh) ### this command returns a list containing the names of an object. 
             ### In the case of a dataframe, it returns the names of the columns (variable names).

#############################################
# Create a new and recoded variable of race #
#############################################

### Explore the original variable

class(nsduh$newrace2) ### what class is this variable?

table(nsduh$newrace2) ### tabulate the original variable

levels(nsduh$newrace2) ### levels of this variable

### Can you think of other ways of exploring this variable?

summary(nsduh$newrace2)

skim(nsduh$newrace2)

str(nsduh$newrace2)

plot(nsduh$newrace2)


### Let's create a new race variable with the following levels 1 = Whites, 2 = Blacks, 3 = Hispanics, 4 = Other

nsduh$race <- ifelse(nsduh$newrace2 == "NonHisp Native Am/AK Native" |  
                       nsduh$newrace2 == "NonHisp Native HI/Other Pac Isl" | 
                       nsduh$newrace2 == "NonHisp Asian" | 
                       nsduh$newrace2 == "NonHisp more than one race", 4,
                     ifelse(nsduh$newrace2 == "Hispanic", 3, 
                            ifelse(nsduh$newrace2 == "NonHisp White", 1, 2))) 

table(nsduh$newrace2, nsduh$race, useNA = "always") ### check the new variable against the original variable

### Q: What class is this variable?
class(nsduh$race)

### Transform the new race variable to be a factor and assign labels
nsduh$race <- factor(nsduh$race, labels = c("Whites", "Blacks", "Hispanics", "Others"))

table(nsduh$race, useNA = "always") ### tabulate the new variable

###########################################################
# Create a new and recoded variable of frequency of NMUPO #
###########################################################

### Explore the original variable
table(nsduh$iranlfy)
str(nsduh$iranlfy) 
skim(nsduh$iranlfy)

### Notice that there are a lot of high values (e.g. 991, 993). What does these values mean? 
### Values of 900+ are "no use" (Refer to the codebook).

### Q: Can we explore this variable without the values that correspond to "no use"? Hint: use indexing and logicals
skim(nsduh$iranlfy[nsduh$iranlfy < 400]) ### return summary of this variable with values lower than 400

hist(nsduh$iranlfy[nsduh$iranlfy < 400]) ### draw a histogram of this variable with values lower than 400


### Create a new categoricalvariable of frequency of NMUPO 0 = no use, 1 = 1-29 days, 2 = 30 - 99 days, 3 = 100 - 365 days
nsduh$freq_nmupo <- ifelse(nsduh$iranlfy < 30, 1, 
                           ifelse(nsduh$iranlfy > 29 & nsduh$iranlfy < 100, 2, 
                                  ifelse(nsduh$iranlfy > 99 &  nsduh$iranlfy < 366, 3, 0)))

table(nsduh$freq_nmupo, useNA = "always") ### check new variable

table(nsduh$iranlfy, nsduh$freq_nmupo, useNA = "always") ### check new variable against the original variable

### That wasn't very useful. Q: How can you check that we recoded the variable correctly? 
### Hint: use the commands to explore/describe objects and indexing.
### For example:


summary(nsduh$iranlfy[nsduh$freq_nmupo == 0]) ### summarize the original variable--which is continuous--among the 0-level of the
                                              ### new variable.

summary(nsduh$iranlfy[nsduh$freq_nmupo == 1]) ### summarize the original variable--which is continuous--among the 1st level of the
                                              ### new variable.

summary(nsduh$iranlfy[nsduh$freq_nmupo == 2])

summary(nsduh$iranlfy[nsduh$freq_nmupo == 4])

### Looks like our recoding worked. 

### Transform the new NMUPO frequency variable to be a factor and assign labels
nsduh$freq_nmupo <- factor(nsduh$freq_nmupo, labels = c("No use", "1-29 days", "30-99 days", "100+ days"))

table(nsduh$freq_nmupo, useNA = "always") ### checking new variable

###########################################################
#        Create a new and recoded variable of age         #
###########################################################

### Explore the original variable

summary(nsduh$age2)

table(nsduh$age2, useNA = "always")

### We want a binary variable of age that indicates those aged 12-25 and those aged 26+

### To make our lives easier, we are going to make a numeric version of the original variable.
nsduh$age2_num <- as.numeric(nsduh$age2)

table(nsduh$age2, nsduh$age2_num, useNA = "always") ### check the new variable against the original variable

### We can incorporate in a single step the recoding step (ifelse) and the factor transformation with labels.
nsduh$age <- factor(ifelse(nsduh$age2_num < 13, 1, 2), labels = c("12-25 yrs", "26+ yrs")) 

table(nsduh$age2, nsduh$age, useNA = "always") ### check against the original variable


###########################################################
#        Create a new and recoded period indicator        #
###########################################################

### Explore the original variable
table(nsduh$year)
str(nsduh$year)
skim(nsduh$year)

### Create a new recoded ndicator of period 0 = 2002-2005, 1 = 2008-2011
nsduh$year_r <- factor(ifelse(nsduh$year < 2008, 0, 1), labels = c("2002-2005", "2008-2011"))

table(nsduh$year, nsduh$year_r) ### check against original variable

###########################################################
#                  Recode NMUPO indicator                 #
###########################################################

### Explore the original variable
table(nsduh$anlyr, useNA = "always")
class(nsduh$anlyr)

### This variable is fine, but I don't like the labels. Let's change the labels of this factor. 
### NOTE: I do not advice to rename the labels of the original variable. I recommend creating a new
### variable with different labels.

nsduh$anlyr_r <- nsduh$anlyr ### create a new variable  of NMUPO that is the same as the original NMUPO variable.

levels(nsduh$anlyr_r) <- c("No PY use", "Any PY NMUPO") ### rename the levels of the new NMUPO variable.

table(nsduh$anlyr, nsduh$anlyr_r, useNA = "always") ### check against the original variable.

###########################################################
#              Recode survey weights variable             #
###########################################################

### [SAMHSA recommends that for the] "...estimation of the *annual average
### number of individuals* who have engaged in a particular behavior based upon pooled data from
### multiple years requires adjustment to the analysis weights. These adjusted weights would be
### created as the final weight divided by the number of years of combined data”
###
### Since the analysis of Martins et al 2015 paper compares the prevalences of heroin use
### between two four-year periods: 2002-2005 and 2008-2011, we need to adjust the original
### weights. We are going to create a new weight variable that is divided by 4, which is
### the number of years pooled per period.

### Explore the original variable
str(nsduh$analwt_c)
skim(nsduh$analwt_c)

### Create a new variable of weights divided by 4
nsduh$wt_new <- nsduh$analwt_c / 4

### That is it! We are done with the recoding necessary to replicate the paper by Martins et al. 2015.
### Lets just check again the variable names in our dataset to make sure we have everything
### we need.

names(nsduh)

### At this point, you could decide to continue with your analysis or save your dataset for a later time.
### To save this new dataset with a new name

setwd("your/filepath/here/where/you/want/to/save/the/data/")

save(nsduh, file = "nsduh_ready4analysis.RData")

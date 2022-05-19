

packages <- c("plm", "Formula", "stargazer", "tidyverse", "MASS", "sandwich", "lmtest")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

data <- read.csv('Korea Income and Welfare.csv')

summary(data)
# Data set is complete. Occupation and company_size columns contain NAs in the case of people who are unemployed (33643 or 33642).
# Reson_none_worker column contains NAs for the people who were employed at that time. 

fixed <-plm(income~family_member+gender+year_born+education_level+ marriage+ religion+ occupation + company_size, data=data, 
            index=c("year"), model="within")

summary(fixed)

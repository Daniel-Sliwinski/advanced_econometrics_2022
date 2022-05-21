packages <- c("plm", "Formula", "stargazer", "tidyverse", "MASS", "sandwich", "lmtest", "rpart", "rpart.plot", "aod")

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

data$age <- data$year - data$year_born

fixed <-plm(income~family_member+gender+year_born+education_level+ marriage+ religion+ occupation + company_size, data=data, 
            index=c("year"), model="within")

summary(fixed)

#making sure that the data is full, by keeping only ids with full data available  from 2011 to 2018
table(data$year)

data_1 <- filter(data, data$year >= 2008)
full_ids <- data_1 %>% group_by(id) %>% summarise(n = n()) %>% filter(n == 11)
data_full <- filter(data_1, data_1$id %in% full_ids$id)

table(data_full$year)

data_full$region <- as.factor(data_full$region)
data_full$gender <- as.factor(data_full$gender)
data_full$education_level <- as.factor(data_full$education_level)
data_full$marriage <- as.factor(data_full$marriage)
data_full$religion <- as.factor(data_full$religion)

table(data_full$company_size)

dtree <- rpart(income ~ company_size, data_full)
rpart.plot(dtree)
#nowa kolumna -> empl: 0=bezrobotny, 1=firma mała, 2=firma średnia, 3=firma duża

data_full <- data_full %>%
  mutate(
    empl = case_when(
      is.na(company_size) ~ 0,
      company_size < 2    ~ 1,
      company_size < 9    ~ 2,
      company_size >= 9   ~ 3,
    )
  )
data_full$empl <- as.factor(data_full$empl)

data_full$married_flg <- as.factor(ifelse(data_full$marriage == 2, 1, 0))

data_full1 <- data_full[ , !(names(data_full) %in% c("year_born", "wave", "occupation", "reason_none_worker", "company_size", "marriage"))]

# Building the models
fixed <-plm(income~family_member + gender + education_level + married_flg + religion + empl + age, data=data_full1, 
            index=c("id","year"), model="within")
summary(fixed)

fixed.time <-plm(income~family_member + gender + education_level + married_flg + religion + empl + age + factor(year), data=data_full1, 
            index=c("id","year"), model="within")
summary(fixed.time)

rand <-plm(income~family_member + gender + education_level + married_flg + religion + empl + age, data=data_full1, 
            index=c("id","year"), model="random")
summary(rand)
pooled <-plm(income~family_member + gender + education_level + married_flg + religion + empl + age, data=data_full1, 
            index=c("id","year"), model="pooling")
summary(pooled)

#testing for individaul effects in fixed effects model
pFtest(fixed, pooled)
#H0 rejected, so there are significant individaul effects ( fixed model better than OLS )

#testing for individaul effects in random effects model
plmtest(pooled, type=c("bp"))
#H0 rejected, so there are significant individaul effects ( random model better than OLS )

#Hausmann test to check wether to use fixed or random effects model
phtest(fixed, rand)
#H- rejected 

#Checking for significance of time effects
pFtest(fixed.time, fixed)
plmtest(fixed, c("time"), type=("bp"))
#they are significant as we reject both H0s

# Testing for serial autocorelation
pbgtest(fixed)
# Testing for heteroskedasticity
bptest(income~family_member + gender + education_level + married_flg + religion + empl + age, data=data_full1, studentize=T)

# There is a need to use the robust variance covariance matrix
coeftest(fixed, vcov.=vcovHC(fixed, method="white1", type="HC0", cluster="group"))

# GETS procedure
# Step 1: removing religion variable, as according to robust var/covar matrix all its levels are insignificant
fixed_1 <-plm(income~family_member + gender + education_level + married_flg + empl + age, data=data_full1, 
            index=c("id","year"), model="within")
summary(fixed_1)
coeftest(fixed_1, vcov.=vcovHC(fixed_1, method="white1", type="HC0", cluster="group"))

# religion
h <- rbind(c(0,0,0,0,0,0,0,0,0,0,1,0,0,0,0))
wald.test.results = wald.test(b = coef(fixed), Sigma = vcovHC(fixed, method="white1", type="HC0", cluster="group"), L=h)
wald.test.results
# We can not reject the H0 of the Wald test, and therefore we can remove the variable religion

# Step 2: removing married_flg variable, as according to robust var/covar matrix all its levels are insignificant
fixed_2 <-plm(income~family_member + gender + education_level + empl + age, data=data_full1, 
              index=c("id","year"), model="within")
summary(fixed_2)
coeftest(fixed_2, vcov.=vcovHC(fixed_2, method="white1", type="HC0", cluster="group"))

# married_flg
h <- rbind(c(0,0,0,0,0,0,0,0,0,0,1,0,0,0,0), c(0,0,0,0,0,0,0,0,0,1,0,0,0,0,0))
wald.test.results = wald.test(b = coef(fixed), Sigma = vcovHC(fixed, method="white1", type="HC0", cluster="group"), L=h)
wald.test.results
# We can not reject the H0 of the Wald test, and therefore we can remove the variable married_flg

# Struktura:
#   - wstęp o problemie i ogólnie o zbiorze danych
#   -przegląd literatury
#   - Omówienie danych (zmiany które robiliśmy, słowniki, macierz korelacji)
#   - Jak wybraliśmy model
#   - procedura GETS
#   - interpretacja i omówienie wyników


####################
####Pricing Code#####
####################

# List of required packages
required_packages <- c("data.table", "dplyr", "lifecontingencies", "readxl", 
                       "ggplot2", "survival", "KMsurv", "caret", "tidyr")

# Check if each package is installed, and if not, install it
for (package in required_packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
}

#Install Relevant Packages
library(data.table)
library(dplyr)
library(lifecontingencies)
library(readxl)
library(ggplot2)
library("survival")
library("KMsurv")
library(caret)
library(tidyr)
getwd()

#Ensure all input files are found within the working directory!

####################
####Load Data#####
####################
data <- fread(paste0(getwd(), "/2024-srcsc-superlife-inforce-dataset.csv"))
summary(data)
unique(data$Death.indicator)
unique(data$Lapse.Indicator)
data %>% group_by(Lapse.Indicator, Policy.type) %>% summarise(count = n())
data <- data %>% mutate(Death.indicator = ifelse(is.na(Death.indicator), 0, Death.indicator),
                        Lapse.Indicator = ifelse(is.na(Lapse.Indicator), 0, Lapse.Indicator)) %>%
  mutate(Lapse.Indicator = ifelse(Lapse.Indicator == "Y", 0, Lapse.Indicator))
data <- data %>% mutate(completion_year = ifelse(Policy.type == "T20", Issue.year+19, 9999 ))

mortality_raw <- Pricingdddddd_excel("srcsc-2024-lumaria-mortality-table.xlsx", 
                            sheet = "Sheet1")

interventions_raw <- read_excel("srcsc-2024-interventions.xlsx", 
                                sheet = "Intervention_2")

mortality_data <- mortality_raw %>% rename(qx = `Mortality Rate`) %>% select(Age, qx)

interventions <- interventions_raw %>% select(Intervention_Name, Lower_Bound_Cost, Upper_Bound_Cost, `Lower_Bound%`, `Upper_Bound%`) %>%
  rename(Upper_Bound_Mortality_Change = `Upper_Bound%`,
         Lower_Bound_Mortality_Change = `Lower_Bound%`)

inflation_raw<- read_excel("srcsc-2024-lumaria-economic-data.xlsx", sheet = "Inflation_Hist&Proj", range = "A1:D165")

oneyear_raw<- read_excel("srcsc-2024-lumaria-economic-data.xlsx", sheet = "OneYR_Hist&Proj", range = "A1:D165")
#Expand inforce data out by years of activity
data_2 <- data %>%
  mutate(Time = ifelse(!is.na(Year.of.Death), Year.of.Death,
                       ifelse(!is.na(Year.of.Lapse), Year.of.Lapse, 2023)) - Issue.year + 1)# %>% mutate(Key = 1:n())
data_2 <- data_2 %>%
  mutate(Key = row_number()) %>%
  slice(rep(1:n(), (Time))) %>%
  group_by(Key) %>%
  mutate(Tenure = seq(1:n()))

data_2 <- data_2 %>% mutate(Year = Tenure + Issue.year - 1) %>%
  mutate(Year.of.Death = ifelse(is.na(Year.of.Death), 0, Year.of.Death),
         Year.of.Lapse = ifelse(is.na(Year.of.Lapse), 0, Year.of.Lapse)) %>%
  mutate(Lapse.Indicator = ifelse(Year.of.Lapse == Year, 1, 0),
         Death.indicator = ifelse(Year.of.Death == Year, 1, 0))

data_2 <- data_2 %>% mutate(completion_year = ifelse(is.na(completion_year), 0, completion_year))

data_2 <- data_2 %>%
  mutate(Mid_Lapse = ifelse(Lapse.Indicator ==1, ifelse(Tenure != 20, 1, 0), 0),
         Age = Issue.age + Year - Issue.year)
gc()
# 
# fwrite(data_2, "Inforce_Long_Format.csv")
# data_2 <-fread("Inforce_Long_Format.csv")

#Find Historical Lapse Rates from Inforce Data
Lapse_Death_Data <- data_2 %>%
  mutate(Age_Group = ifelse(Age < 40, "<40",
                            ifelse(Age >= 40 & Age < 45, "40-44",
                                   ifelse(Age>=45 & Age < 50, "45-49",
                                          ifelse(Age>=50 & Age < 55, "50-54",
                                                 ifelse(Age>=55 & Age < 60, "55-59",
                                                        ifelse(Age>=60 & Age < 65, "60-64",
                                                               ifelse(Age>=65 & Age < 70, "65-69",
                                                                      ifelse(Age>=70 & Age < 75, "70-74",
                                                                             ifelse(Age>=75 & Age < 80, "75-79",
                                                                                    ifelse(Age>=80 & Age < 85, "80-84",
                                                                                           ifelse(Age>=85 & Age < 90, "85-89", "90+"
                                                                                           ))))))))))))
gc()
Lapse_Death_Data <- Lapse_Death_Data %>%
  group_by(Policy.type, Sex, Age_Group) %>%
  summarise(Policies = n(), Total_Mid_Lapse = sum(Mid_Lapse),
            Mid_Lapse_Rate = sum(Mid_Lapse)/n(),
            Total_Death = sum(Death.indicator),
            Historical_Mortality_Rate = sum(Death.indicator)/n())

#fwrite(Lapse_Death_Data, "Lapse_Death_Data.csv")

#Lapse_Death_Data <- fread("Lapse_Death_Data.csv")

####################
####Assumptions#####
####################

#Program plus initial cost(right now included in ongoing cost calcs)
Upfront_Cost <- 500-200#500-200
#Renewal of product and program
Ongoing_Cost <- 155 + 200#155 + 200
#Assumed Cost of processing claims
Claim_Cost <- 5000


####################
####EDA#####
####################
## Plot Mortality Rate by Policy Type
WholeLife <- data_2 %>%
  filter(Policy.type == "SPWL") %>%
  group_by(Age) %>%
  summarise(Policies = n(), Total_Mid_Lapse = sum(Mid_Lapse),
            Mid_Lapse_Rate = sum(Mid_Lapse)/n(),
            Total_Death = sum(Death.indicator),
            WholeLife = sum(Death.indicator)/n()) %>%
  select(Age, WholeLife)


Term20 <- data_2 %>%
  filter(Policy.type == "T20") %>%
  group_by(Age) %>%
  summarise(Policies = n(), Total_Mid_Lapse = sum(Mid_Lapse),
            Mid_Lapse_Rate = sum(Mid_Lapse)/n(),
            Total_Death = sum(Death.indicator),
            Term20 = sum(Death.indicator)/n()) %>%
  select(Age, Term20)
PolicyTypeLifeTable <- Term20 %>% left_join(WholeLife)
PolicyTypeLifeTable <- PolicyTypeLifeTable %>% left_join(mortality_data)

plot <- ggplot() +
  geom_line(data = PolicyTypeLifeTable, aes(x = Age, y = WholeLife, color = "Whole Life")) +
  geom_line(data = PolicyTypeLifeTable, aes(x = Age, y = Term20, color = "20 Year")) +
  geom_line(data = mortality_data, aes(x = Age, y = qx), color = "black", linetype = "dashed") +  # Adding mortality_data
  xlim(35, 74) +
  ylim(0, 0.03)+
  ggtitle("Mortality by Policy type") +
  ylab("Mortality Rate")+
  theme_minimal()
ggsave("Mortality by Policy Type.png", plot)


## Plot Mortality Rate by Smoking Status
Smoker <- data_2 %>%
  filter(Smoker.Status == "S") %>%
  group_by(Age) %>%
  summarise(Policies = n(), Total_Mid_Lapse = sum(Mid_Lapse),
            Mid_Lapse_Rate = sum(Mid_Lapse)/n(),
            Total_Death = sum(Death.indicator),
            Smoker = sum(Death.indicator)/n()) %>%
  select(Age, Smoker)


NonSmoker <- data_2 %>%
  filter(Smoker.Status == "NS") %>%
  group_by(Age) %>%
  summarise(Policies = n(), Total_Mid_Lapse = sum(Mid_Lapse),
            Mid_Lapse_Rate = sum(Mid_Lapse)/n(),
            Total_Death = sum(Death.indicator),
            NonSmoker = sum(Death.indicator)/n()) %>%
  select(Age, NonSmoker)
PolicyTypeLifeTable <- Smoker %>% left_join(NonSmoker)
PolicyTypeLifeTable <- PolicyTypeLifeTable %>% left_join(mortality_data)

fwrite(PolicyTypeLifeTable, "Smoking_Mortality.csv")

plot <- ggplot() +
  geom_line(data = PolicyTypeLifeTable, aes(x = Age, y = Smoker, color = "Smoker")) +
  geom_line(data = PolicyTypeLifeTable, aes(x = Age, y = NonSmoker, color = "NonSmoker")) +
  geom_line(data = mortality_data, aes(x = Age, y = qx), color = "black", linetype = "dashed") +  # Adding mortality_data
  xlim(26, 86) +
  ggtitle("Mortality by Smoker type") +
  ylab("Mortality Rate")+
  theme_minimal()

ggsave("Mortality by Smoker Type.png", plot)

## Plot Mortality Rate by Gender
Male <- data_2 %>%
  filter(Sex == "M") %>%
  group_by(Age) %>%
  summarise(Policies = n(), Total_Mid_Lapse = sum(Mid_Lapse),
            Mid_Lapse_Rate = sum(Mid_Lapse)/n(),
            Total_Death = sum(Death.indicator),
            Male = sum(Death.indicator)/n()) %>%
  select(Age, Male)


Female <- data_2 %>%
  filter(Sex == "F") %>%
  group_by(Age) %>%
  summarise(Policies = n(), Total_Mid_Lapse = sum(Mid_Lapse),
            Mid_Lapse_Rate = sum(Mid_Lapse)/n(),
            Total_Death = sum(Death.indicator),
            Female = sum(Death.indicator)/n()) %>%
  select(Age, Female)
PolicyTypeLifeTable <- Female %>% left_join(Male)
PolicyTypeLifeTable <- PolicyTypeLifeTable %>% left_join(mortality_data)

plot <- ggplot() +
  geom_line(data = PolicyTypeLifeTable, aes(x = Age, y = Male, color = "Male")) +
  geom_line(data = PolicyTypeLifeTable, aes(x = Age, y = Female, color = "Female")) +
  geom_line(data = mortality_data, aes(x = Age, y = qx), color = "black", linetype = "dashed") +  # Adding mortality_data
  xlim(26, 86) +
  ylim(0, 0.125)+
  ggtitle("Mortality by Gender") +
  ylab("Mortality Rate")+
  theme_minimal()

ggsave("Mortality by Gender.png", plot)

# Plot Mortality Rate by Underwriting Class

Underwriting_Class <- data_2 %>%
  group_by(Age, Underwriting.Class) %>%
  summarise(Mortality = sum(Death.indicator)/n())

Underwriting_Export <- Underwriting_Class %>% 
  pivot_wider(names_from = Underwriting.Class, values_from = Mortality) %>% 
  left_join(mortality_data)

fwrite(Underwriting_Export, "Underwriting Mortality.csv")

plot <- ggplot(Underwriting_Class, aes(x = Age, y = Mortality, color = Underwriting.Class)) +
  geom_line() +
  geom_line(data = mortality_data, aes(x = Age, y = qx), color = "black", linetype = "dashed") +  # Adding mortality_data
  ggtitle("Mortality by Underwriting Class") +
  xlab("Age") +
  ylab("Mortality") +
  xlim(26, 86) +
  ylim(0, 0.25) +
  scale_color_manual(values = c("very low risk" = "blue", "low risk" = "green", "moderate risk" = "orange", "high risk" = "red", "national life table" = "black")) +
  theme_minimal()
ggsave("Mortality by Underwriting Class.png", plot)


## Plot Mortality Rate by FACE AMOUNT
Face <- data_2 %>%
  mutate(Face.amount = as.factor(Face.amount)) %>%
  group_by(Age, Face.amount) %>%
  summarise(Mortality = sum(Death.indicator)/n())

plot <- ggplot(Face, aes(x = Age, y = Mortality, color = Face.amount)) +
  geom_line() +
  geom_line(data = mortality_data, aes(x = Age, y = qx), color = "black", linetype = "dashed") +  # Adding mortality_data
  ggtitle("Mortality by Face Amount") +
  xlab("Age") +
  ylab("Mortality") +
  xlim(26, 86) +
  ylim(0, 0.25)+
  theme_minimal()
ggsave("Mortality by Face Amount.png", plot)

## Plot Mortality Rate by Region
Region <- data_2 %>%
  mutate(Region = as.factor(Region)) %>%
  group_by(Age, Region) %>%
  summarise(Mortality = sum(Death.indicator)/n())

plot <- ggplot(Region, aes(x = Age, y = Mortality, color = Region)) +
  geom_line() +
  geom_line(data = mortality_data, aes(x = Age, y = qx), color = "black", linetype = "dashed") +  # Adding mortality_data
  ggtitle("Mortality by Region") +
  xlab("Age") +
  ylab("Mortality") +
  xlim(26, 86) +
  ylim(0, 0.25)+
  theme_minimal()
ggsave("Mortality by Region.png", plot)

## Plot Mortality Rate by Urban.vs.Rural
Urban.vs.Rural <- data_2 %>%
  mutate(Urban.vs.Rural = as.factor(Urban.vs.Rural)) %>%
  group_by(Age, Urban.vs.Rural) %>%
  summarise(Mortality = sum(Death.indicator)/n())

plot <- ggplot(Urban.vs.Rural, aes(x = Age, y = Mortality, color = Urban.vs.Rural)) +
  geom_line() +
  geom_line(data = mortality_data, aes(x = Age, y = qx), color = "black", linetype = "dashed") +  # Adding mortality_data
  ggtitle("Mortality by Urban.vs.Rural") +
  xlab("Age") +
  ylab("Mortality") +
  xlim(26, 86) +
  ylim(0, 0.125)+
  theme_minimal()
ggsave("Mortality by Urban.vs.Rural.png", plot)


## Plot Overall Mortality Rate
Overall <- data_2 %>%
  mutate(Urban.vs.Rural = as.factor(Urban.vs.Rural)) %>%
  group_by(Age) %>%
  summarise(Mortality = sum(Death.indicator)/n(),
            Exposure = n()) %>% left_join(mortality_data)

Overall <- Overall %>%
  rename(`In-Force` = Mortality, Government = qx)

# Create the plot
ggplot(Overall, aes(x = Age)) +
  geom_line(aes(y = `In-Force`, color = "In-Force")) +  # Line graph for In-Force
  geom_line(aes(y = Government, color = "National")) +  # Line graph for Government
  geom_bar(aes(y = Exposure), stat = "identity", fill = "lightblue", alpha = 0.5) +  # Bar graph for Exposure
  ggtitle("Mortality, In-Force, and Government by Age") +
  xlab("Age") +
  ylab("Value") +
  scale_color_manual(values = c("In-Force" = "blue", "Government" = "red"), guide = "none") +  # Color adjustment
  scale_y_continuous(sec.axis = sec_axis(~., name = "Exposure", breaks = NULL)) +  # Secondary axis for Exposure
  theme_minimal()

#fwrite(Overall, "Gov.vs.Inforce.csv")



plot <- ggplot(Overall) +
  geom_line(aes(x = Age, y = `In-Force`), color = "red") +
  geom_line(aes(x = Age, y = Government), color = "black", linetype = "dashed") +  # Adding mortality_data
  ggtitle("Total In-Force Mortality") +
  xlab("Age") +
  ylab("Mortality") +
  xlim(26, 86) +
  ylim(0, 0.125)+
  theme_minimal()
ggsave("Mortality.png", plot)

rm(data_2)

####################
####Cox Regression Modelling#####
####################

# Clean the data, and convert to survival data object
CRData <- data %>% 
  mutate(Time = ifelse(!is.na(Year.of.Death), Year.of.Death,
                       ifelse(!is.na(Year.of.Lapse), Year.of.Lapse, 2023)) - Issue.year) %>%
  mutate(Delta = Death.indicator) %>%
  select(Policy.type, Issue.age, Sex, Face.amount, Smoker.Status, 
         Underwriting.Class, Urban.vs.Rural, Region, Distribution.Channel, Time, Delta)
cens.CRData <- Surv(CRData$Time, CRData$Delta)

##Fit a model on the factors below:
CRModel <- coxph(cens.CRData ~ as.factor(Sex) +
                   as.factor(Smoker.Status) + as.factor(Underwriting.Class), method = "breslow",
                 data = CRData)
summary(CRModel)

####################
####Technical Pricing Calculations#####
####################
# Create Modelpoints of every possible combination
Modelpoints <- expand.grid(Policy.type = unique((CRData$Policy.type)),
                           Issue.age = c(18:120),
                           Sex = unique((CRData$Sex)),
                           Face.amount = unique((CRData$Face.amount)),
                           Smoker.Status = unique((CRData$Smoker.Status)),
                           Underwriting.Class = unique((CRData$Underwriting.Class)),
                           time = c(1:120))

Modelpoints <- Modelpoints %>% 
  mutate(Age = Issue.age + time) %>%
  filter(Age <= 120) %>%
  filter(Policy.type == "SPWL" | (Policy.type == "T20" & time <= 20)) %>%
  filter(Issue.age <= 75)

#Score the modelpoints and left join the baseline hazard function
Modelpoints$explm <- predict(CRModel, Modelpoints, type = "risk")

Modelpoints <- Modelpoints %>% 
  mutate(Age_Group = ifelse(Issue.age < 30, "<30",
                            ifelse(Issue.age>=30 & Issue.age < 35, "30-34", 
                                   ifelse(Issue.age>=35 & Issue.age < 40, "35-40", 
                                          ifelse(Issue.age>=40 & Issue.age < 45, "40-44", 
                                                 ifelse(Issue.age>=45 & Issue.age < 50, "45-49",
                                                        ifelse(Issue.age>=50 & Issue.age < 55, "50-54",
                                                               ifelse(Issue.age>=55 & Issue.age < 60, "55-59",
                                                                      ifelse(Issue.age>=60 & Issue.age < 65, "60-64",
                                                                             ifelse(Issue.age>=65 & Issue.age < 70, "65-69",
                                                                                    ifelse(Issue.age>=70 & Issue.age < 75, "70-74",
                                                                                           ifelse(Issue.age>=75 & Issue.age < 80, "75-79",
                                                                                                  ifelse(Issue.age>=80 & Issue.age < 85, "80-84",
                                                                                                         ifelse(Issue.age>=85 & Issue.age < 90, "85-89", "90+"
                                                                                                         )))))))))))))) %>%
  left_join(mortality_data) %>%
  #Use mortality rates from the life table
  left_join(Lapse_Death_Data) %>%
  mutate(Mid_Lapse_Rate = ifelse(is.na(Mid_Lapse_Rate), 0, Mid_Lapse_Rate)) %>%
  mutate(Mortality_Rate = qx*explm) # Add proportional hazards from Cox PH Model

Modelpoints <- Modelpoints %>% select(-c(Policies, Total_Mid_Lapse, Total_Death))

#Model mortality rate assumptions
#Low_Utilisation - Minimum change in mortality from the program
#Central_Utilisation - Average between minimum change and maximum change
#High_Utilisation - Maximum change in mortality from the program
Modelpoints <- Modelpoints %>%
  mutate(Mortality_Rate = Mortality_Rate * (1 - 0.00),
         Low_Utilisation_Mortality_Rate = Mortality_Rate * (1-0.02),
         Central_Utilisation_Mortality_Rate  = Mortality_Rate * (1 - 0.076848),
         High_Utilisation_Mortality_Rate = Mortality_Rate * (1 - 0.06)*(1-0.04)*(1-0.04)) %>%
  #Cap mortality rates at 100%
  mutate(Mortality_Rate = ifelse(Mortality_Rate > 1, 1, Mortality_Rate),
         Low_Utilisation_Mortality_Rate = ifelse(Low_Utilisation_Mortality_Rate >1, 1, Low_Utilisation_Mortality_Rate),
         Central_Utilisation_Mortality_Rate = ifelse(Central_Utilisation_Mortality_Rate >1, 1, Central_Utilisation_Mortality_Rate),
         High_Utilisation_Mortality_Rate = ifelse(High_Utilisation_Mortality_Rate >1, 1, High_Utilisation_Mortality_Rate))

#Compute the cumulative survival for each observation
Modelpoints <- Modelpoints %>% mutate(Survival_Rate = (1 - Mortality_Rate) * (1 - Mid_Lapse_Rate),
                                      High_Utilisation_Survival_Rate = (1 - High_Utilisation_Mortality_Rate) * (1 - Mid_Lapse_Rate),
                                      Central_Utilisation_Survival_Rate = (1 - Central_Utilisation_Mortality_Rate) * (1 - Mid_Lapse_Rate),
                                      Low_Utilisation_Survival_Rate = (1 - Low_Utilisation_Mortality_Rate) * (1 - Mid_Lapse_Rate) ) %>%
  group_by(Policy.type, Issue.age, Sex, Face.amount, Smoker.Status, Underwriting.Class) %>%
  arrange(Policy.type, Issue.age, Sex, Face.amount, Smoker.Status, Underwriting.Class, time)%>%
  mutate(Cumulative_Survival_Rate = cumprod(Survival_Rate),
         Low_Utilisation_Cumulative_Survival_Rate = cumprod(Low_Utilisation_Survival_Rate),
         Central_Utilisation_Cumulative_Survival_Rate = cumprod(Central_Utilisation_Survival_Rate),
         High_Utilisation_Cumulative_Survival_Rate = cumprod(High_Utilisation_Survival_Rate)) %>%
  mutate(Survival_Previous = lag(Cumulative_Survival_Rate, default = 1),
         Low_Utilisation_Survival_Previous = lag(Low_Utilisation_Cumulative_Survival_Rate, default = 1),
         Central_Utilisation_Survival_Previous = lag(Central_Utilisation_Cumulative_Survival_Rate, default = 1),
         High_Utilisation_Survival_Previous = lag(High_Utilisation_Cumulative_Survival_Rate , default = 1))

#I calculate the expected value of each observation and discount, also calculate best case and worst case intervention scenario
#Lower_Bound - discounted using the minimum inflation assumptions from the model
#Upper_Bound - discounted using the minimum inflation assumptions from the model
#indexation rates are the maximum of the Inflation and 5%p.a.

inflation_raw <- inflation_raw %>% mutate(indexation_rate_regular = ifelse(Inflation >= 0.05, 0.05, Inflation),
                                          indexation_rate_upper = ifelse(Upper_Bound_Inf >= 0.05, 0.05, Upper_Bound_Inf))
# We inflate the costs and payouts and then discount back to present value
Modelpoints <- Modelpoints %>%  
  mutate(Year = 2023+time) %>%
  left_join(inflation_raw) %>%
  rename(discount_factor = Inflation) %>%
  mutate(discount_factor = ifelse(is.na(discount_factor), mean(inflation_raw$Inflation), discount_factor),
         Lower_Bound_Inf  = ifelse(is.na(Lower_Bound_Inf), mean(inflation_raw$Lower_Bound_Inf), Lower_Bound_Inf),
         Upper_Bound_Inf  = ifelse(is.na(Upper_Bound_Inf), mean(inflation_raw$Upper_Bound_Inf), Upper_Bound_Inf)) %>%
  rename(lower_bound_discount_factor = Lower_Bound_Inf,
         upper_bound_discount_factor = Upper_Bound_Inf) %>%
  mutate(Monthly_Expected_Loss = Survival_Previous * (1-Mid_Lapse_Rate) * Mortality_Rate * ((Face.amount) * (1 + indexation_rate_regular)^(time) + (Claim_Cost) * (1 + discount_factor)^(time)),
         Monthly_Expected_Loss_Lower_Bound = Survival_Previous * (1-Mid_Lapse_Rate) * Mortality_Rate * (Face.amount + Claim_Cost) * (1 + lower_bound_discount_factor)^(time),
         Monthly_Expected_Loss_Upper_Bound = Survival_Previous * (1-Mid_Lapse_Rate) * Mortality_Rate * ((Face.amount) * (1 + indexation_rate_upper)^(time) + (Claim_Cost) * (1 + upper_bound_discount_factor)^(time)),
         
         Low_Utilisation_Monthly_Expected_Loss = Low_Utilisation_Survival_Previous * (1-Mid_Lapse_Rate) * Low_Utilisation_Mortality_Rate * ((Face.amount) * (1 + indexation_rate_regular)^(time) + (Claim_Cost) * (1 + discount_factor)^(time)),
         Low_Utilisation_Monthly_Expected_Loss_Lower_Bound = Low_Utilisation_Survival_Previous * (1-Mid_Lapse_Rate) * Low_Utilisation_Mortality_Rate * (Face.amount + Claim_Cost)*(1 + lower_bound_discount_factor)^(time),
         Low_Utilisation_Monthly_Expected_Loss_Upper_Bound = Low_Utilisation_Survival_Previous * (1-Mid_Lapse_Rate) * Low_Utilisation_Mortality_Rate * ((Face.amount) * (1 + indexation_rate_upper)^(time) + (Claim_Cost) * (1 + upper_bound_discount_factor)^(time)),
         
         Central_Utilisation_Monthly_Expected_Loss = Central_Utilisation_Survival_Previous * (1-Mid_Lapse_Rate) * Central_Utilisation_Mortality_Rate * ((Face.amount) * (1 + indexation_rate_regular)^(time) + (Claim_Cost) * (1 + discount_factor)^(time)),
         Central_Utilisation_Monthly_Expected_Loss_Lower_Bound = Central_Utilisation_Survival_Previous * (1-Mid_Lapse_Rate) * Central_Utilisation_Mortality_Rate * (Face.amount + Claim_Cost)*(1 + lower_bound_discount_factor)^(time),
         Central_Utilisation_Monthly_Expected_Loss_Upper_Bound = Central_Utilisation_Survival_Previous * (1-Mid_Lapse_Rate) * Central_Utilisation_Mortality_Rate * ((Face.amount) * (1 + indexation_rate_upper)^(time) + (Claim_Cost) * (1 + upper_bound_discount_factor)^(time)),
         
         High_Utilisation_Monthly_Expected_Loss = High_Utilisation_Survival_Previous * (1-Mid_Lapse_Rate) * High_Utilisation_Mortality_Rate * ((Face.amount) * (1 + indexation_rate_regular)^(time) + (Claim_Cost) * (1 + discount_factor)^(time)),
         High_Utilisation_Monthly_Expected_Loss_Lower_Bound = High_Utilisation_Survival_Previous * (1-Mid_Lapse_Rate) * High_Utilisation_Mortality_Rate * (Face.amount + Claim_Cost)*(1 + lower_bound_discount_factor)^(time),
         High_Utilisation_Monthly_Expected_Loss_Upper_Bound = High_Utilisation_Survival_Previous * (1-Mid_Lapse_Rate) * High_Utilisation_Mortality_Rate * ((Face.amount) * (1 + indexation_rate_upper)^(time) + (Claim_Cost) * (1 + upper_bound_discount_factor)^(time)),
         
         Monthly_Expected_Cost_No_Intervention = Survival_Previous*200*(1+discount_factor)^(time),
         
         Monthly_Expected_Cost = Survival_Previous*Ongoing_Cost*(1+discount_factor)^(time),
         Monthly_Expected_Cost_Lower_Bound = Survival_Previous*Ongoing_Cost*(1+lower_bound_discount_factor)^(time),
         Monthly_Expected_Cost_Upper_Bound = Survival_Previous*Ongoing_Cost*(1+upper_bound_discount_factor)^(time),
         
         Low_Utilisation_Monthly_Expected_Cost = Low_Utilisation_Survival_Previous*Ongoing_Cost*(1+discount_factor)^(time),
         Low_Utilisation_Monthly_Expected_Cost_Lower_Bound = Low_Utilisation_Survival_Previous*Ongoing_Cost*(1+lower_bound_discount_factor)^(time),
         Low_Utilisation_Monthly_Expected_Cost_Upper_Bound = Low_Utilisation_Survival_Previous*Ongoing_Cost*(1+upper_bound_discount_factor)^(time),
         
         Central_Utilisation_Monthly_Expected_Cost = Central_Utilisation_Survival_Previous*Ongoing_Cost*(1+discount_factor)^(time),
         Central_Utilisation_Monthly_Expected_Cost_Lower_Bound = Central_Utilisation_Survival_Previous*Ongoing_Cost*(1+lower_bound_discount_factor)^(time),
         Central_Utilisation_Monthly_Expected_Cost_Upper_Bound = Central_Utilisation_Survival_Previous*Ongoing_Cost*(1+upper_bound_discount_factor)^(time),
         
         High_Utilisation_Monthly_Expected_Cost = High_Utilisation_Survival_Previous*Ongoing_Cost*(1+discount_factor)^(time),
         High_Utilisation_Monthly_Expected_Cost_Lower_Bound = High_Utilisation_Survival_Previous*Ongoing_Cost*(1+lower_bound_discount_factor)^(time),
         High_Utilisation_Monthly_Expected_Cost_Upper_Bound = High_Utilisation_Survival_Previous*Ongoing_Cost*(1+upper_bound_discount_factor)^(time)
         
  ) %>%
  mutate(Discounted_Expected_Value = (Monthly_Expected_Cost + Monthly_Expected_Loss) * (1+discount_factor) ^ (-time),
         Discounted_Expected_Value_Lower_Bound = (Monthly_Expected_Cost_Lower_Bound + Monthly_Expected_Loss_Lower_Bound) * (1+lower_bound_discount_factor) ^ (-time),
         Discounted_Expected_Value_Upper_Bound = (Monthly_Expected_Cost_Upper_Bound + Monthly_Expected_Loss_Upper_Bound) * (1+upper_bound_discount_factor) ^ (-time),
         
         Discounted_Expected_Value_No_Intervention = (Monthly_Expected_Cost_No_Intervention + Monthly_Expected_Loss) * (1+discount_factor) ^ (-time),
         
         Low_Utilisation_Discounted_Expected_Value = (Low_Utilisation_Monthly_Expected_Cost + Low_Utilisation_Monthly_Expected_Loss) * (1+discount_factor) ^ (-time),
         Low_Utilisation_Discounted_Expected_Value_Lower_Bound = (Low_Utilisation_Monthly_Expected_Cost_Lower_Bound + Low_Utilisation_Monthly_Expected_Loss_Lower_Bound) * (1+lower_bound_discount_factor) ^ (-time),
         Low_Utilisation_Discounted_Expected_Value_Upper_Bound = (Low_Utilisation_Monthly_Expected_Cost_Upper_Bound + Low_Utilisation_Monthly_Expected_Loss_Upper_Bound) * (1+upper_bound_discount_factor) ^ (-time),
         
         Central_Utilisation_Discounted_Expected_Value = (Central_Utilisation_Monthly_Expected_Cost + Central_Utilisation_Monthly_Expected_Loss) * (1+discount_factor) ^ (-time),
         Central_Utilisation_Discounted_Expected_Value_Lower_Bound = (Central_Utilisation_Monthly_Expected_Cost_Lower_Bound + Central_Utilisation_Monthly_Expected_Loss_Lower_Bound) * (1+lower_bound_discount_factor) ^ (-time),
         Central_Utilisation_Discounted_Expected_Value_Upper_Bound = (Central_Utilisation_Monthly_Expected_Cost_Upper_Bound + Central_Utilisation_Monthly_Expected_Loss_Upper_Bound) * (1+upper_bound_discount_factor) ^ (-time),
         
         High_Utilisation_Discounted_Expected_Value = (High_Utilisation_Monthly_Expected_Cost + High_Utilisation_Monthly_Expected_Loss) * (1+discount_factor) ^ (-time),
         High_Utilisation_Discounted_Expected_Value_Lower_Bound = (High_Utilisation_Monthly_Expected_Cost_Lower_Bound + High_Utilisation_Monthly_Expected_Loss_Lower_Bound) * (1+lower_bound_discount_factor) ^ (-time),
         High_Utilisation_Discounted_Expected_Value_Upper_Bound = (High_Utilisation_Monthly_Expected_Cost_Upper_Bound + High_Utilisation_Monthly_Expected_Loss_Upper_Bound) * (1+upper_bound_discount_factor) ^ (-time),
         
         Discounted_Survival = lag(Cumulative_Survival_Rate, default = 1)* (1+discount_factor) ^ (-(time)),
         Discounted_Survival_Lower_Bound = lag(Cumulative_Survival_Rate, default = 1)* (1+lower_bound_discount_factor) ^ (-(time)),
         Discounted_Survival_Upper_Bound = lag(Cumulative_Survival_Rate, default = 1)* (1+upper_bound_discount_factor) ^ (-(time)),
         
         Central_Discounted_Survival = lag(Central_Utilisation_Cumulative_Survival_Rate, default = 1)* (1+discount_factor) ^ (-(time)),
         Central_Discounted_Survival_Lower_Bound = lag(Central_Utilisation_Cumulative_Survival_Rate, default = 1)* (1+lower_bound_discount_factor) ^ (-(time)),
         Central_Discounted_Survival_Upper_Bound = lag(Central_Utilisation_Cumulative_Survival_Rate, default = 1)* (1+upper_bound_discount_factor) ^ (-(time)),
         
         High_Discounted_Survival = lag(High_Utilisation_Cumulative_Survival_Rate, default = 1)* (1+discount_factor) ^ (-(time)),
         High_Discounted_Survival_Lower_Bound = lag(High_Utilisation_Cumulative_Survival_Rate, default = 1)* (1+lower_bound_discount_factor) ^ (-(time)),
         High_Discounted_Survival_Upper_Bound = lag(High_Utilisation_Cumulative_Survival_Rate, default = 1)* (1+upper_bound_discount_factor) ^ (-(time)),
         
         Low_Discounted_Survival = lag(Low_Utilisation_Cumulative_Survival_Rate, default = 1)* (1+discount_factor) ^ (-(time)),
         Low_Discounted_Survival_Lower_Bound = lag(Low_Utilisation_Cumulative_Survival_Rate, default = 1)* (1+lower_bound_discount_factor) ^ (-(time)),
         Low_Discounted_Survival_Upper_Bound = lag(Low_Utilisation_Cumulative_Survival_Rate, default = 1)* (1+upper_bound_discount_factor) ^ (-(time))) %>%
  left_join(oneyear_raw) %>% rename(Spot_1yr = `1yr_Risk_Free_Annual_Spot_Rate`)
#Interest is modeled as difference between expected premiums and expected costs multiplied by the modelled 1-year spot rate.
Modelpoints <- Modelpoints %>% mutate(
  ##  #Un-Hash when computing Stress Testing Results_2
  # Interest_1 = cumsum(Survival_Previous)*Spot_1yr * (1+discount_factor) ^ (-(time)),
  # Interest_1_Lower_Bound = cumsum(Survival_Previous)*Upper_Bound_1yr * (1+lower_bound_discount_factor) ^ (-(time)),
  # Interest_1_Upper_Bound = cumsum(Survival_Previous)*Lower_Bound_1yr * (1+upper_bound_discount_factor) ^ (-(time)),
  # 
  # Low_Utilisation_Interest_1 = cumsum(Low_Utilisation_Survival_Previous)*Spot_1yr * (1+discount_factor) ^ (-(time)),
  # Low_Utilisation_Interest_1_Lower_Bound = cumsum(Low_Utilisation_Survival_Previous)*Upper_Bound_1yr * (1+lower_bound_discount_factor) ^ (-(time)),
  # Low_Utilisation_Interest_1_Upper_Bound = cumsum(Low_Utilisation_Survival_Previous)*Lower_Bound_1yr * (1+upper_bound_discount_factor) ^ (-(time)),
  # 
  # Central_Utilisation_Interest_1 = cumsum(Central_Utilisation_Survival_Previous)*Spot_1yr * (1+discount_factor) ^ (-(time)),
  # Central_Utilisation_Interest_1_Lower_Bound = cumsum(Central_Utilisation_Survival_Previous)*Upper_Bound_1yr * (1+lower_bound_discount_factor) ^ (-(time)),
  # Central_Utilisation_Interest_1_Upper_Bound = cumsum(Central_Utilisation_Survival_Previous)*Lower_Bound_1yr * (1+upper_bound_discount_factor) ^ (-(time)),
  # 
  # High_Utilisation_Interest_1 = cumsum(High_Utilisation_Survival_Previous)*Spot_1yr * (1+discount_factor) ^ (-(time)),
  # High_Utilisation_Interest_1_Lower_Bound = cumsum(High_Utilisation_Survival_Previous)*Upper_Bound_1yr * (1+lower_bound_discount_factor) ^ (-(time)),
  # High_Utilisation_Interest_1_Upper_Bound = cumsum(High_Utilisation_Survival_Previous)*Lower_Bound_1yr * (1+upper_bound_discount_factor) ^ (-(time)),
  # 
  # Interest_2_No_Intervention = cumsum(Monthly_Expected_Cost_No_Intervention)*Spot_1yr * (1+discount_factor) ^ (-(time)),
  # 
  # Interest_2 = cumsum(Monthly_Expected_Cost)*Spot_1yr * (1+discount_factor) ^ (-(time)),
  # Interest_2_Lower_Bound = cumsum(Monthly_Expected_Cost_Lower_Bound)*Upper_Bound_1yr * (1+lower_bound_discount_factor) ^ (-(time)),
  # Interest_2_Upper_Bound = cumsum(Monthly_Expected_Cost_Upper_Bound)*Lower_Bound_1yr * (1+upper_bound_discount_factor) ^ (-(time)),
  # 
  # Low_Utilisation_Interest_2 = cumsum(Low_Utilisation_Monthly_Expected_Cost)*Spot_1yr * (1+discount_factor) ^ (-(time)),
  # Low_Utilisation_Interest_2_Lower_Bound = cumsum(Low_Utilisation_Monthly_Expected_Cost_Lower_Bound)*Upper_Bound_1yr * (1+lower_bound_discount_factor) ^ (-(time)),
  # Low_Utilisation_Interest_2_Upper_Bound = cumsum(Low_Utilisation_Monthly_Expected_Cost_Upper_Bound)*Lower_Bound_1yr * (1+upper_bound_discount_factor) ^ (-(time)),
  # 
  # Central_Utilisation_Interest_2 = cumsum(Central_Utilisation_Monthly_Expected_Cost)*Spot_1yr * (1+discount_factor) ^ (-(time)),
  # Central_Utilisation_Interest_2_Lower_Bound = cumsum(Central_Utilisation_Monthly_Expected_Cost_Lower_Bound)*Upper_Bound_1yr * (1+lower_bound_discount_factor) ^ (-(time)),
  # Central_Utilisation_Interest_2_Upper_Bound = cumsum(Central_Utilisation_Monthly_Expected_Cost_Upper_Bound)*Lower_Bound_1yr * (1+upper_bound_discount_factor) ^ (-(time)),
  # 
  # High_Utilisation_Interest_2 = cumsum(High_Utilisation_Monthly_Expected_Cost)*Spot_1yr * (1+discount_factor) ^ (-(time)),
  # High_Utilisation_Interest_2_Lower_Bound = cumsum(High_Utilisation_Monthly_Expected_Cost_Lower_Bound)*Upper_Bound_1yr * (1+lower_bound_discount_factor) ^ (-(time)),
  # High_Utilisation_Interest_2_Upper_Bound = cumsum(High_Utilisation_Monthly_Expected_Cost_Upper_Bound)*Lower_Bound_1yr * (1+upper_bound_discount_factor) ^ (-(time))
  
  #  ##Hash when computing Stress Testing Results_2
  Interest_2_No_Intervention = cumsum(Monthly_Expected_Cost_No_Intervention)*Spot_1yr * (1+discount_factor) ^ (-(time)),
  
  Interest_1 = cumsum(Survival_Previous)*Spot_1yr * (1+discount_factor) ^ (-(time)),
  Interest_1_Lower_Bound = cumsum(Survival_Previous)*Lower_Bound_1yr * (1+lower_bound_discount_factor) ^ (-(time)),
  Interest_1_Upper_Bound = cumsum(Survival_Previous)*Upper_Bound_1yr * (1+upper_bound_discount_factor) ^ (-(time)),
  
  Low_Utilisation_Interest_1 = cumsum(Low_Utilisation_Survival_Previous)*Spot_1yr * (1+discount_factor) ^ (-(time)),
  Low_Utilisation_Interest_1_Lower_Bound = cumsum(Low_Utilisation_Survival_Previous)*Lower_Bound_1yr * (1+lower_bound_discount_factor) ^ (-(time)),
  Low_Utilisation_Interest_1_Upper_Bound = cumsum(Low_Utilisation_Survival_Previous)*Upper_Bound_1yr * (1+upper_bound_discount_factor) ^ (-(time)),
  
  Central_Utilisation_Interest_1 = cumsum(Central_Utilisation_Survival_Previous)*Spot_1yr * (1+discount_factor) ^ (-(time)),
  Central_Utilisation_Interest_1_Lower_Bound = cumsum(Central_Utilisation_Survival_Previous)*Lower_Bound_1yr * (1+lower_bound_discount_factor) ^ (-(time)),
  Central_Utilisation_Interest_1_Upper_Bound = cumsum(Central_Utilisation_Survival_Previous)*Upper_Bound_1yr * (1+upper_bound_discount_factor) ^ (-(time)),
  
  High_Utilisation_Interest_1 = cumsum(High_Utilisation_Survival_Previous)*Spot_1yr * (1+discount_factor) ^ (-(time)),
  High_Utilisation_Interest_1_Lower_Bound = cumsum(High_Utilisation_Survival_Previous)*Lower_Bound_1yr * (1+lower_bound_discount_factor) ^ (-(time)),
  High_Utilisation_Interest_1_Upper_Bound = cumsum(High_Utilisation_Survival_Previous)*Upper_Bound_1yr * (1+upper_bound_discount_factor) ^ (-(time)),
  
  Interest_2 = cumsum(Monthly_Expected_Cost)*Spot_1yr * (1+discount_factor) ^ (-(time)),
  Interest_2_Lower_Bound = cumsum(Monthly_Expected_Cost_Lower_Bound)*Lower_Bound_1yr * (1+lower_bound_discount_factor) ^ (-(time)),
  Interest_2_Upper_Bound = cumsum(Monthly_Expected_Cost_Upper_Bound)*Upper_Bound_1yr * (1+upper_bound_discount_factor) ^ (-(time)),
  
  Low_Utilisation_Interest_2 = cumsum(Low_Utilisation_Monthly_Expected_Cost)*Spot_1yr * (1+discount_factor) ^ (-(time)),
  Low_Utilisation_Interest_2_Lower_Bound = cumsum(Low_Utilisation_Monthly_Expected_Cost_Lower_Bound)*Lower_Bound_1yr * (1+lower_bound_discount_factor) ^ (-(time)),
  Low_Utilisation_Interest_2_Upper_Bound = cumsum(Low_Utilisation_Monthly_Expected_Cost_Upper_Bound)*Upper_Bound_1yr * (1+upper_bound_discount_factor) ^ (-(time)),
  
  Central_Utilisation_Interest_2 = cumsum(Central_Utilisation_Monthly_Expected_Cost)*Spot_1yr * (1+discount_factor) ^ (-(time)),
  Central_Utilisation_Interest_2_Lower_Bound = cumsum(Central_Utilisation_Monthly_Expected_Cost_Lower_Bound)*Lower_Bound_1yr * (1+lower_bound_discount_factor) ^ (-(time)),
  Central_Utilisation_Interest_2_Upper_Bound = cumsum(Central_Utilisation_Monthly_Expected_Cost_Upper_Bound)*Upper_Bound_1yr * (1+upper_bound_discount_factor) ^ (-(time)),
  
  High_Utilisation_Interest_2 = cumsum(High_Utilisation_Monthly_Expected_Cost)*Spot_1yr * (1+discount_factor) ^ (-(time)),
  High_Utilisation_Interest_2_Lower_Bound = cumsum(High_Utilisation_Monthly_Expected_Cost_Lower_Bound)*Lower_Bound_1yr * (1+lower_bound_discount_factor) ^ (-(time)),
  High_Utilisation_Interest_2_Upper_Bound = cumsum(High_Utilisation_Monthly_Expected_Cost_Upper_Bound)*Upper_Bound_1yr * (1+upper_bound_discount_factor) ^ (-(time))
)

#I group together and sum the discounted values to obtain the net present values for each component
Summary_Modelpoints <- Modelpoints %>% group_by(Policy.type, Issue.age, Sex, Face.amount, Smoker.Status, Underwriting.Class, Age_Group) %>% 
  summarise(Expected_Loss_No_Intervention = sum(Discounted_Expected_Value),
            
            Expected_Loss = sum(Discounted_Expected_Value),
            Expected_Loss_Lower_Bound = sum(Discounted_Expected_Value_Lower_Bound),
            Expected_Loss_Upper_Bound = sum(Discounted_Expected_Value_Upper_Bound),
            
            Low_Utilisation_Expected_Loss = sum(Low_Utilisation_Discounted_Expected_Value),
            Low_Utilisation_Expected_Loss_Lower_Bound = sum(Low_Utilisation_Discounted_Expected_Value_Lower_Bound),
            Low_Utilisation_Expected_Loss_Upper_Bound = sum(Low_Utilisation_Discounted_Expected_Value_Upper_Bound),
            
            Central_Utilisation_Expected_Loss = sum(Central_Utilisation_Discounted_Expected_Value),
            Central_Utilisation_Expected_Loss_Lower_Bound = sum(Central_Utilisation_Discounted_Expected_Value_Lower_Bound),
            Central_Utilisation_Expected_Loss_Upper_Bound = sum(Central_Utilisation_Discounted_Expected_Value_Upper_Bound),
            
            High_Utilisation_Expected_Loss = sum(High_Utilisation_Discounted_Expected_Value),
            High_Utilisation_Expected_Loss_Lower_Bound = sum(High_Utilisation_Discounted_Expected_Value_Lower_Bound),
            High_Utilisation_Expected_Loss_Upper_Bound = sum(High_Utilisation_Discounted_Expected_Value_Upper_Bound),
            
            Agg_Discount_Survival = sum(Discounted_Survival),
            Agg_Discount_Survival_Lower_Bound = sum(Discounted_Survival_Lower_Bound),
            Agg_Discount_Survival_Upper_Bound = sum(Discounted_Survival_Upper_Bound),
            
            Central_Agg_Survival = sum(Central_Discounted_Survival),
            Central_Agg_Survival_Lower_Bound = sum(Central_Discounted_Survival_Lower_Bound),
            Central_Agg_Survival_Upper_Bound = sum(Central_Discounted_Survival_Upper_Bound),
            
            High_Agg_Survival = sum(High_Discounted_Survival),
            High_Agg_Survival_Lower_Bound = sum(High_Discounted_Survival_Lower_Bound),
            High_Agg_Survival_Upper_Bound = sum(High_Discounted_Survival_Upper_Bound),
            
            Low_Agg_Survival = sum(Low_Discounted_Survival),
            Low_Agg_Survival_Lower_Bound = sum(Low_Discounted_Survival_Lower_Bound),
            Low_Agg_Survival_Upper_Bound = sum(Low_Discounted_Survival_Upper_Bound),
            
            Lifetime = sum(Cumulative_Survival_Rate),
            Low_Lifetime = sum(Low_Utilisation_Cumulative_Survival_Rate),
            Central_Lifetime = sum(Central_Utilisation_Cumulative_Survival_Rate),
            High_Lifetime = sum(High_Utilisation_Cumulative_Survival_Rate),
            
            Interest_1 = sum(Interest_1),
            Interest_1_Lower_Bound = sum(Interest_1_Lower_Bound),
            Interest_1_Upper_Bound = sum(Interest_1_Upper_Bound),
            
            Low_Utilisation_Interest_1 = sum(Low_Utilisation_Interest_1),
            Low_Utilisation_Interest_1_Lower_Bound = sum(Low_Utilisation_Interest_1_Lower_Bound),
            Low_Utilisation_Interest_1_Upper_Bound = sum(Low_Utilisation_Interest_1_Upper_Bound),
            
            Central_Utilisation_Interest_1 = sum(Central_Utilisation_Interest_1),
            Central_Utilisation_Interest_1_Lower_Bound = sum(Central_Utilisation_Interest_1_Lower_Bound),
            Central_Utilisation_Interest_1_Upper_Bound = sum(Central_Utilisation_Interest_1_Upper_Bound),
            
            High_Utilisation_Interest_1 = sum(High_Utilisation_Interest_1),
            High_Utilisation_Interest_1_Lower_Bound = sum(High_Utilisation_Interest_1_Lower_Bound),
            High_Utilisation_Interest_1_Upper_Bound = sum(High_Utilisation_Interest_1_Upper_Bound),
            
            Interest_2_No_Intervention = sum(Interest_2_No_Intervention),
            
            Interest_2 = sum(Interest_2),
            Interest_2_Lower_Bound = sum(Interest_2_Lower_Bound),
            Interest_2_Upper_Bound = sum(Interest_2_Upper_Bound),
            
            Low_Utilisation_Interest_2 = sum(Low_Utilisation_Interest_2),
            Low_Utilisation_Interest_2_Lower_Bound = sum(Low_Utilisation_Interest_2_Lower_Bound),
            Low_Utilisation_Interest_2_Upper_Bound = sum(Low_Utilisation_Interest_2_Upper_Bound),
            
            Central_Utilisation_Interest_2 = sum(Central_Utilisation_Interest_2),
            Central_Utilisation_Interest_2_Lower_Bound = sum(Central_Utilisation_Interest_2_Lower_Bound),
            Central_Utilisation_Interest_2_Upper_Bound = sum(Central_Utilisation_Interest_2_Upper_Bound), 
            
            High_Utilisation_Interest_2 = sum(High_Utilisation_Interest_2),
            High_Utilisation_Interest_2_Lower_Bound = sum(High_Utilisation_Interest_2_Lower_Bound),
            High_Utilisation_Interest_2_Upper_Bound = sum(High_Utilisation_Interest_2_Upper_Bound) 
  )

#I add to each modelpoint the current policyholders belonging to that modelpoint
Policies <- data %>%
  filter(is.na(Year.of.Death) & is.na(Year.of.Lapse)) %>%
  group_by(Policy.type, Issue.age, Sex, Face.amount, Smoker.Status, Underwriting.Class) %>%
  summarise(Policies = n())

Summary_Modelpoints <- Summary_Modelpoints %>% left_join(Policies)

Summary_Modelpoints <- Summary_Modelpoints %>% mutate(Policies = ifelse(is.na(Policies), 1, Policies))

#Using the Policyholders, I group by age and then use a weighted average to 
#aggregate  by age group, policy type, face amount, smoking status and 
#underwriting class.
Final_Premium_Info <- Summary_Modelpoints %>% group_by(Policy.type, Face.amount, Smoker.Status, Age_Group, Underwriting.Class) %>%
  summarise(Expected_Loss_No_Intervention = weighted.mean(Expected_Loss_No_Intervention, Policies),
            Expected_Loss = weighted.mean(Expected_Loss, Policies),
            Expected_Loss_Lower_Bound = weighted.mean(Expected_Loss_Lower_Bound, Policies),
            Expected_Loss_Upper_Bound = weighted.mean(Expected_Loss_Upper_Bound, Policies),
            
            Low_Utilisation_Expected_Loss = weighted.mean(Low_Utilisation_Expected_Loss, Policies), 
            Low_Utilisation_Expected_Loss_Lower_Bound = weighted.mean(Low_Utilisation_Expected_Loss_Lower_Bound, Policies), 
            Low_Utilisation_Expected_Loss_Upper_Bound = weighted.mean(Low_Utilisation_Expected_Loss_Upper_Bound, Policies), 
            
            Central_Utilisation_Expected_Loss = weighted.mean(Central_Utilisation_Expected_Loss, Policies), 
            Central_Utilisation_Expected_Loss_Lower_Bound = weighted.mean(Central_Utilisation_Expected_Loss_Lower_Bound, Policies), 
            Central_Utilisation_Expected_Loss_Upper_Bound = weighted.mean(Central_Utilisation_Expected_Loss_Upper_Bound, Policies),
            
            High_Utilisation_Expected_Loss = weighted.mean(High_Utilisation_Expected_Loss, Policies), 
            High_Utilisation_Expected_Loss_Lower_Bound = weighted.mean(High_Utilisation_Expected_Loss_Lower_Bound, Policies), 
            High_Utilisation_Expected_Loss_Upper_Bound = weighted.mean(High_Utilisation_Expected_Loss_Upper_Bound, Policies), 
            
            Lifetime = weighted.mean(Lifetime, Policies), 
            Low_Lifetime = weighted.mean(Low_Lifetime, Policies), 
            Central_Lifetime = weighted.mean(Central_Lifetime, Policies),
            High_Lifetime = weighted.mean(High_Lifetime, Policies),
            
            Agg_Discount_Survival = weighted.mean(Agg_Discount_Survival, Policies), 
            Agg_Discount_Survival_Lower_Bound = weighted.mean(Agg_Discount_Survival_Lower_Bound, Policies), 
            Agg_Discount_Survival_Upper_Bound = weighted.mean(Agg_Discount_Survival_Upper_Bound, Policies), 
            
            Central_Agg_Survival = weighted.mean(Central_Agg_Survival, Policies), 
            Central_Agg_Survival_Lower_Bound = weighted.mean(Central_Agg_Survival_Lower_Bound, Policies), 
            Central_Agg_Survival_Upper_Bound = weighted.mean(Central_Agg_Survival_Upper_Bound, Policies), 
            
            High_Agg_Survival = weighted.mean(High_Agg_Survival, Policies), 
            High_Agg_Survival_Lower_Bound = weighted.mean(High_Agg_Survival_Lower_Bound, Policies), 
            High_Agg_Survival_Upper_Bound = weighted.mean(High_Agg_Survival_Upper_Bound, Policies), 
            
            Low_Agg_Survival = weighted.mean(Low_Agg_Survival, Policies),
            Low_Agg_Survival_Lower_Bound = weighted.mean(Low_Agg_Survival_Lower_Bound, Policies),
            Low_Agg_Survival_Upper_Bound = weighted.mean(Low_Agg_Survival_Upper_Bound, Policies),
            
            Interest_1 = weighted.mean(Interest_1, Policies),
            Interest_1_Lower_Bound = weighted.mean(Interest_1_Lower_Bound, Policies),
            Interest_1_Upper_Bound = weighted.mean(Interest_1_Upper_Bound, Policies),
            
            Low_Utilisation_Interest_1 = weighted.mean(Low_Utilisation_Interest_1, Policies),
            Low_Utilisation_Interest_1_Lower_Bound = weighted.mean(Low_Utilisation_Interest_1_Lower_Bound, Policies),
            Low_Utilisation_Interest_1_Upper_Bound = weighted.mean(Low_Utilisation_Interest_1_Upper_Bound, Policies),
            
            Central_Utilisation_Interest_1 = weighted.mean(Central_Utilisation_Interest_1, Policies),
            Central_Utilisation_Interest_1_Lower_Bound = weighted.mean(Central_Utilisation_Interest_1_Lower_Bound, Policies),
            Central_Utilisation_Interest_1_Upper_Bound = weighted.mean(Central_Utilisation_Interest_1_Upper_Bound, Policies),
            
            High_Utilisation_Interest_1 = weighted.mean(High_Utilisation_Interest_1, Policies),
            High_Utilisation_Interest_1_Lower_Bound = weighted.mean(High_Utilisation_Interest_1_Lower_Bound, Policies),
            High_Utilisation_Interest_1_Upper_Bound = weighted.mean(High_Utilisation_Interest_1_Upper_Bound, Policies),
            
            Interest_2_No_Intervention = weighted.mean(Interest_2_No_Intervention, Policies),
            
            Interest_2 = weighted.mean(Interest_2, Policies),
            Interest_2_Lower_Bound = weighted.mean(Interest_2_Lower_Bound, Policies),
            Interest_2_Upper_Bound = weighted.mean(Interest_2_Upper_Bound, Policies),
            
            Low_Utilisation_Interest_2 = weighted.mean(Low_Utilisation_Interest_2, Policies),
            Low_Utilisation_Interest_2_Lower_Bound = weighted.mean(Low_Utilisation_Interest_2_Lower_Bound, Policies),
            Low_Utilisation_Interest_2_Upper_Bound = weighted.mean(Low_Utilisation_Interest_2_Upper_Bound, Policies),
            
            Central_Utilisation_Interest_2 = weighted.mean(Central_Utilisation_Interest_2, Policies),
            Central_Utilisation_Interest_2_Lower_Bound = weighted.mean(Central_Utilisation_Interest_2_Lower_Bound, Policies),
            Central_Utilisation_Interest_2_Upper_Bound = weighted.mean(Central_Utilisation_Interest_2_Upper_Bound, Policies),
            
            High_Utilisation_Interest_2 = weighted.mean(High_Utilisation_Interest_2, Policies),
            High_Utilisation_Interest_2_Lower_Bound = weighted.mean(High_Utilisation_Interest_2_Lower_Bound, Policies),
            High_Utilisation_Interest_2_Upper_Bound = weighted.mean(High_Utilisation_Interest_2_Upper_Bound, Policies),
            
            Policies = sum(Policies)
            
  )
#Premiums are calculated using the Zero-NPV principle, no profit margin is added
Final_Premium_Info <- Final_Premium_Info %>% 
  mutate(Premium_No_Intervention = (Upfront_Cost + Expected_Loss_No_Intervention+Interest_2_No_Intervention)/(Agg_Discount_Survival+Interest_1),
         
         Premium = (Upfront_Cost + Expected_Loss+Interest_2)/(Agg_Discount_Survival+Interest_1),
         Premium_Lower_Bound = (Upfront_Cost + Expected_Loss_Lower_Bound + Interest_2_Lower_Bound)/(Agg_Discount_Survival_Lower_Bound + Interest_1_Lower_Bound),
         Premium_Upper_Bound = (Upfront_Cost + Expected_Loss_Upper_Bound + Interest_2_Upper_Bound)/(Agg_Discount_Survival_Upper_Bound + Interest_1_Upper_Bound),
         
         Central_Utilisation_Premium = (Upfront_Cost + Central_Utilisation_Expected_Loss + Central_Utilisation_Interest_2)/(Central_Agg_Survival + Central_Utilisation_Interest_1),
         Central_Utilisation_Premium_Lower_Bound = (Upfront_Cost + Central_Utilisation_Expected_Loss_Lower_Bound + Central_Utilisation_Interest_2_Lower_Bound)/(Central_Agg_Survival_Lower_Bound + Central_Utilisation_Interest_1_Lower_Bound),
         Central_Utilisation_Premium_Upper_Bound = (Upfront_Cost + Central_Utilisation_Expected_Loss_Upper_Bound + Central_Utilisation_Interest_2_Upper_Bound)/(Central_Agg_Survival_Upper_Bound + Central_Utilisation_Interest_1_Upper_Bound), 
         
         High_Utilisation_Premium = (Upfront_Cost + High_Utilisation_Expected_Loss + High_Utilisation_Interest_2)/(High_Agg_Survival + High_Utilisation_Interest_1),
         High_Utilisation_Premium_Lower_Bound = (Upfront_Cost + High_Utilisation_Expected_Loss_Lower_Bound + High_Utilisation_Interest_2_Lower_Bound)/(High_Agg_Survival_Lower_Bound + High_Utilisation_Interest_1_Lower_Bound),
         High_Utilisation_Premium_Upper_Bound = (Upfront_Cost + High_Utilisation_Expected_Loss_Upper_Bound + High_Utilisation_Interest_2_Upper_Bound)/(High_Agg_Survival_Upper_Bound + High_Utilisation_Interest_1_Upper_Bound),
         
         Low_Utilisation_Premium = (Upfront_Cost + Low_Utilisation_Expected_Loss + Low_Utilisation_Interest_2)/(Low_Agg_Survival + Low_Utilisation_Interest_1),
         Low_Utilisation_Premium_Lower_Bound = (Upfront_Cost + Low_Utilisation_Expected_Loss_Lower_Bound + Low_Utilisation_Interest_2_Lower_Bound)/(Low_Agg_Survival_Lower_Bound + Low_Utilisation_Interest_1_Lower_Bound),
         Low_Utilisation_Premium_Upper_Bound = (Upfront_Cost + Low_Utilisation_Expected_Loss_Upper_Bound + Low_Utilisation_Interest_2_Upper_Bound)/(Low_Agg_Survival_Upper_Bound + Low_Utilisation_Interest_1_Upper_Bound)
  ) %>%
  mutate(Lifetime_Premium_No_Intervention = Premium_No_Intervention * Agg_Discount_Survival,
         
         Lifetime_Premium = Premium*Agg_Discount_Survival,
         Lifetime_Premium_Lower_Bound = Premium_Lower_Bound*Agg_Discount_Survival_Lower_Bound,
         Lifetime_Premium_Upper_Bound = Premium_Upper_Bound*Agg_Discount_Survival_Upper_Bound,
         
         Central_Utilisation_Lifetime_Premium = Central_Utilisation_Premium * Central_Agg_Survival,
         Central_Utilisation_Lifetime_Premium_Lower_Bound = Central_Utilisation_Premium_Lower_Bound * Central_Agg_Survival_Lower_Bound,
         Central_Utilisation_Lifetime_Premium_Upper_Bound = Central_Utilisation_Premium_Upper_Bound * Central_Agg_Survival_Upper_Bound,
         
         High_Utilisation_Lifetime_Premium = High_Utilisation_Premium * High_Agg_Survival,
         High_Utilisation_Lifetime_Premium_Lower_Bound = High_Utilisation_Premium_Lower_Bound * High_Agg_Survival_Lower_Bound,
         High_Utilisation_Lifetime_Premium_Upper_Bound = High_Utilisation_Premium_Upper_Bound * High_Agg_Survival_Upper_Bound,
         
         Low_Utilisation_Lifetime_Premium = Low_Utilisation_Premium * Low_Agg_Survival,
         Low_Utilisation_Lifetime_Premium_Lower_Bound = Low_Utilisation_Premium_Lower_Bound * Low_Agg_Survival_Lower_Bound,
         Low_Utilisation_Lifetime_Premium_Upper_Bound = Low_Utilisation_Premium_Upper_Bound * Low_Agg_Survival_Upper_Bound
  )

####################
####Export Data#####
####################

In_Use_Premium <- Final_Premium_Info %>% ungroup() %>%
  select(Policy.type, Face.amount,
         Smoker.Status, Age_Group, 
         Underwriting.Class, Central_Utilisation_Premium) %>%
  mutate(Point = paste(Policy.type, Face.amount, Smoker.Status, Underwriting.Class, sep = "_")) %>%
  select(Age_Group, Point, Central_Utilisation_Premium) %>% 
  pivot_wider(names_from = Point, values_from = Central_Utilisation_Premium)
#fwrite(In_Use_Premium, "Final Premium.csv")

Lifetime_Info_Agg <- Summary_Modelpoints %>% filter(Policy.type == "SPWL") %>%
  group_by(Age_Group, Smoker.Status) %>%
  summarise(Lifetime = weighted.mean(Lifetime, Policies),
            Low_Lifetime = weighted.mean(Low_Lifetime, Policies),
            Central_Lifetime = weighted.mean(Central_Lifetime, Policies),
            High_Lifetime = weighted.mean(High_Lifetime, Policies),
            Policies = sum(Policies))
#fwrite(Lifetime_Info_Agg, "Lifetime_Info_Agg.csv")

Policies_2 <- data %>%
  filter(is.na(Year.of.Death) & is.na(Year.of.Lapse)) %>%
  mutate(Age_Group = ifelse(Issue.age < 30, "<30",
                            ifelse(Issue.age>=30 & Issue.age < 35, "30-34", 
                                   ifelse(Issue.age>=35 & Issue.age < 40, "35-40", 
                                          ifelse(Issue.age>=40 & Issue.age < 45, "40-44", 
                                                 ifelse(Issue.age>=45 & Issue.age < 50, "45-49",
                                                        ifelse(Issue.age>=50 & Issue.age < 55, "50-54",
                                                               ifelse(Issue.age>=55 & Issue.age < 60, "55-59",
                                                                      ifelse(Issue.age>=60 & Issue.age < 65, "60-64",
                                                                             ifelse(Issue.age>=65 & Issue.age < 70, "65-69",
                                                                                    ifelse(Issue.age>=70 & Issue.age < 75, "70-74",
                                                                                           ifelse(Issue.age>=75 & Issue.age < 80, "75-79",
                                                                                                  ifelse(Issue.age>=80 & Issue.age < 85, "80-84",
                                                                                                         ifelse(Issue.age>=85 & Issue.age < 90, "85-89", "90+"
                                                                                                         )))))))))))))) %>%
  group_by(Age_Group,Face.amount, Smoker.Status, Policy.type, Underwriting.Class) %>%
  summarise(Policies = n())

Lifetime_Premium <- Final_Premium_Info %>% left_join(Policies) %>%
  select(Lifetime_Premium_No_Intervention, Lifetime_Premium, Low_Utilisation_Lifetime_Premium, Central_Utilisation_Lifetime_Premium, 
         High_Utilisation_Lifetime_Premium, Policies) %>% ungroup() %>%
  summarise(Lifetime_Premium_No_Intervention = weighted.mean(Lifetime_Premium_No_Intervention, Policies),
            Lifetime_Premium = weighted.mean(Lifetime_Premium, Policies),
            Low_Utilisation_Lifetime_Premium = weighted.mean(Low_Utilisation_Lifetime_Premium, Policies),
            Central_Utilisation_Lifetime_Premium = weighted.mean(Central_Utilisation_Lifetime_Premium, Policies),
            High_Utilisation_Lifetime_Premium = weighted.mean(High_Utilisation_Lifetime_Premium, Policies))


Average_Premium <- Final_Premium_Info %>% 
  left_join(Policies_2) %>%
  ungroup() %>%
  select(Premium, Premium_No_Intervention, Central_Utilisation_Premium, High_Utilisation_Premium, Low_Utilisation_Premium, Policies) %>%
  summarise(
    Premium_No_Intervention= weighted.mean(Premium_No_Intervention, Policies),
    Premium = weighted.mean(Premium, Policies),
    High_Utilisation_Premium = weighted.mean(High_Utilisation_Premium, Policies),
    Low_Utilisation_Premium = weighted.mean(Low_Utilisation_Premium, Policies),
    Central_Utilisation_Premium = weighted.mean(Central_Utilisation_Premium, Policies)
  ) %>% mutate(Increase_Premium = Premium - Premium_No_Intervention,
               Increase_HU_Premium = High_Utilisation_Premium - Premium_No_Intervention,
               Increase__LU_Premium = Low_Utilisation_Premium - Premium_No_Intervention,
               Increase_CE_Premium = Central_Utilisation_Premium - Premium_No_Intervention)
fwrite(Average_Premium, "Average Premium.csv")

Lifetime_Info <- Summary_Modelpoints %>% #group_by(Age_Group) %>%
  # summarise(Lifetime = weighted.mean(Lifetime, Policies),
  #           Low_Lifetime = weighted.mean(Low_Lifetime, Policies),
  #           High_Lifetime = weighted.mean(High_Lifetime, Policies),
  #           Policies = sum(Policies)) %>%
  select(Policy.type, Issue.age, Sex, Face.amount, Smoker.Status, Underwriting.Class, Age_Group, Lifetime, Low_Lifetime, Central_Lifetime, High_Lifetime)
#
fwrite(Lifetime_Info, "Lifetime_Info.csv")

fwrite(Final_Premium_Info, "Premium Results.csv")

FINAL <- Final_Premium_Info

####################
####Stress Testing#####
####################

FINAL <- fread("Premium Results.csv")
FINAL <- FINAL %>% select(Policy.type, Face.amount, Smoker.Status, Age_Group, Underwriting.Class, Premium, High_Utilisation_Premium, Low_Utilisation_Premium, Central_Utilisation_Premium)

for (Selected in c("Premium", "High_Utilisation_Premium", "Low_Utilisation_Premium", "Central_Utilisation_Premium")){
  
  #Left Join Pricing onto ungrouped modelpoints (exact ages)
  Profit_Analysis <- Summary_Modelpoints %>% left_join(FINAL)
  Profit_Analysis <- Profit_Analysis %>% rename(Selected = Selected)
  #Calculate Profit
  Profit_Analysis <- Profit_Analysis %>% 
    mutate(Profit = Selected*(Agg_Discount_Survival+Interest_1)-(Upfront_Cost + Expected_Loss+Interest_2) ,
           Profit_Lower_Bound = Selected*(Agg_Discount_Survival_Lower_Bound + Interest_1_Lower_Bound)-(Upfront_Cost + Expected_Loss_Lower_Bound + Interest_2_Lower_Bound),
           Profit_Upper_Bound = Selected*(Agg_Discount_Survival_Upper_Bound + Interest_1_Upper_Bound)-(Upfront_Cost + Expected_Loss_Upper_Bound + Interest_2_Upper_Bound),
           
           Central_Utilisation_Profit = Selected*(Central_Agg_Survival + Central_Utilisation_Interest_1) - (Upfront_Cost + Central_Utilisation_Expected_Loss + Central_Utilisation_Interest_2),
           Central_Utilisation_Profit_Lower_Bound = Selected*(Central_Agg_Survival_Lower_Bound + Central_Utilisation_Interest_1_Lower_Bound) - (Upfront_Cost + Central_Utilisation_Expected_Loss_Lower_Bound + Central_Utilisation_Interest_2_Lower_Bound),
           Central_Utilisation_Profit_Upper_Bound = Selected*(Central_Agg_Survival_Upper_Bound + Central_Utilisation_Interest_1_Upper_Bound) - (Upfront_Cost + Central_Utilisation_Expected_Loss_Upper_Bound + Central_Utilisation_Interest_2_Upper_Bound),
           
           High_Utilisation_Profit = Selected*(High_Agg_Survival + High_Utilisation_Interest_1) - (Upfront_Cost + High_Utilisation_Expected_Loss + High_Utilisation_Interest_2),
           High_Utilisation_Profit_Lower_Bound = Selected*(High_Agg_Survival_Lower_Bound + High_Utilisation_Interest_1_Lower_Bound) - (Upfront_Cost + High_Utilisation_Expected_Loss_Lower_Bound + High_Utilisation_Interest_2_Lower_Bound),
           High_Utilisation_Profit_Upper_Bound = Selected*(High_Agg_Survival_Upper_Bound + High_Utilisation_Interest_1_Upper_Bound) - (Upfront_Cost + High_Utilisation_Expected_Loss_Upper_Bound + High_Utilisation_Interest_2_Upper_Bound),
           
           Low_Utilisation_Profit = Selected*(Low_Agg_Survival + Low_Utilisation_Interest_1) - (Upfront_Cost + Low_Utilisation_Expected_Loss + Low_Utilisation_Interest_2),
           Low_Utilisation_Profit_Lower_Bound = Selected*(Low_Agg_Survival_Lower_Bound + Low_Utilisation_Interest_1_Lower_Bound) - (Upfront_Cost + Low_Utilisation_Expected_Loss_Lower_Bound + Low_Utilisation_Interest_2_Lower_Bound),
           Low_Utilisation_Profit_Upper_Bound = Selected*(Low_Agg_Survival_Upper_Bound + Low_Utilisation_Interest_1_Upper_Bound) - (Upfront_Cost + Low_Utilisation_Expected_Loss_Upper_Bound + Low_Utilisation_Interest_2_Upper_Bound)
    ) 
  
  ###Scoring Old Policyholders
  Historical_Data <- data %>%
    filter(Death.indicator==0 & Lapse.Indicator == 0) %>%
    mutate(Age_Group = ifelse(Issue.age < 30, "<30",
                              ifelse(Issue.age>=30 & Issue.age < 35, "30-34", 
                                     ifelse(Issue.age>=35 & Issue.age < 40, "35-40", 
                                            ifelse(Issue.age>=40 & Issue.age < 45, "40-44", 
                                                   ifelse(Issue.age>=45 & Issue.age < 50, "45-49",
                                                          ifelse(Issue.age>=50 & Issue.age < 55, "50-54",
                                                                 ifelse(Issue.age>=55 & Issue.age < 60, "55-59",
                                                                        ifelse(Issue.age>=60 & Issue.age < 65, "60-64",
                                                                               ifelse(Issue.age>=65 & Issue.age < 70, "65-69",
                                                                                      ifelse(Issue.age>=70 & Issue.age < 75, "70-74",
                                                                                             ifelse(Issue.age>=75 & Issue.age < 80, "75-79",
                                                                                                    ifelse(Issue.age>=80 & Issue.age < 85, "80-84",
                                                                                                           ifelse(Issue.age>=85 & Issue.age < 90, "85-89", "90+"
                                                                                                           )))))))))))))) %>%
    left_join(Profit_Analysis) 
  #Calculate the average profit/loss per policyholder
  Stress_Testing <- Historical_Data %>%
    summarise(Profit = mean(Profit),
              Profit_Lower_Bound = mean(Profit_Lower_Bound),
              Profit_Upper_Bound = mean(Profit_Upper_Bound),
              
              Central_Utilisation_Profit = mean(Central_Utilisation_Profit),
              Central_Utilisation_Profit_Lower_Bound = mean(Central_Utilisation_Profit_Lower_Bound),
              Central_Utilisation_Profit_Upper_Bound = mean(Central_Utilisation_Profit_Upper_Bound),
              
              High_Utilisation_Profit = mean(High_Utilisation_Profit),
              High_Utilisation_Profit_Lower_Bound = mean(High_Utilisation_Profit_Lower_Bound),
              High_Utilisation_Profit_Upper_Bound = mean(High_Utilisation_Profit_Upper_Bound),
              
              Low_Utilisation_Profit = mean(Low_Utilisation_Profit),
              Low_Utilisation_Profit_Lower_Bound = mean(Low_Utilisation_Profit_Lower_Bound),
              Low_Utilisation_Profit_Upper_Bound = mean(Low_Utilisation_Profit_Upper_Bound))
  
  #Stress Testing Results_1.csv- Inflation and discount rates are positively correlated
  #Stress Testing Results_2 - They are negatively correlated
  fwrite(Stress_Testing, paste0(Selected," Stress Testing Results_1.csv"))
}

#############
####Plotting Premium####
##############
FINAL <- fread("Premium Results.csv")

Policies_2 %>%  group_by(Face.amount) %>% summarise(Count = sum(Policies))

Policies %>%  group_by(Face.amount) %>% summarise(Count = sum(Policies))

Premium_Plotting <- FINAL %>% select(Policy.type, Face.amount, Smoker.Status, Age_Group, 
                                     Underwriting.Class, Central_Utilisation_Premium) %>%
  group_by(Policy.type, Face.amount, Age_Group, Smoker.Status) %>%
  summarise(Central_Utilisation_Premium = mean(Central_Utilisation_Premium)) %>%
  filter(Face.amount == "500000") %>%
  group_by(Policy.type, Age_Group, Smoker.Status) %>%
  summarise(Central_Utilisation_Premium = mean(Central_Utilisation_Premium)) %>%
  mutate(Policy_Smoker = paste0(Policy.type, "_", Smoker.Status)) %>%
  ungroup() %>%
  select(Policy_Smoker, Age_Group, Central_Utilisation_Premium) %>%
  rename(Selected_Premium = Central_Utilisation_Premium) %>%
  mutate(Age = ifelse(Age_Group == "<30", 20, sub("-.*", "", Age_Group))) %>%
  mutate(Policy_Smoker = as.factor(Policy_Smoker),
         Age = as.numeric(Age)) %>%
  select(Age, Policy_Smoker, Selected_Premium) %>%
  mutate(Policy_Smoker = ifelse(Policy_Smoker == "SPWL_NS", "Whole Life NonSmoker", 
                                ifelse(Policy_Smoker == "SPWL_S", "Whole Life Smoker",
                                       ifelse(Policy_Smoker == "T20_NS", "20-Year NonSmoker", 
                                              ifelse(Policy_Smoker == "T20_S", "20-Year Smoker", "")))))

Export_Premium <- Premium_Plotting %>% pivot_wider(names_from = Age, values_from = Selected_Premium)

##fwrite(Export_Premium, 'Premium Plotting.csv')

plot <- ggplot(Premium_Plotting, aes(x = Age, y = Selected_Premium, group = Policy_Smoker, color = Policy_Smoker)) +
  geom_step() +
  ggtitle("Premium Stepchart for a Č500,000 Policy") +
  xlab("Issue Age") +
  ylab("Annual Premium (Č)") +
  theme_minimal()+
  xlim(20, 75)+
  labs(color = "Policy Type")
ggsave("Premium Step Chart.png", plot)

####################
####End#####
####################
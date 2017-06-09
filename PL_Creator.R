library(tidyverse)
library(scales)
library(rmarkdown)
library(flexdashboard)

## setup Import of data for project

naChar <- c(""," ",".","na","NA","N/A","n/a","N/a","n/A","Na","nA")
filePath <- "data/"
dateRun <- "_Current.csv"

eligible <- paste0(filePath,"PW_ELIG",dateRun)
facility <- paste0(filePath,"AHSN_facility",dateRun)
members <- paste0(filePath,"All_Members",dateRun)
incentive <- paste0(filePath,"Incentive_Report",dateRun)
insured <- paste0(filePath,"Insured",dateRun)

# Import and clean data


eligible <- read.csv(eligible, skip=0, stringsAsFactors = FALSE, na=naChar) %>%
  mutate(MemberNumber = toupper(ELIG_AHSN), Employee.ID = as.character(E_ID)) %>%
  select(MemberNumber,Employee.ID) %>%
  distinct(MemberNumber, .keep_all=TRUE)


facility <- read.csv(facility, skip=1, stringsAsFactors = FALSE, na=naChar) %>%
  filter(is.na(DEPT)==FALSE & is.na(EE_AHSN)==FALSE) %>%
  mutate(MemberNumber = toupper(EE_AHSN)) %>%
  select(-EE_AHSN) %>%
  distinct(MemberNumber, .keep_all=TRUE)

members <- read.csv(members, skip=0, stringsAsFactors = FALSE, na=naChar) %>%
  mutate(MemberNumber = toupper(Member.Number), IsMember = ifelse(is.na(Last.Login.Date)==FALSE,1,0)) %>%
  select(MemberNumber, IsMember)%>%
  distinct(MemberNumber, .keep_all=TRUE)

incentive <- read.csv(incentive, skip=0, stringsAsFactors = FALSE, na=naChar) %>%
  mutate(MemberNumber = toupper(Member.Number),
         Step1 = ifelse(PHA.Points == 5,1,0),
         Step2 = ifelse(Survey.Points == 5,1,0),
         Step3 = ifelse(Screening.Points == 5,1,0),
         Step4 = ifelse(Total.Points.Earned >= 85,1,0),
         AllSteps = ifelse(Total.Points.Earned == 100,1,0)) %>%
  select(MemberNumber, Step1, Step2, Step3, Step4, AllSteps) %>%
  distinct(MemberNumber, .keep_all=TRUE)

insured <- read.csv(insured, skip=0, stringsAsFactors = FALSE, na=naChar) %>%
  left_join(eligible, by="Employee.ID") %>%
  filter(is.na(Benefit.Plan.Type)==FALSE & is.na(MemberNumber)==FALSE) %>%
  mutate(HasInsurance = 1) %>%
  select(MemberNumber, HasInsurance) %>%
  distinct(MemberNumber, .keep_all=TRUE)

## Merge Cleaned Files

ProcessLevelData <-
  facility %>%
  left_join(incentive, by="MemberNumber") %>%
  left_join(members, by="MemberNumber") %>%
  left_join(insured, by="MemberNumber")

ProcessLevelData[is.na(ProcessLevelData)] <- 0

ProcessLevelData <-
  ProcessLevelData %>%
  select(MemberNumber, LOCATION_DESCRIPTION, PROCESS_LEVEL_NAME, DEPARTMENT_NAME,
         Step1, Step2, Step3, Step4, HasInsurance,IsMember) %>%
  distinct(MemberNumber, .keep_all=TRUE)

pl <- ProcessLevelData

pl["Step5"] <- ifelse((pl$Step1+pl$Step2+pl$Step3+pl$Step4) >= 4,1,0)


masterdata <-
  pl %>%
  group_by(PROCESS_LEVEL_NAME) %>%
  summarise(Employees = n(),
            Step1 = percent(sum(Step1)/n()),
            Step2 = percent(sum(Step2)/n()),
            Step3 = percent(sum(Step3)/n()),
            Step4 = percent(sum(Step4)/n()),
            All4Steps = percent(sum(Step5)/n()),
            HasInsurance = percent(sum(HasInsurance)/n()),
            IsMember = percent(sum(IsMember)/n())) %>%
  arrange(-Employees) %>%
  filter(Employees >= 5)


detaildata <-
  pl %>%
  mutate(Department = as.character(substr(DEPARTMENT_NAME,8,9999)),  ProcessLevel = PROCESS_LEVEL_NAME) %>%
  group_by(ProcessLevel,Department) %>%
  summarise(Employees = n(),
            Step1 = percent(sum(Step1)/n()),
            Step2 = percent(sum(Step2)/n()),
            Step3 = percent(sum(Step3)/n()),
            Step4 = percent(sum(Step4)/n()),
            All4Steps = percent(sum(Step5)/n()),
            HasInsurance = percent(sum(HasInsurance)/n()),
            IsMember = percent(sum(IsMember)/n())) %>%
  arrange(-Employees) %>% filter(Employees >= 5) %>%
  select(ProcessLevel, Department, Step1, Step2, Step3, Step4, All4Steps, HasInsurance, IsMember) %>% 
  arrange(ProcessLevel)


write.csv(detaildata,"pldata.csv",row.names = FALSE,na="")


render("PL_Dashboard.Rmd", flex_dashboard(self_contained=TRUE,theme= "lumen",orientation="rows"))


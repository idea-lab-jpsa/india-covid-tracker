##### SLT Update Dashboard ----

library(dplyr)
library(shiny)
library(plotly)
library(shinythemes)
library(tidyr)
library(DT)

# State Data

state_data <- read.csv("http://api.covid19india.org/csv/latest/states.csv")

state_data$Date <- as.Date(state_data$Date)

# States and Districts Default List

states <- c('Andhra Pradesh','Andaman and Nicobar Islands','Arunachal Pradesh','Assam','Bihar','Chandigarh','Chhattisgarh','Dadra and Nagar Haveli and Daman and Diu','Delhi','Goa','Gujarat','Haryana','Himachal Pradesh','Jammu and Kashmir','Jharkhand','Karnataka','Kerala','Ladakh','Lakshadweep','Madhya Pradesh','Maharashtra','Manipur','Meghalaya','Mizoram','Nagaland','Odisha','Puducherry','Punjab','Rajasthan','Sikkim','Tamil Nadu','Telangana','Tripura','Uttar Pradesh','Uttarakhand','West Bengal')

state <- c('Andhra Pradesh','Andaman and Nicobar Islands','Arunachal Pradesh','Assam','Bihar','Chandigarh','Chhattisgarh','Dadra and Nagar Haveli and Daman and Diu','Delhi','Goa','Gujarat','Haryana','Himachal Pradesh','Jammu and Kashmir','Jharkhand','Karnataka','Kerala','Ladakh','Lakshadweep','Madhya Pradesh','Maharashtra','Manipur','Meghalaya','Mizoram','Nagaland','Odisha','Puducherry','Punjab','Rajasthan','Sikkim','Tamil Nadu','Telangana','Tripura','Uttar Pradesh','Uttarakhand','West Bengal')

ap <- c("Anantapur","Chittoor","East Godavari","Foreign Evacuees","Guntur","Krishna","Kurnool","Other State","Prakasam","S.P.S. Nellore","Srikakulam","Visakhapatnam",
        "Vizianagaram","West Godavari","Y.S.R. Kadapa")

# District Data

districts <- read.csv("https://api.covid19india.org/csv/latest/districts.csv")

districts$Date <- as.Date(districts$Date)

districts %>%
  mutate(state_dist = paste(State, District, sep = "-")) -> districts

dist_one_day <- districts %>% 
  filter(Date == Sys.Date()-1)

# District Population

dist_pop <- read.csv("Data/District Population.csv")

# Read list of districts

selected_districts <- read.csv("Data/Districts Official.csv")


selected_districts %>%
  mutate(state_dist = paste(State, District, sep = "-")) -> selected_districts

# Filter selected districts (For plots)

districts %>%
  filter(state_dist %in% selected_districts$state_dist) -> district_data


# Filter selected districts (For Table)

districts %>%
  filter(state_dist %in% selected_districts$state_dist & Date >= Sys.Date()-36) -> selected_districts_data

selected_districts_data %>%
  group_by(State, District) %>%
  arrange(Date) %>%
  mutate(daily_conf = Confirmed - lag(Confirmed, default = first(Confirmed)),
         daily_rec = Recovered - lag(Recovered, default = first(Recovered)),
         daily_dec = Deceased - lag(Deceased, default = first(Deceased))) -> selected_districts_data

# 7-day average of daily new cases ----

selected_districts_data %>%
  mutate(time_period = case_when(
    Date <= Sys.Date()-1 & Date >= Sys.Date()-7 ~ "this_week",
    Date <= Sys.Date()-8 & Date >= Sys.Date()-14 ~ "last_week",
    Date <= Sys.Date()-15 & Date >= Sys.Date()-21 ~ "two_weeks_back",
    Date <= Sys.Date()-22 & Date >= Sys.Date()-28 ~ "three_weeks_back",
    Date <= Sys.Date()-29 & Date >= Sys.Date()-35 ~ "last_month",
  )) -> selected_districts_data

selected_districts_data %>%
  group_by(State, District, time_period) %>%
  summarise(avg_daily_new_cases = round(mean(daily_conf,na.rm = TRUE))) -> avg_new_cases

avg_new_cases %>%
  mutate(key = paste0(State,District,time_period)) -> avg_new_cases

# 5-day Linear Growth Rate ----

selected_districts_data %>% 
  group_by(State, District) %>%
  arrange(Date) %>%
  mutate(growth = round(((Confirmed - lag(Confirmed,n = 4, default = first(Confirmed)))/lag(Confirmed,n = 4,default = first(Confirmed)))/5*100,2)) -> selected_districts_data

# 5-day Exponential Growth Rate ----

selected_districts_data %>% 
  group_by(State, District) %>%
  arrange(Date) %>%
  mutate(exp_growth = (exp(log(Confirmed/lag(Confirmed,n = 4, default = first(Confirmed)))/4)-1),
         exp_growth_perc = round((exp(log(Confirmed/lag(Confirmed,n = 4, default = first(Confirmed)))/4)-1)*100,2)) -> selected_districts_data

# Doubling Time ----

selected_districts_data %>% 
  mutate(doubling_time = round(log(2)/log(1+exp_growth))) -> selected_districts_data

selected_districts_data %>%
  filter(Date %in% c(Sys.Date()-1,Sys.Date()-8,Sys.Date()-15,Sys.Date()-22,Sys.Date()-29)) %>%
  mutate(time_period = case_when(
    Date == Sys.Date()-1 ~ "this_week",
    Date == Sys.Date()-8 ~ "last_week",
    Date == Sys.Date()-15 ~ "two_weeks_back",
    Date == Sys.Date()-22 ~ "three_weeks_back",
    Date == Sys.Date()-29 ~ "last_month",
  )) -> selected_districts_data

selected_districts_data %>%
  mutate(key = paste0(State,District,time_period)) %>%
  select(State, District, time_period, key, growth, exp_growth, exp_growth_perc, doubling_time)-> selected_districts_data

# Vaccinations ----

vax_data <- read.csv("http://api.covid19india.org/csv/latest/cowin_vaccine_data_districtwise.csv")

yestday <- paste0("X",format(Sys.Date()-2, "%d-%m-%Y"),".3")

yestday <- gsub("-",".",yestday)

lastweek <- paste0("X",format(Sys.Date()-8, "%d-%m-%Y"),".3")

lastweek <- gsub("-",".",lastweek)

lastmonth <- paste0("X",format(Sys.Date()-29, "%d-%m-%Y"),".3")

lastmonth <- gsub("-",".",lastmonth)

vax_data %>%
  select("State_Code","State","District_Key","Cowin.Key",all_of(yestday), all_of(lastweek),all_of(lastmonth)) -> vax_data

colnames(vax_data)[5] <- "this_week_vax"
colnames(vax_data)[6] <- "last_week_vax"
colnames(vax_data)[7] <- "last_month_vax"

vax_data <- vax_data[-c(1),]

vax_data %>% 
  separate(District_Key,c("State_Codes","District"),"_") -> vax_data

# Delhi correction (Combine all districts) ----

vax_data[vax_data$District %in% c("Central Delhi","New Delhi","East Delhi","Delhi","North Delhi","North East Delhi","North West Delhi","Shahdara","South Delhi","South East Delhi","South West Delhi","West Delhi"), "District"] <- "Delhi"

vax_data %>%
  mutate(state_dist = paste(State, District , sep = "-")) -> vax_data

vax_data$this_week_vax <- as.numeric(vax_data$this_week_vax)
vax_data$last_week_vax <- as.numeric(vax_data$last_week_vax)
vax_data$last_month_vax <- as.numeric(vax_data$last_month_vax)

vax_data %>%
  group_by(State, District, state_dist) %>%
  summarise(this_week_vax = sum(this_week_vax),
            last_week_vax = sum(last_week_vax),
            last_month_vax = sum(last_month_vax)) -> vax_data

# Filtering Vax Data for selected districts ----
vax_data %>%
  filter(state_dist %in% selected_districts$state_dist) -> vax_data


# Test Positivity Rate ----

tpr <- read.csv("https://raw.githubusercontent.com/aatishb/indiatestpositivitydata/main/districtdata.csv")

tpr$Date <- as.Date(tpr$Date)

# Fix Delhi

tpr %>%
  filter(State == "DELHI") %>%
  group_by(Date, State) %>%
  summarise(Test.Positivity.Rate = mean(Test.Positivity.Rate)) -> delhi_tpr

delhi_tpr$District <- "DELHI"

delhi_tpr <- delhi_tpr[,c(1,2,4,3)]

rbind(tpr, delhi_tpr) -> tpr


# Fix District Name issues
tpr[tpr$District == "PASHCHIM CHAMPARAN", "District"] <- "WEST CHAMPARAN"
tpr[tpr$District == "PURBI CHAMPARAN", "District"] <- "EAST CHAMPARAN"
tpr[tpr$District == "BANAS KANTHA", "District"] <- "BANASKANTHA"
tpr[tpr$District == "ANUGUL", "District"] <- "ANGUL"
tpr[tpr$District == "JAJAPUR", "District"] <- "JAJPUR"
tpr[tpr$District == "FIROZEPUR", "District"] <- "FEROZEPUR"
tpr[tpr$District == "KANCHIPURAM", "District"] <- "KANCHEEPURAM"
tpr[tpr$District == "KANNIYAKUMARI", "District"] <- "KANYAKUMARI"

tpr %>%
  mutate(state_dist = paste(State, District,sep = "-")) -> tpr

selected_districts$state_dist_caps <- toupper(selected_districts$state_dist)

tpr %>%
  filter(state_dist %in% selected_districts$state_dist_caps) -> tpr_selected

tpr_selected %>%
  filter(Date %in% c(Sys.Date()-1,Sys.Date()-8,Sys.Date()-15,Sys.Date()-22,Sys.Date()-29)) %>%
  mutate(time_period = case_when(
    Date == Sys.Date()-1 ~ "this_week",
    Date == Sys.Date()-8 ~ "last_week",
    Date == Sys.Date()-15 ~ "two_weeks_back",
    Date == Sys.Date()-22 ~ "three_weeks_back",
    Date == Sys.Date()-29 ~ "last_month",
  )) -> tpr_selected


tpr_selected %>%
  mutate(key = paste0(State,District,time_period)) -> tpr_selected

# Merge

avg_new_cases$key <- toupper(avg_new_cases$key)
selected_districts_data$key <- toupper(selected_districts_data$key)
tpr_selected$key<- toupper(tpr_selected$key)

avg_new_cases %>%
  inner_join(selected_districts_data, by = c("key" = "key")) %>%
  left_join(tpr_selected, by = c("key" = "key")) -> merged

# This Week

merged %>%
  filter(time_period.x == "this_week") -> this_week 

this_week$Test.Positivity.Rate <- round(this_week$Test.Positivity.Rate*100,2)

this_week %>%
  mutate(state_dist = paste(State.x, District.x, sep = "-")) -> this_week

this_week %>%
  inner_join(vax_data, by = c("state_dist" = "state_dist")) -> this_week

this_week %>%
  inner_join(dist_pop, by = c("state_dist" = "key")) -> this_week

this_week %>%
  mutate(vax_perc = round(this_week_vax/pop*100,2)) -> this_week
  
this_week %>%
  select(State.x, District.x, avg_daily_new_cases, growth, doubling_time, Test.Positivity.Rate, this_week_vax, vax_perc) -> this_week_all

colnames(this_week_all) <- c("State","District","Daily New Cases (7-day average)", "Growth Rate % (7-day average)", "Doubling Time (in days)", "Test Positivity Rate (%)", "Persons vaccinated with at least one dose", "% of Population vaccinated with at least one dose")

# Last Week

merged %>%
  filter(time_period.x == "last_week") -> last_week 

last_week$Test.Positivity.Rate <- round(last_week$Test.Positivity.Rate*100,2)

last_week %>%
  mutate(state_dist = paste(State.x, District.x, sep = "-")) -> last_week

last_week %>%
  inner_join(vax_data, by = c("state_dist" = "state_dist")) -> last_week

last_week %>%
  inner_join(dist_pop, by = c("state_dist" = "key")) -> last_week

last_week %>%
  mutate(vax_perc = round(last_week_vax/pop*100,2)) -> last_week

last_week %>%
  select(State.x, District.x, avg_daily_new_cases, growth, doubling_time, Test.Positivity.Rate, last_week_vax, vax_perc) -> last_week_all

colnames(last_week_all) <- c("State","District","Daily New Cases (7-day average)", "Growth Rate % (7-day average)", "Doubling Time (in days)", "Test Positivity Rate (%)", "Persons vaccinated with at least one dose", "% of Population vaccinated with at least one dose")

# Last Month

merged %>%
  filter(time_period.x == "last_month") -> last_month 

last_month$Test.Positivity.Rate <- round(last_month$Test.Positivity.Rate*100,2)

last_month %>%
  mutate(state_dist = paste(State.x, District.x, sep = "-")) -> last_month

last_month %>%
  inner_join(vax_data, by = c("state_dist" = "state_dist")) -> last_month

last_month %>%
  inner_join(dist_pop, by = c("state_dist" = "key")) -> last_month

last_month %>%
  mutate(vax_perc = round(last_month_vax/pop*100,2)) -> last_month

last_month %>%
  select(State.x, District.x, avg_daily_new_cases, growth, doubling_time, Test.Positivity.Rate, last_month_vax, vax_perc) -> last_month_all

colnames(last_month_all) <- c("State","District","Daily New Cases (7-day average)", "Growth Rate % (7-day average)", "Doubling Time (in days)", "Test Positivity Rate (%)", "Persons vaccinated with at least one dose", "% of Population vaccinated with at least one dose")


# Triggers ----

# This Week v Last Week

this_week_all %>%
  inner_join(last_week_all, by = c("State","District"), suffix = c(" This Week"," Last Week")) -> tw_lw

tw_lw %>%
  mutate(`Daily Cases Trend (This Week v Last Week)` = case_when(`Daily New Cases (7-day average) This Week` > `Daily New Cases (7-day average) Last Week` ~ "Increasing",
                                       `Daily New Cases (7-day average) This Week` < `Daily New Cases (7-day average) Last Week` ~ "Decreasing",
                                       TRUE ~ "Stable"),
         `Growth Rate Trend (This Week v Last Week)` = case_when(`Growth Rate % (7-day average) This Week` > `Growth Rate % (7-day average) Last Week` ~ "Increasing",
                                       `Growth Rate % (7-day average) This Week` < `Growth Rate % (7-day average) Last Week` ~ "Decreasing",
                                       TRUE ~ "Stable"),
         `Doubling Time Trend (This Week v Last Week)` = case_when(`Doubling Time (in days) This Week` > `Doubling Time (in days) Last Week` ~ "Increasing",
                                         `Doubling Time (in days) This Week` < `Doubling Time (in days) Last Week` ~ "Decreasing",
                                         TRUE ~ "Stable"),
         `Test Positivity Rate Trend (This Week v Last Week)` = case_when(`Test Positivity Rate (%) This Week` > `Test Positivity Rate (%) Last Week` ~ "Increasing",
                               `Test Positivity Rate (%) This Week` < `Test Positivity Rate (%) Last Week` ~ "Decreasing",
                               TRUE ~ "Stable")) -> tw_lw_trend

# tw_lw_trend %>%
#   select("State","District","Daily New Cases (7-day average) This Week", "Daily New Cases (7-day average) Last Week", "Daily Cases Trend",
#          "Growth Rate % (7-day average) This Week", "Growth Rate % (7-day average) Last Week", "Growth Rate Trend",
#          "Doubling Time (in days) This Week", "Doubling Time (in days) Last Week", "Doubling Time Trend", 
#          "Test Positivity Rate (%) This Week", "Test Positivity Rate (%) Last Week", "Test Positivity Rate Trend",
#          "% of Population vaccinated with at least one dose This Week", "% of Population vaccinated with at least one dose Last Week") -> tw_lw_final

# Without Growth rate and doubling time ----

tw_lw_trend %>%
  select("State","District","Daily New Cases (7-day average) This Week", "Daily New Cases (7-day average) Last Week", "Daily Cases Trend (This Week v Last Week)",
         "Test Positivity Rate (%) This Week", "Test Positivity Rate (%) Last Week", "Test Positivity Rate Trend (This Week v Last Week)",
         "% of Population vaccinated with at least one dose This Week", "% of Population vaccinated with at least one dose Last Week") -> tw_lw_final


datatable(tw_lw_final, options = list(
  columnDefs = list(list(className = 'dt-center', targets = 0:4)),
  pageLength = 100
)) %>% formatStyle(
  c('Daily Cases Trend (This Week v Last Week)','Test Positivity Rate Trend (This Week v Last Week)'),
  backgroundColor = styleEqual(c("Increasing", "Decreasing", "Stable"), c('red', 'green', 'yellow'))
) -> tw_lw_coded


# This Week v Last Month ----

this_week_all %>%
  inner_join(last_month_all, by = c("State","District"), suffix = c(" This Week"," Last Month")) -> tw_lm

tw_lm %>%
  mutate(`Daily Cases Trend (This Week v Last Month)` = case_when(`Daily New Cases (7-day average) This Week` > `Daily New Cases (7-day average) Last Month` ~ "Increasing",
                                         `Daily New Cases (7-day average) This Week` < `Daily New Cases (7-day average) Last Month` ~ "Decreasing",
                                         TRUE ~ "Stable"),
         `Growth Rate Trend (This Week v Last Month)` = case_when(`Growth Rate % (7-day average) This Week` > `Growth Rate % (7-day average) Last Month` ~ "Increasing",
                                         `Growth Rate % (7-day average) This Week` < `Growth Rate % (7-day average) Last Month` ~ "Decreasing",
                                         TRUE ~ "Stable"),
         `Doubling Time Trend (This Week v Last Month)` = case_when(`Doubling Time (in days) This Week` > `Doubling Time (in days) Last Month` ~ "Increasing",
                                           `Doubling Time (in days) This Week` < `Doubling Time (in days) Last Month` ~ "Decreasing",
                                           TRUE ~ "Stable"),
         `Test Positivity Rate Trend (This Week v Last Month)` = case_when(`Test Positivity Rate (%) This Week` > `Test Positivity Rate (%) Last Month` ~ "Increasing",
                                                  `Test Positivity Rate (%) This Week` < `Test Positivity Rate (%) Last Month` ~ "Decreasing",
                                                  TRUE ~ "Stable")) -> tw_lm_trend

# tw_lm_trend %>%
#   select("State","District","Daily New Cases (7-day average) This Week", "Daily New Cases (7-day average) Last Month", "Daily Cases Trend",
#          "Growth Rate % (7-day average) This Week", "Growth Rate % (7-day average) Last Month", "Growth Rate Trend",
#          "Doubling Time (in days) This Week", "Doubling Time (in days) Last Month", "Doubling Time Trend", 
#          "Test Positivity Rate (%) This Week", "Test Positivity Rate (%) Last Month", "Test Positivity Rate Trend",
#          "% of Population vaccinated with at least one dose This Week", "% of Population vaccinated with at least one dose Last Month") -> tw_lm_final

# Without Growth rate and Doubling time ----

tw_lm_trend %>%
  select("State","District","Daily New Cases (7-day average) This Week", "Daily New Cases (7-day average) Last Month", "Daily Cases Trend (This Week v Last Month)",
         "Test Positivity Rate (%) This Week", "Test Positivity Rate (%) Last Month", "Test Positivity Rate Trend (This Week v Last Month)",
         "% of Population vaccinated with at least one dose This Week", "% of Population vaccinated with at least one dose Last Month") -> tw_lm_final


datatable(tw_lm_final, options = list(
  columnDefs = list(list(className = 'dt-center', targets = 0:4)),
  pageLength = 100
)) %>% formatStyle(
  c('Daily Cases Trend (This Week v Last Month)','Test Positivity Rate Trend (This Week v Last Month)'),
  backgroundColor = styleEqual(c("Increasing", "Decreasing", "Stable"), c('red', 'green', 'yellow'))
) -> tw_lm_coded



# State Table

# State - Daily New Cases and Test Positivity Rate

state_data %>%
  filter(Date >= Sys.Date()-36) -> state_data_table

state_data_table %>%
  group_by(State) %>%
  arrange(Date) %>%
  mutate(daily_conf = Confirmed - lag(Confirmed, default = first(Confirmed)),
         daily_rec = Recovered - lag(Recovered, default = first(Recovered)),
         daily_dec = Deceased - lag(Deceased, default = first(Deceased)),
         daily_test = Tested - lag(Tested, default = first(Tested)),
         tpr = round((daily_conf/daily_test)*100,2)) -> state_data_table

state_data_table %>%
  mutate(time_period_state = case_when(
    Date <= Sys.Date()-1 & Date >= Sys.Date()-7 ~ "this_week_state",
    Date <= Sys.Date()-8 & Date >= Sys.Date()-14 ~ "last_week_state",
    Date <= Sys.Date()-15 & Date >= Sys.Date()-21 ~ "two_weeks_back_state",
    Date <= Sys.Date()-22 & Date >= Sys.Date()-28 ~ "three_weeks_back_state",
    Date <= Sys.Date()-29 & Date >= Sys.Date()-35 ~ "last_month_state",
  )) -> state_data_table

state_data_table %>%
  group_by(State, time_period_state) %>%
  summarise(avg_daily_new_cases = round(mean(daily_conf,na.rm = TRUE)),
            avg_tpr = round(mean(tpr,na.rm = TRUE))) -> avg_new_cases_state

avg_new_cases_state %>%
  mutate(key_state = paste0(State, time_period_state)) -> avg_new_cases_state

# State - population

state_pop <- read.csv("Data/state_pop.csv")

# State - Vaccinations

state_vax_data <- read.csv("http://api.covid19india.org/csv/latest/cowin_vaccine_data_statewise.csv")

state_vax_data$Updated.On <- as.Date(state_vax_data$Updated.On,format = "%d/%m/%Y")

state_vax_data %>%
  filter(Updated.On %in% c(Sys.Date()-2,Sys.Date()-8, Sys.Date()-29)) %>%
  mutate(time_period_state = case_when(
    Updated.On == Sys.Date()-2 ~ "this_week_state",
    Updated.On == Sys.Date()-8 ~ "last_week_state",
    Updated.On == Sys.Date()-29 ~ "last_month_state")) -> state_vax_data_table

state_vax_data_table %>%
  mutate(key_state = paste0(State,time_period_state)) -> state_vax_data_table

state_vax_data_table %>%
  select(key_state, First.Dose.Administered) -> state_vax_data_table


# State - Merge

avg_new_cases_state$key_state <- toupper(avg_new_cases_state$key_state)
state_vax_data_table$key_state <- toupper(state_vax_data_table$key_state)

state_vax_data_table %>%
  inner_join(avg_new_cases_state, by = c("key_state" = "key_state")) -> merged

# State - This Week

merged %>%
  filter(time_period_state == "this_week_state") -> this_week_state

this_week_state %>%
  inner_join(state_pop, by = c("State"="state")) -> this_week_state

this_week_state %>%
  mutate(vax_perc = round(First.Dose.Administered/pop*100,2)) -> this_week_state

this_week_state %>%
  select(State, pop, avg_daily_new_cases, avg_tpr, First.Dose.Administered, vax_perc) -> this_week_state

colnames(this_week_state) <- c("State","Population","Daily New Cases (7-day average)", "Test Positivity Rate (%)", "Persons vaccinated with at least one dose","% of Population vaccinated with at least one dose")

# State - Last Week

merged %>%
  filter(time_period_state == "last_week_state") -> last_week_state

last_week_state %>%
  inner_join(state_pop, by = c("State"="state")) -> last_week_state

last_week_state %>%
  mutate(vax_perc = round(First.Dose.Administered/pop*100,2)) -> last_week_state

last_week_state %>%
  select(State, avg_daily_new_cases, avg_tpr, First.Dose.Administered, vax_perc) -> last_week_state

colnames(last_week_state) <- c("State","Daily New Cases (7-day average)", "Test Positivity Rate (%)", "Persons vaccinated with at least one dose","% of Population vaccinated with at least one dose")


# State - Last Month

merged %>%
  filter(time_period_state == "last_month_state") -> last_month_state

last_month_state %>%
  inner_join(state_pop, by = c("State"="state")) -> last_month_state

last_month_state %>%
  mutate(vax_perc = round(First.Dose.Administered/pop*100,2)) -> last_month_state

last_month_state %>%
  select(State, avg_daily_new_cases, avg_tpr, First.Dose.Administered, vax_perc) -> last_month_state

colnames(last_month_state) <- c("State","Daily New Cases (7-day average)", "Test Positivity Rate (%)", "Persons vaccinated with at least one dose","% of Population vaccinated with at least one dose")

# State - Triggers ----

# State - This Week v Last Week ----

this_week_state %>%
  inner_join(last_week_state, by = "State", suffix = c(" This Week"," Last Week")) -> tw_lw_state

tw_lw_state %>%
  mutate(`Daily Cases Trend (This Week v Last Week)` = case_when(`Daily New Cases (7-day average) This Week` > `Daily New Cases (7-day average) Last Week` ~ "Increasing",
                                                                 `Daily New Cases (7-day average) This Week` < `Daily New Cases (7-day average) Last Week` ~ "Decreasing",
                                                                 TRUE ~ "Stable"),
         `Test Positivity Rate Trend (This Week v Last Week)` = case_when(`Test Positivity Rate (%) This Week` > `Test Positivity Rate (%) Last Week` ~ "Increasing",
                                                                          `Test Positivity Rate (%) This Week` < `Test Positivity Rate (%) Last Week` ~ "Decreasing",
                                                                          TRUE ~ "Stable")) -> tw_lw_trend_state

tw_lw_trend_state %>%
  select("State","Daily New Cases (7-day average) This Week", "Daily New Cases (7-day average) Last Week", "Daily Cases Trend (This Week v Last Week)",
         "Test Positivity Rate (%) This Week", "Test Positivity Rate (%) Last Week", "Test Positivity Rate Trend (This Week v Last Week)",
         "Persons vaccinated with at least one dose This Week", "% of Population vaccinated with at least one dose This Week",
         "Persons vaccinated with at least one dose Last Week", "% of Population vaccinated with at least one dose Last Week") -> tw_lw_final_state


datatable(tw_lw_final_state, options = list(
  columnDefs = list(list(className = 'dt-center', targets = 0:4)),
  pageLength = 100
)) %>% formatStyle(
  c('Daily Cases Trend (This Week v Last Week)','Test Positivity Rate Trend (This Week v Last Week)'),
  backgroundColor = styleEqual(c("Increasing", "Decreasing", "Stable"), c('red', 'green', 'yellow'))
) -> tw_lw_coded_state


# State - This Week v Last Month ----

this_week_state %>%
  inner_join(last_month_state, by = "State", suffix = c(" This Week"," Last Month")) -> tw_lm_state

tw_lm_state %>%
  mutate(`Daily Cases Trend (This Week v Last Month)` = case_when(`Daily New Cases (7-day average) This Week` > `Daily New Cases (7-day average) Last Month` ~ "Increasing",
                                                                  `Daily New Cases (7-day average) This Week` < `Daily New Cases (7-day average) Last Month` ~ "Decreasing",
                                                                  TRUE ~ "Stable"),
         `Test Positivity Rate Trend (This Week v Last Month)` = case_when(`Test Positivity Rate (%) This Week` > `Test Positivity Rate (%) Last Month` ~ "Increasing",
                                                                           `Test Positivity Rate (%) This Week` < `Test Positivity Rate (%) Last Month` ~ "Decreasing",
                                                                           TRUE ~ "Stable")) -> tw_lm_trend_state

tw_lm_trend_state %>%
  select("State","Daily New Cases (7-day average) This Week", "Daily New Cases (7-day average) Last Month", "Daily Cases Trend (This Week v Last Month)",
         "Test Positivity Rate (%) This Week", "Test Positivity Rate (%) Last Month", "Test Positivity Rate Trend (This Week v Last Month)",
         "Persons vaccinated with at least one dose This Week", "% of Population vaccinated with at least one dose This Week",
         "Persons vaccinated with at least one dose Last Month", "% of Population vaccinated with at least one dose Last Month") -> tw_lm_final_state


datatable(tw_lm_final_state, options = list(
  columnDefs = list(list(className = 'dt-center', targets = 0:4)),
  pageLength = 100
)) %>% formatStyle(
  c('Daily Cases Trend (This Week v Last Month)','Test Positivity Rate Trend (This Week v Last Month)'),
  backgroundColor = styleEqual(c("Increasing", "Decreasing", "Stable"), c('red', 'green', 'yellow'))
) -> tw_lm_coded_state





# UI

ui <- fluidPage (
  
  titlePanel(h1("India COVID-19 Tracker",align = "center"), 
             windowTitle = "India COVID-19 Tracker"),
  
  tabsetPanel(
    tabPanel(
      title = "District COVID-19 Snapshot ",
      tabsetPanel(
        tabPanel(
          title = "This Week v Last Week",
          fluidRow(
            h5(paste0("This Week : 7 days preceding ",Sys.Date()-1)),
            h5(paste0("Last Week : 7 days preceding ",Sys.Date()-8)),
          ),
          fluidRow(
            column(8,
                   p(class = 'text-center', downloadButton('x3', 'Download Data'))
            )
          ),
          fluidRow(
            column(8, DT::dataTableOutput('tw_lw'))
          )
        ),
        tabPanel(
          title = "This Week v Last Month",
          fluidRow(
            h5(paste0("This Week : 7 days preceding ",Sys.Date()-1)),
            h5(paste0("Last Month : 7 days preceding ",Sys.Date()-29)),
          ),
          fluidRow(
            column(8,
                   p(class = 'text-center', downloadButton('x4', 'Download Data'))
            )
          ),
          fluidRow(
            column(8, DT::dataTableOutput('tw_lm'))
          )
        )
      )
    ),
    tabPanel(
      title = "State COVID-19 Snapshot",
      tabsetPanel(
        tabPanel(
          title = "This Week v Last Week",
          fluidRow(
            h3(paste0("This Week : 7 days preceding ",Sys.Date()-1)),
            h3(paste0("Last Week : 7 days preceding ",Sys.Date()-8)),
          ),
          fluidRow(
            column(8,
                   p(class = 'text-center', downloadButton('x5', 'Download Data'))
            )
          ),
          fluidRow(
            column(8, DT::dataTableOutput('tw_lw_state'))
          )
        ),
        tabPanel(
          title = "This Week v Last Month",
          fluidRow(
            h3(paste0("This Week : 7 days preceding ",Sys.Date()-1)),
            h3(paste0("Last Month : 7 days preceding ",Sys.Date()-29)),
          ),
          fluidRow(
            column(8,
                   p(class = 'text-center', downloadButton('x6', 'Download Data'))
            )
          ),
          fluidRow(
            column(8, DT::dataTableOutput('tw_lm_state'))
          )
        )
      )
    ),
    tabPanel(
      title = "Active Cases Trend (States)",
      sliderInput("days_state", "Select number of days (from yesterday)",
                  min = 0, max = 300, value = 60),
      plotlyOutput("state_active_cases", height = 1400, width = 1400)
    ),
    tabPanel(
      title = "Active Cases Trend (Districts)",
      sliderInput("days_dist", "Select number of days (from yesterday)",
                  min = 0, max = 300, value = 60),
      
      plotlyOutput("dist_active_cases", height = 5000, width = 1400)
    ),
    
    tabPanel(
      title = "Compare Active Cases Trend",
      tabsetPanel(
        tabPanel(
          title = "Compare States",
          fluidRow(
            column(width = 3,
                   selectInput("state1c", "Select first state (orange)", state),
            ),
            column(width = 3,
                   selectInput("state2c", "Select second state (blue)", state),
            )
          ),
          fluidRow(
            column(6,
                   plotlyOutput("state_active_cases_compare", height = 500, width = 700)
            ),
            column(6,
                   plotlyOutput("india_active_cases_compare", height = 500, width = 700)
            )
          )
        ),
        tabPanel(
          title = "Compare Districts",
          fluidRow(
            column(width = 3,
                   selectInput("state_1cc", "Select first state", states),
                   selectInput("district1c", "Select first district (orange)", choices = ap),
            ),
            column(width = 3,
                   selectInput("state_2cc", "Select second state", states),
                   selectInput("district2c", "Select second district (yellow)", choices = ap),
            ),
            
            column(12,
                   plotlyOutput("dist_active_cases_compare", height = 500, width = 700)
            )
          )
        )
      )
    )
  ),
  tags$head(tags$style(HTML('* {font-family: "Century Gothic"};')))
)


server <- function(input, output, session) {
  output$state_active_cases <- renderPlotly({

    state_data %>%
      filter(Date >= (Sys.Date()-input$days_state) & Date < Sys.Date()) -> state_data
    
    state_data %>%
      group_by(State) %>%
      arrange(Date) %>%
      mutate(daily_conf = Confirmed - lag(Confirmed, default = first(Confirmed)),
             daily_rec = Recovered - lag(Recovered, default = first(Recovered)),
             daily_dec = Deceased - lag(Deceased, default = first(Deceased))) -> state_data
    
    state_data %>%
      group_by(State) %>%
      arrange(desc(Date)) %>%
      mutate(active_cases = lag(Confirmed - Recovered - Deceased - (daily_conf - daily_rec - daily_dec))) -> state_active
    
    
    state_active <- state_active %>% filter(!State %in% c("India"))
    
    state_active %>%
      ggplot( aes(x=Date, y=active_cases)) +
      geom_area(fill="#E35925", alpha=0.5) +
      geom_line(color="#E35925") +
      facet_wrap(State ~ ., ncol = 4, scales = "free_y") +
      theme_minimal() +
      theme(axis.line = element_line(colour = "black"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank()) -> p
    
    ggplotly(p) %>%
      layout(autosize = T)
    
  })
  
  output$dist_active_cases <- renderPlotly({
    
    district_data %>%
      filter(Date >= (Sys.Date()-input$days_dist) & Date < Sys.Date()) -> district_data
    
    district_data %>%
      group_by(State, District) %>%
      arrange(Date) %>%
      mutate(daily_conf = Confirmed - lag(Confirmed, default = first(Confirmed)),
             daily_rec = Recovered - lag(Recovered, default = first(Recovered)),
             daily_dec = Deceased - lag(Deceased, default = first(Deceased))) -> district_data_a
    
    district_data_a %>%
      group_by(State, District) %>%
      arrange(desc(Date)) %>%
      mutate(active_cases = lag(Confirmed - Recovered - Deceased - (daily_conf - daily_rec - daily_dec))) -> district_active
    
    district_active %>%
      ggplot( aes(x=Date, y=active_cases)) +
      geom_area(fill="#E35925", alpha=0.5) +
      geom_line(color="#E35925") +
      facet_wrap(state_dist ~ ., ncol = 4, scales = "free_y") +
      theme_minimal() +
      theme(axis.line = element_line(colour = "black"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank()) -> p
    
    ggplotly(p)
    
  })
  
  output$state_active_cases_compare <- renderPlotly({
    
    state_data %>%
      filter(State == "India") -> india_data1
    
    state_data %>%
      filter(State == input$state1c) -> state_data1c
    
    state_data1c %>%
      filter(Date >= Sys.Date() - 120 & Date < Sys.Date()) -> state_data1c
    
    state_data1c %>%
      arrange(Date) %>%
      mutate(daily_conf = Confirmed - lag(Confirmed, default = first(Confirmed)),
             daily_rec = Recovered - lag(Recovered, default = first(Recovered)),
             daily_dec = Deceased - lag(Deceased, default = first(Deceased))) -> state_data1c
    
    state_data1c %>%
      arrange(desc(Date)) %>%
      mutate(active_cases = lag(Confirmed - Recovered - Deceased - (daily_conf - daily_rec - daily_dec)),
             `Active Cases per lakh` = (active_cases/100000)) -> state_active1c
    
    state_data %>%
      filter(State == input$state2c) -> state_data2c
    
    state_data2c %>%
      filter(Date >= Sys.Date() - 120 & Date < Sys.Date()) -> state_data2c
    
    state_data2c %>%
      arrange(Date) %>%
      mutate(daily_conf = Confirmed - lag(Confirmed, default = first(Confirmed)),
             daily_rec = Recovered - lag(Recovered, default = first(Recovered)),
             daily_dec = Deceased - lag(Deceased, default = first(Deceased))) -> state_data2c
    
    state_data2c %>%
      arrange(desc(Date)) %>%
      mutate(active_cases = lag(Confirmed - Recovered - Deceased - (daily_conf - daily_rec - daily_dec)),
             `Active Cases per lakh` = (active_cases/100000)) -> state_active2c
    
    p <- ggplot() + 
      geom_area(data = state_active1c, aes(x = Date, y = `Active Cases per lakh`, text = input$state1), fill = "#E35925", color = "#E35925", alpha = 0.5) +
      geom_area(data = state_active2c, aes(x = Date, y = `Active Cases per lakh`, text = input$state2), fill = "#2FAA9F", color = "#2FAA9F", alpha = 0.5) +
      xlab("") +
      ylab("Active COVID-19 Cases per lakh population") +
      theme_minimal() +
      theme(axis.line = element_line(colour = "black"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())
    
    ggplotly(p, tooltip = c("text","x","y"))
    
  })
  
  output$india_active_cases_compare <- renderPlotly({
    
    state_data %>%
      filter(State == "India") -> india_data1
    
    india_data1 %>%
      filter(Date >= Sys.Date() - 120 & Date < Sys.Date()) -> india_data1
    
    india_data1 %>%
      arrange(Date) %>%
      mutate(daily_conf = Confirmed - lag(Confirmed, default = first(Confirmed)),
             daily_rec = Recovered - lag(Recovered, default = first(Recovered)),
             daily_dec = Deceased - lag(Deceased, default = first(Deceased))) -> india_data1
    
    india_data1 %>%
      arrange(desc(Date)) %>%
      mutate(active_cases = lag(Confirmed - Recovered - Deceased - (daily_conf - daily_rec - daily_dec)),
             `Active Cases per lakh` = (active_cases/100000)) -> india_active
    
    p <- ggplot() + 
      geom_area(data = india_active, aes(x = Date, y = `Active Cases per lakh`, text = "India" ), fill = "#F2C200", color = "#F2C200", alpha = 0.5) +
      xlab("") +
      ylab("Active COVID-19 Cases per lakh population") +
      ggtitle("India Active Cases") + 
      theme_minimal() +
      theme(axis.line = element_line(colour = "black"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())
    
    ggplotly(p, tooltip = c("text","x","y"))
    
  })
  
  observeEvent(input$state_1cc,{
    updateSelectInput(session, 'district1c',
                      choices = unique(dist_one_day$District[dist_one_day$State==input$state_1cc]))
  })  
  
  observeEvent(input$state_2cc,{
    updateSelectInput(session, 'district2c',
                      choices = unique(dist_one_day$District[dist_one_day$State==input$state_2cc]))
  }) 
  
  output$dist_active_cases_compare <- renderPlotly({
    
    districts %>%
      filter(District == input$district1c) -> district_data1c
    
    district_data1c %>%
      filter(Date >= Sys.Date() - 120 & Date < Sys.Date()) -> district_data1c
    
    district_data1c %>%
      arrange(Date) %>%
      mutate(daily_conf = Confirmed - lag(Confirmed, default = first(Confirmed)),
             daily_rec = Recovered - lag(Recovered, default = first(Recovered)),
             daily_dec = Deceased - lag(Deceased, default = first(Deceased))) -> district_data1c
    
    district_data1c %>%
      arrange(desc(Date)) %>%
      mutate(`Active Cases` = lag(Confirmed - Recovered - Deceased - (daily_conf - daily_rec - daily_dec))) -> district_active1c
    
    districts %>%
      filter(District == input$district2c) -> district_data2c
    
    district_data2c %>%
      filter(Date >= Sys.Date() - 120 & Date < Sys.Date()) -> district_data2c
    
    district_data2c %>%
      arrange(Date) %>%
      mutate(daily_conf = Confirmed - lag(Confirmed, default = first(Confirmed)),
             daily_rec = Recovered - lag(Recovered, default = first(Recovered)),
             daily_dec = Deceased - lag(Deceased, default = first(Deceased))) -> district_data2c
    
    district_data2c %>%
      arrange(desc(Date)) %>%
      mutate(`Active Cases` = lag(Confirmed - Recovered - Deceased - (daily_conf - daily_rec - daily_dec))) -> district_active2c
    
    p <- ggplot() + 
      geom_area(data = district_active1c, aes(x = Date, y = `Active Cases`, text = input$district1), fill = "#E35925", color = "#E35925", alpha = 0.3) +
      geom_area(data = district_active2c, aes(x = Date, y = `Active Cases`, text = input$district2), fill = "#F2C200", color = "#F2C200", alpha = 0.3) +
      xlab("") +
      ylab("Active COVID-19 Cases") +
      theme_minimal() +
      theme(axis.line = element_line(colour = "black"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())
    
    ggplotly(p, tooltip = c("text","x","y"))
    
  })
  
  # Snapshot Tables ----
  output$tw_lw = DT::renderDataTable(tw_lw_coded, server = FALSE, selection = "single")
  output$tw_lm = DT::renderDataTable(tw_lm_coded, server = FALSE, selection = "single")
  
  output$tw_lw_state = DT::renderDataTable(tw_lw_coded_state, server = FALSE, selection = "single")
  output$tw_lm_state = DT::renderDataTable(tw_lm_coded_state, server = FALSE, selection = "single")
  
  # Download Button ----
  
  # District
  output$x3 = downloadHandler('district_covid_update_this_week_last_week.csv', content = function(file) {
    write.csv(tw_lw_final, file)
  })
  
  output$x4 = downloadHandler('district_covid_update_this_week_last_month.csv', content = function(file) {
    write.csv(tw_lm_final, file)
  })
  
  # State
  
  output$x5 = downloadHandler('state_covid_update_this_week_last_week.csv', content = function(file) {
    write.csv(tw_lw_final_state, file)
  })
  
  output$x6 = downloadHandler('state_covid_update_this_week_last_month.csv', content = function(file) {
    write.csv(tw_lm_final_state, file)
  })
  
  
}


shinyApp(ui, server)
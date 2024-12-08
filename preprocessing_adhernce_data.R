# Preprocessing adherence data
library(readxl)
library(dplyr)
library(tidyr)
library(ggpubr)
library(stringr)

# Load in TFVdp measurements from the dried blood spots
# when multiple entries for the same patient and visitcode exist, choose the one closer to the intended date
data_dbs <- read_excel("dbs.xlsx")
 
data_clean <- data_dbs %>%
  group_by(ptid,visitcode)%>%
  slice(which.min(abs(collectiondate - (84*as.integer(visitcode)))))%>%
  mutate(visitcode = as.numeric(visitcode))%>%
  select(ptid,visitcode,dbs_adj)

# Load in adherence data for EAM (in dataset prep_adh_m) and how many pills the patient got from the pharmacy (pharm_adh_m)
# convert the visitcodes to the the month of the visit and set it to 1 if it is wrongly set to 0 (in 1 case)
data_adherence <- read_excel("adherence.xlsx")

data_adh <- data_adherence %>%
  mutate(visitcode = (as.numeric(visitcode)-1000)/10)%>%
  mutate(visitcode = ifelse(visitcode == 0,1,visitcode))%>%
  select(ptid,visitcode,prep_adh_m,pharm_adh_m)

# merge the 2 dataframes
# if pharm == 0 and no value is entered for dbs or prep, set them to the minimum value
# filter only for visitcodes over 3 (since dbs did not have mesurements for 1 month)
data_gbtm <- merge(x = data_clean, y = data_adh, by = c("ptid","visitcode"), all = TRUE)%>%
  mutate(prep_adh_m = ifelse(is.na(prep_adh_m) & pharm_adh_m == 0,0,prep_adh_m))%>%
  mutate(dbs_adj = ifelse(is.na(dbs_adj) & pharm_adh_m == 0,100,dbs_adj))%>%
  filter(visitcode >= 3)%>%
  rename(dbs = dbs_adj)%>%
  rename(prep_adh = prep_adh_m)%>%
  rename(pharm_adh = pharm_adh_m)%>%
  ungroup()

# safe table in long format for further analysis
write.csv(data_gbtm,"adherence_values.csv",row.names = FALSE)

# convert dataframe from long to wide format, since that is required by the stata traj plugin
# NA is replaced with empty character to facilitate easier loading of the data in stata
output_gbtm <- data_gbtm %>%
  group_by(ptid)%>%
  pivot_wider(names_from = visitcode,values_from = c(dbs,prep_adh,pharm_adh))%>%
  select(ptid,dbs_3,dbs_6,dbs_9,dbs_12,dbs_15,dbs_18,dbs_21,dbs_24,prep_adh_3,prep_adh_6,prep_adh_9,prep_adh_12,prep_adh_15,prep_adh_18,prep_adh_21,prep_adh_24,pharm_adh_3,pharm_adh_6,pharm_adh_9,pharm_adh_12,pharm_adh_15,pharm_adh_18,pharm_adh_21,pharm_adh_24)%>%
  ungroup()%>%
  mutate(across(dbs_3:pharm_adh_24,as.character))%>%
  mutate(month1 = 3)%>%
  mutate(month2 = 6)%>%
  mutate(month3 = 9)%>%
  mutate(month4 = 12)%>%
  mutate(month5 = 15)%>%
  mutate(month6 = 18)%>%
  mutate(month7 = 21)%>%
  mutate(month8 = 24)%>%
  replace(is.na(.),"")

write.csv(output_gbtm,"stata_gbtm.csv",row.names = FALSE)
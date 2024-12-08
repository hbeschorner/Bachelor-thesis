# Compare models with each other
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(purrr)
library(shades)
library(magick)
library(forcats)
library(caret)
library(pdftools)

################ Load group membership (results from GBTM) for each data source and calculate average posterior probabilities

# read in group membership for DBS
groups_dbs <- read.csv("groups_dbs.csv") %>% 
  select(ptid,X_traj_Group,X_traj_ProbG1,X_traj_ProbG2,X_traj_ProbG3) %>% 
  rename(group_dbs = X_traj_Group)%>%
  rowwise()%>%
  mutate(group_prob = max(X_traj_ProbG1,X_traj_ProbG2,X_traj_ProbG3))%>%
  mutate(group_dbs = case_when(
    group_dbs == 1 ~ "low",
    group_dbs == 2 ~ "high",
    group_dbs == 3 ~ "declining"
  ))

# calculate average posterior probabilities for each group
prob_dbs_low = mean(as.data.frame(groups_dbs %>%
                                    filter(group_dbs == "low")%>%
                                    select(group_prob))$group_prob)
prob_dbs_low
prob_dbs_declining = mean(as.data.frame(groups_dbs %>%
                                    filter(group_dbs == "declining")%>%
                                    select(group_prob))$group_prob)
prob_dbs_declining
prob_dbs_high = mean(as.data.frame(groups_dbs %>%
                                    filter(group_dbs == "high")%>%
                                    select(group_prob))$group_prob)
prob_dbs_high

# read in group membership for EAM
groups_prep <- read.csv("groups_prep.csv") %>% 
  select(ptid,X_traj_Group,X_traj_ProbG1,X_traj_ProbG2,X_traj_ProbG3) %>% 
  rename(group_prep = X_traj_Group )%>%
  rowwise()%>%
  mutate(group_prob = max(X_traj_ProbG1,X_traj_ProbG2,X_traj_ProbG3))%>%
  mutate(group_prep = case_when(
    group_prep == 1 ~ "low",
    group_prep == 2 ~ "declining",
    group_prep == 3 ~ "high"
  ))

# calculate average posterior probabilities for each group
prob_prep_low = mean(as.data.frame(groups_prep %>%
                                    filter(group_prep == "low")%>%
                                    select(group_prob))$group_prob)
prob_prep_low
prob_prep_declining = mean(as.data.frame(groups_prep %>%
                                          filter(group_prep == "declining")%>%
                                          select(group_prob))$group_prob)
prob_prep_declining
prob_prep_high = mean(as.data.frame(groups_prep %>%
                                     filter(group_prep == "high")%>%
                                     select(group_prob))$group_prob)
prob_prep_high

# read in group membership for Pharmacy refills
groups_pharm <- read.csv("groups_pharm.csv") %>% 
  select(ptid,X_traj_Group,X_traj_ProbG1,X_traj_ProbG2,X_traj_ProbG3) %>% 
  rename(group_pharm = X_traj_Group) %>%
  rowwise()%>%
  mutate(group_prob = max(X_traj_ProbG1,X_traj_ProbG2,X_traj_ProbG3))%>%
  mutate(group_pharm = case_when(
    group_pharm == 1 ~ "low",
    group_pharm == 2 ~ "declining",
    group_pharm == 3 ~ "high"
  ))

# average posterior probabilities
prob_pharm_low = mean(as.data.frame(groups_pharm %>%
                                     filter(group_pharm == "low")%>%
                                     select(group_prob))$group_prob)
prob_pharm_low
prob_pharm_declining = mean(as.data.frame(groups_pharm %>%
                                           filter(group_pharm == "declining")%>%
                                           select(group_prob))$group_prob)
prob_pharm_declining
prob_pharm_high = mean(as.data.frame(groups_pharm %>%
                                      filter(group_pharm == "high")%>%
                                      select(group_prob))$group_prob)
prob_pharm_high

################ Calculate and plot confusion matrix/contingency table for each combination of data sources

# DBS and EAM
dbs_prep <- merge(groups_dbs%>%select(ptid,group_dbs)%>%mutate(group_dbs=as.factor(group_dbs))%>%mutate(group_dbs=fct_relevel(group_dbs,c("low","declining","high"))),groups_prep%>%select(ptid,group_prep)%>%mutate(group_prep=as.factor(group_prep))%>%mutate(group_prep=fct_relevel(group_prep,c("low","declining","high"))), by = c("ptid"))

# calculate table
contingency_dbs_prep <- confusionMatrix(data=dbs_prep$group_dbs,reference=dbs_prep$group_prep)
contingency_dbs_prep

plt_c1 <- as.data.frame(contingency_dbs_prep$table)
plt_c1$Prediction <- factor(plt_c1$Prediction, levels=rev(levels(plt_c1$Prediction)))

# plot results
c1 <- plt_c1 %>% 
  mutate(color = case_when(Prediction == Reference ~ 'green', 
                         Freq == 0 ~ 'white', 
                         TRUE ~ 'red')) %>%
  mutate(scaling = 2.5*(as.double(Freq)/329))%>%
  rowwise()%>%
  mutate(color=saturation(color,scaling)) %>%
  ggplot(aes(Prediction,Reference, fill= color)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_identity() + 
  scale_x_discrete(labels=c("high","declining","low")) +
  scale_y_discrete(labels=c("low","declining","high")) +
  labs(y = "class EAM",x = "class DBS")+
  ggtitle("A: Contingency table\n     DBS, EAM")

# DBS and pharmacy refills
dbs_pharm <- merge(groups_dbs%>%select(ptid,group_dbs)%>%mutate(group_dbs=as.factor(group_dbs))%>%mutate(group_dbs=fct_relevel(group_dbs,c("low","declining","high"))),groups_pharm%>%select(ptid,group_pharm)%>%mutate(group_pharm=as.factor(group_pharm))%>%mutate(group_pharm=fct_relevel(group_pharm,c("low","declining","high"))), by = c("ptid"))

# calculate table
contingency_dbs_pharm <- confusionMatrix(data=dbs_pharm$group_dbs,reference=dbs_pharm$group_pharm)
contingency_dbs_pharm

plt_c2 <- as.data.frame(contingency_dbs_pharm$table)
plt_c2$Prediction <- factor(plt_c2$Prediction, levels=rev(levels(plt_c2$Prediction)))

# plot results
c2 <- plt_c2 %>% 
  mutate(color = case_when(Prediction == Reference ~ 'green', 
                           Freq == 0 ~ 'white', 
                           TRUE ~ 'red')) %>%
  mutate(scaling = 2.5*(as.double(Freq)/329))%>%
  rowwise()%>%
  mutate(color=saturation(color,scaling)) %>%
  ggplot(aes(Prediction,Reference, fill= color)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_identity() + 
  scale_x_discrete(labels=c("high","declining","low")) +
  scale_y_discrete(labels=c("low","declining","high")) +
  labs(y = "class Pharmacy",x = "class DBS")+
  ggtitle("B: Contingency table\n     DBS,Pharmacy refills")

# EAM and pharmacy refills
prep_pharm <- merge(groups_prep%>%select(ptid,group_prep)%>%mutate(group_prep=as.factor(group_prep))%>%mutate(group_prep=fct_relevel(group_prep,c("low","declining","high"))),groups_pharm%>%select(ptid,group_pharm)%>%mutate(group_pharm=as.factor(group_pharm))%>%mutate(group_pharm=fct_relevel(group_pharm,c("low","declining","high"))), by = c("ptid"))

# calculate table
contingency_prep_pharm <- confusionMatrix(data=prep_pharm$group_prep,reference=prep_pharm$group_pharm)
contingency_prep_pharm

plt_c3 <- as.data.frame(contingency_prep_pharm$table)
plt_c3$Prediction <- factor(plt_c3$Prediction, levels=rev(levels(plt_c3$Prediction)))

# plot results
c3 <- plt_c3 %>% 
  mutate(color = case_when(Prediction == Reference ~ 'green', 
                           Freq == 0 ~ 'white', 
                           TRUE ~ 'red')) %>%
  mutate(scaling = 2.5*(as.double(Freq)/329))%>%
  rowwise()%>%
  mutate(color=saturation(color,scaling)) %>%
  ggplot(aes(Prediction,Reference, fill= color)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_identity() + 
  scale_x_discrete(labels=c("high","declining","low")) +
  scale_y_discrete(labels=c("low","declining","high")) +
  labs(y = "class Pharmacy",x = "class EAM")+
  ggtitle("C: Contingency table\n     EAM, Pharmacy refills")

# save plot with all tables
cont_tables_data_sources <- ggarrange(c1,c2,c3,nrow = 1)
ggsave(filename="cont_tables_data_sources.pdf",plot = cont_tables_data_sources,width=3,height=1, dpi =100,scale = 3.25)

################  concordance between dpw (DBS/EAM)

# Load in adherence values

# convert TFVdp values to adherence in dpw
data_dbs_values <- read.csv("groups_dbs.csv") %>%
  select(ptid,dbs_3, dbs_6, dbs_9, dbs_12, dbs_15, dbs_18, dbs_21, dbs_24,X_traj_Group)%>%
  pivot_longer(dbs_3:dbs_24,names_to = "month",names_prefix = "dbs_",values_to = "dbs")%>%
  mutate(month=factor(month,levels = c("3","6","9","12","15","18","21","24")))%>%
  mutate(adherence_dbs = case_when(is.na(dbs) ~ NA,
                                   dbs < 350~"<2dpw",
                                   dbs >= 350 & dbs < 700 ~ "2 to <4dpw",
                                   dbs >= 700 & dbs < 1250 ~ "4 to <7dpw",
                                   TRUE ~ "7dpw"))%>%
  rename(group_dbs = X_traj_Group)%>%
  mutate(group_dbs = case_when(
    group_dbs == 1 ~ "low",
    group_dbs == 2 ~ "high",
    group_dbs == 3 ~ "declining"
  ))%>%
  mutate(adherence_dbs = factor(adherence_dbs,levels = c("7dpw","4 to <7dpw","2 to <4dpw","<2dpw")))

# convert EAM adherence to dpw
data_eam_values <- read.csv("groups_prep.csv") %>%
  select(ptid,prep_adh_3, prep_adh_6, prep_adh_9, prep_adh_12, prep_adh_15, prep_adh_18, prep_adh_21, prep_adh_24,X_traj_Group)%>%
  pivot_longer(prep_adh_3:prep_adh_24,names_to = "month",names_prefix = "prep_adh_",values_to = "eam")%>%
  mutate(month=factor(month,levels = c("3","6","9","12","15","18","21","24")))%>%
  mutate(adherence_eam = case_when(is.na(eam) ~ NA,
                                   eam < 2 * (100/7) ~"<2dpw",
                                   eam >= 2 * (100/7) & eam < 4 * (100/7) ~ "2 to <4dpw",
                                   eam >= 4 * (100/7) & eam < 7 * (100/7) ~ "4 to <7dpw",
                                   TRUE ~ "7dpw"))%>%
  drop_na(adherence_eam)%>%
  rename(group_prep = X_traj_Group)%>%
  mutate(group_prep = case_when(
    group_prep == 1 ~ "low",
    group_prep == 2 ~ "declining",
    group_prep == 3 ~ "high"
  ))%>%
  mutate(adherence_eam = factor(adherence_eam,levels = c("7dpw","4 to <7dpw","2 to <4dpw","<2dpw")))

# Calculate and plot confusion matrix/contingency table for entries in dpw for DBS/EAM
cont_adherence <- merge(data_dbs_values%>%select(ptid,month,group_dbs,adherence_dbs),data_eam_values%>%select(ptid,month,adherence_eam), by = c("ptid","month"))
conf_adherence <- confusionMatrix(data=cont_adherence$adherence_dbs,reference=cont_adherence$adherence_eam)
conf_adherence

# Calculate and plot confusion matrix/contingency table for entries in dpw for each DBS trajectory as reference
# low DBS-trajectory
conf_low <- confusionMatrix(data=(cont_adherence%>%filter(group_dbs=="low"))$adherence_dbs,reference=(cont_adherence%>%filter(group_dbs=="low"))$adherence_eam)
conf_low

# declining DBS-trajectory
conf_decl <- confusionMatrix(data=(cont_adherence%>%filter(group_dbs=="declining"))$adherence_dbs,reference=(cont_adherence%>%filter(group_dbs=="declining"))$adherence_eam)
conf_decl

# high DBS-trajectory
conf_high <- confusionMatrix(data=(cont_adherence%>%filter(group_dbs=="high"))$adherence_dbs,reference=(cont_adherence%>%filter(group_dbs=="high"))$adherence_eam)
conf_high

# plot table for all trajectories combined
plt_c4 <- as.data.frame(conf_adherence$table)
plt_c4$Prediction <- factor(plt_c4$Prediction, levels=rev(levels(plt_c4$Prediction)))

cont_dbs_eam_all <- plt_c4 %>% 
  mutate(color = case_when(Prediction == Reference ~ 'green', 
                           Freq == 0 ~ 'white', 
                           TRUE ~ 'red')) %>%
  mutate(scaling = 2.5*(as.double(Freq)/sum(plt_c4$Freq)))%>%
  rowwise()%>%
  mutate(color=saturation(color,scaling)) %>%
  ggplot(aes(Prediction,Reference, fill= color)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_identity() + 
  scale_x_discrete(labels=c("<2dpw","2 to <4dpw","4 to <7dpw","7dpw")) +
  scale_y_discrete(labels=c("7dpw","4 to <7dpw","2 to <4dpw","<2dpw")) +
  labs(x = "adherence DBS",y = "adherence EAM")+
  ggtitle("Contingency table doses per week \nDBS,EAM")

ggsave(filename="cont_dbs_eam_all.pdf",plot = cont_dbs_eam_all,width=1.5,height=1, dpi =100,scale = 3)

# plot table for each DBS trajectory
# low DBS trajectory
plt_clow <- as.data.frame(conf_low$table)
plt_clow$Prediction <- factor(plt_clow$Prediction, levels=rev(levels(plt_clow$Prediction)))

clow <- plt_clow %>% 
  mutate(color = case_when(Prediction == Reference ~ 'green', 
                           Freq == 0 ~ 'white', 
                           TRUE ~ 'red')) %>%
  mutate(scaling = 2.5*(as.double(Freq)/(nrow(cont_adherence%>%filter(group_dbs=="low")))))%>%
  rowwise()%>%
  mutate(color=saturation(color,scaling)) %>%
  ggplot(aes(Prediction,Reference, fill= color)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_identity() + 
  scale_x_discrete(labels=c("<2dpw","2 to <4dpw","4 to <7dpw","7dpw")) +
  scale_y_discrete(labels=c("7dpw","4 to <7dpw","2 to <4dpw","<2dpw")) +
  labs(x = "adherence DBS",y = "adherence EAM")+
  ggtitle("A: Contingency table doses per week \nlow DBS-trajectory,EAM")

# declining DBS trajectory
plt_decl <- as.data.frame(conf_decl$table)
plt_decl$Prediction <- factor(plt_decl$Prediction, levels=rev(levels(plt_decl$Prediction)))

decl <- plt_decl %>% 
  mutate(color = case_when(Prediction == Reference ~ 'green', 
                           Freq == 0 ~ 'white', 
                           TRUE ~ 'red')) %>%
  mutate(scaling = 2.5*(as.double(Freq)/(nrow(cont_adherence%>%filter(group_dbs=="declining")))))%>%
  rowwise()%>%
  mutate(color=saturation(color,scaling)) %>%
  ggplot(aes(Prediction,Reference, fill= color)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_identity() + 
  scale_x_discrete(labels=c("<2dpw","2 to <4dpw","4 to <7dpw","7dpw")) +
  scale_y_discrete(labels=c("7dpw","4 to <7dpw","2 to <4dpw","<2dpw")) +
  labs(x = "adherence DBS",y = "adherence EAM")+
  ggtitle("B: Contingency table doses per week \ndeclining DBS-trajectory,EAM")

plt_high <- as.data.frame(conf_high$table)
plt_high$Prediction <- factor(plt_high$Prediction, levels=rev(levels(plt_high$Prediction)))

# high DBS trajectory
high <- plt_high %>% 
  mutate(color = case_when(Prediction == Reference ~ 'green', 
                           Freq == 0 ~ 'white', 
                           TRUE ~ 'red')) %>%
  mutate(scaling = 2.5*(as.double(Freq)/(nrow(cont_adherence%>%filter(group_dbs=="high")))))%>%
  rowwise()%>%
  mutate(color=saturation(color,scaling)) %>%
  ggplot(aes(Prediction,Reference, fill= color)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_identity() + 
  scale_x_discrete(labels=c("<2dpw","2 to <4dpw","4 to <7dpw","7dpw")) +
  scale_y_discrete(labels=c("7dpw","4 to <7dpw","2 to <4dpw","<2dpw")) +
  labs(x = "adherence DBS",y = "adherence EAM")+
  ggtitle("C: Contingency table doses per week \nhigh DBS-trajectory,EAM")

cont_tables_dbs_eam_dpw <- ggarrange(clow,decl,high,nrow = 1)
ggsave(filename="cont_tables_dbs_eam_dpw.pdf",plot = cont_tables_dbs_eam_dpw,width=3,height=1, dpi =100,scale = 4)

################ Concordance over time

concordance_over_time_1 <- merge(data_dbs_values,data_eam_values,by=c("ptid","month"))%>%
  filter(!is.na(eam) & !is.na(dbs))%>%
  mutate(accuracy = case_when(adherence_dbs == adherence_eam~ "same",
                              adherence_dbs == "<2dpw" & adherence_eam %in% c("2 to <4dpw","4 to <7dpw","7dpw") ~ "less than EAM",
                              adherence_dbs == "2 to <4dpw" & adherence_eam %in% c("4 to <7dpw","7dpw") ~ "less than EAM",
                              adherence_dbs == "<2dpw" & adherence_eam == "7dpw" ~ "less than EAM",
                              TRUE ~ "more than EAM"))%>%
  group_by(month,accuracy)%>%
  count()%>%
  group_by(month)%>%
  mutate(n = n/sum(n))%>%
  ungroup()

ot_1 <- ggplot(concordance_over_time_1,aes(y=n,x=month,group=accuracy,color=accuracy))+
  geom_line()+
  ylab("")+
  ggtitle("A: all DBS trajectories combined")+
  guides(group= guide_legend(title ="DBS compared to EAM"))+
  guides(color=guide_legend(title ="DBS compared to EAM"))+
  scale_y_continuous(labels = scales::percent)

concordance_over_time_2 <- merge(data_dbs_values%>%filter(group_dbs=="low"),data_eam_values,by=c("ptid","month"))%>%
  filter(!is.na(eam) & !is.na(dbs))%>%
  mutate(accuracy = case_when(adherence_dbs == adherence_eam~ "same",
                              adherence_dbs == "<2dpw" & adherence_eam %in% c("2 to <4dpw","4 to <7dpw","7dpw") ~ "less than EAM",
                              adherence_dbs == "2 to <4dpw" & adherence_eam %in% c("4 to <7dpw","7dpw") ~ "less than EAM",
                              adherence_dbs == "<2dpw" & adherence_eam == "7dpw" ~ "less than EAM",
                              TRUE ~ "more than EAM"))%>%
  group_by(month,accuracy)%>%
  count()%>%
  group_by(month)%>%
  mutate(n = n/sum(n))%>%
  ungroup()

ot_2 <- ggplot(concordance_over_time_2,aes(y=n,x=month,group=accuracy,color=accuracy))+
  geom_line()+
  ylab("")+
  ggtitle("B: low DBS-tajectory")+
  guides(group= guide_legend(title ="DBS compared to EAM"))+
  guides(color=guide_legend(title ="DBS compared to EAM"))+
  scale_y_continuous(labels = scales::percent)

concordance_over_time_3 <- merge(data_dbs_values%>%filter(group_dbs=="declining"),data_eam_values,by=c("ptid","month"))%>%
  filter(!is.na(eam) & !is.na(dbs))%>%
  mutate(accuracy = case_when(adherence_dbs == adherence_eam~ "same",
                              adherence_dbs == "<2dpw" & adherence_eam %in% c("2 to <4dpw","4 to <7dpw","7dpw") ~ "less than EAM",
                              adherence_dbs == "2 to <4dpw" & adherence_eam %in% c("4 to <7dpw","7dpw") ~ "less than EAM",
                              adherence_dbs == "<2dpw" & adherence_eam == "7dpw" ~ "less than EAM",
                              TRUE ~ "more than EAM"))%>%
  group_by(month,accuracy)%>%
  count()%>%
  group_by(month)%>%
  mutate(n = n/sum(n))%>%
  ungroup()

ot_3 <- ggplot(concordance_over_time_3,aes(y=n,x=month,group=accuracy,color=accuracy))+
  geom_line()+
  ylab("")+
  ggtitle("C: declining DBS-trajectory")+
  guides(group= guide_legend(title ="DBS compared to EAM"))+
  guides(color=guide_legend(title ="DBS compared to EAM"))+
  scale_y_continuous(labels = scales::percent)

concordance_over_time_4 <- merge(data_dbs_values%>%filter(group_dbs=="high"),data_eam_values,by=c("ptid","month"))%>%
  filter(!is.na(eam) & !is.na(dbs))%>%
  mutate(accuracy = case_when(adherence_dbs == adherence_eam~ "same",
                              adherence_dbs == "<2dpw" & adherence_eam %in% c("2 to <4dpw","4 to <7dpw","7dpw") ~ "less than EAM",
                              adherence_dbs == "2 to <4dpw" & adherence_eam %in% c("4 to <7dpw","7dpw") ~ "less than EAM",
                              adherence_dbs == "<2dpw" & adherence_eam == "7dpw" ~ "less than EAM",
                              TRUE ~ "more than EAM"))%>%
  group_by(month,accuracy)%>%
  count()%>%
  group_by(month)%>%
  mutate(n = n/sum(n))%>%
  ungroup()

ot_4 <- ggplot(concordance_over_time_4,aes(y=n,x=month,group=accuracy,color=accuracy))+
  geom_line()+
  ylab("")+
  ggtitle("D: high DBS-trajectory")+
  guides(group= guide_legend(title ="DBS compared to EAM"))+
  guides(color=guide_legend(title ="DBS compared to EAM"))+
  scale_y_continuous(labels = scales::percent)

ot <- ggarrange(ot_1,ot_2,ot_3,ot_4,ncol=2, nrow=2, common.legend = TRUE, legend="bottom")
ot <- annotate_figure(ot, top = text_grob("DBS adherence compared to EAM adherence over time", 
                                    color = "black", face = "bold", size = 14))
ggsave(filename="adh_over_time.pdf",plot = ot,width=3,height=2, dpi =100,scale = 2.8)

################ Impact of dropouts

# remove dropouts
cont_adherence_dropouts <- merge(data_dbs_values%>%select(ptid,month,group_dbs,dbs,adherence_dbs),data_eam_values%>%select(ptid,month,eam,adherence_eam), by = c("ptid","month"))
cont_adherence_dropouts <- cont_adherence_dropouts %>% 
  filter(!is.na(dbs)&!is.na(eam))%>%
  group_by(ptid)%>%
  mutate(month = as.numeric(month))%>%
  mutate(stopped = ifelse(map_lgl(row_number(),~any(month>=month[.x] & (eam > 0 | dbs >100))),
                          "no", "yes"))%>%
  filter(stopped == "no")%>%
  mutate(month=month*3)%>%
  mutate(month=factor(month,levels = c("3","6","9","12","15","18","21","24")))

# calculate contingency table/confusion matrix for DBS/EAM for all trajectories (in dpw)
conf_adherence_dropouts <- confusionMatrix(data=cont_adherence_dropouts$adherence_dbs,reference=cont_adherence_dropouts$adherence_eam)
conf_adherence_dropouts

# calculate contingency table/confusion matrix for DBS/EAM for each DBS trajectorie (in dpw)
# low DBS trajectory
conf_low_dropouts <- confusionMatrix(data=(cont_adherence_dropouts%>%filter(group_dbs=="low"))$adherence_dbs,reference=(cont_adherence_dropouts%>%filter(group_dbs=="low"))$adherence_eam)
conf_low_dropouts

# declining DBS trajectory
conf_decl_dropouts <- confusionMatrix(data=(cont_adherence_dropouts%>%filter(group_dbs=="declining"))$adherence_dbs,reference=(cont_adherence_dropouts%>%filter(group_dbs=="declining"))$adherence_eam)
conf_decl_dropouts

# high DBS trajectory
conf_high_dropouts <- confusionMatrix(data=(cont_adherence_dropouts%>%filter(group_dbs=="high"))$adherence_dbs,reference=(cont_adherence_dropouts%>%filter(group_dbs=="high"))$adherence_eam)
conf_high_dropouts

# plot results for all trajectories
plt_c4_dropouts <- as.data.frame(conf_adherence_dropouts$table)
plt_c4_dropouts$Prediction <- factor(plt_c4_dropouts$Prediction, levels=rev(levels(plt_c4_dropouts$Prediction)))

cont_dbs_eam_dropouts <- plt_c4_dropouts %>% 
  mutate(color = case_when(Prediction == Reference ~ 'green', 
                           Freq == 0 ~ 'white', 
                           TRUE ~ 'red')) %>%
  mutate(scaling = 2.5*(as.double(Freq)/1899))%>%
  rowwise()%>%
  mutate(color=saturation(color,scaling)) %>%
  ggplot(aes(Prediction,Reference, fill= color)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_identity() + 
  scale_x_discrete(labels=c("<2dpw","2 to <4dpw","4 to <7dpw","7dpw")) +
  scale_y_discrete(labels=c("7dpw","4 to <7dpw","2 to <4dpw","<2dpw")) +
  labs(x = "adherence DBS",y = "adherence EAM")+
  ggtitle("Contingency table doses per week\n without dropouts:DBS,EAM")

ggsave(filename="cont_dbs_eam_dropouts.pdf",plot = cont_dbs_eam_dropouts,width=1.5,height=1, dpi =100,scale = 3)

# plot results for each DBS trajectory
# low DBS trajectory
plt_clow_dropouts <- as.data.frame(conf_low_dropouts$table)
plt_clow_dropouts$Prediction <- factor(plt_clow_dropouts$Prediction, levels=rev(levels(plt_clow_dropouts$Prediction)))

clow_dropouts <- plt_clow_dropouts %>% 
  mutate(color = case_when(Prediction == Reference ~ 'green', 
                           Freq == 0 ~ 'white', 
                           TRUE ~ 'red')) %>%
  mutate(scaling = 2.5*(as.double(Freq)/(nrow(cont_adherence%>%filter(group_dbs=="low")))))%>%
  rowwise()%>%
  mutate(color=saturation(color,scaling)) %>%
  ggplot(aes(Prediction,Reference, fill= color)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_identity() + 
  scale_x_discrete(labels=c("<2dpw","2 to <4dpw","4 to <7dpw","7dpw")) +
  scale_y_discrete(labels=c("7dpw","4 to <7dpw","2 to <4dpw","<2dpw")) +
  labs(x = "adherence DBS",y = "adherence EAM")+
  ggtitle("A: Contingency table doses per week \nw/o dropouts: low DBS-trajectory,EAM")

# declining DBS trajectory
plt_decl_dropouts <- as.data.frame(conf_decl_dropouts$table)
plt_decl_dropouts$Prediction <- factor(plt_decl_dropouts$Prediction, levels=rev(levels(plt_decl_dropouts$Prediction)))

decl_dropouts <- plt_decl_dropouts %>% 
  mutate(color = case_when(Prediction == Reference ~ 'green', 
                           Freq == 0 ~ 'white', 
                           TRUE ~ 'red')) %>%
  mutate(scaling = 2.5*(as.double(Freq)/(nrow(cont_adherence%>%filter(group_dbs=="declining")))))%>%
  rowwise()%>%
  mutate(color=saturation(color,scaling)) %>%
  ggplot(aes(Prediction,Reference, fill= color)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_identity() + 
  scale_x_discrete(labels=c("<2dpw","2 to <4dpw","4 to <7dpw","7dpw")) +
  scale_y_discrete(labels=c("7dpw","4 to <7dpw","2 to <4dpw","<2dpw")) +
  labs(x = "adherence DBS",y = "adherence EAM")+
  ggtitle("B: Contingency table doses per week \nw/o dropouts: decl. DBS-trajectory,EAM")

# high DBS trajectory
plt_high_dropouts <- as.data.frame(conf_high_dropouts$table)
plt_high_dropouts$Prediction <- factor(plt_high_dropouts$Prediction, levels=rev(levels(plt_high_dropouts$Prediction)))

high_dropouts <- plt_high_dropouts %>% 
  mutate(color = case_when(Prediction == Reference ~ 'green', 
                           Freq == 0 ~ 'white', 
                           TRUE ~ 'red')) %>%
  mutate(scaling = 2.5*(as.double(Freq)/(nrow(cont_adherence%>%filter(group_dbs=="high")))))%>%
  rowwise()%>%
  mutate(color=saturation(color,scaling)) %>%
  ggplot(aes(Prediction,Reference, fill= color)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_identity() + 
  scale_x_discrete(labels=c("<2dpw","2 to <4dpw","4 to <7dpw","7dpw")) +
  scale_y_discrete(labels=c("7dpw","4 to <7dpw","2 to <4dpw","<2dpw")) +
  labs(x = "adherence DBS",y = "adherence EAM")+
  ggtitle("C: Contingency table doses per week \nw/o dropouts: high DBS-trajectory,EAM")

cont_tables_dbs_eam_dropouts <- ggarrange(clow_dropouts,decl_dropouts,high_dropouts,nrow = 1)
ggsave(filename="cont_tables_dbs_eam_dropouts.pdf",plot = cont_tables_dbs_eam_dropouts,width=3,height=1, dpi =100,scale = 4.2)

################ Adherence in dpw for DBS and EAM without dropouts
# DBS
adh_plot_dbs_dropouts <- ggplot(cont_adherence_dropouts%>%filter(!is.na(adherence_dbs)),aes(x=month))+
  geom_bar(aes(fill = adherence_dbs),position = "fill")+
  theme_bw()+
  scale_fill_manual(
    values = c(
      "<2dpw" = "#d7191c",
      "2 to <4dpw" = "#fdae61",
      "4 to <7dpw" = "#a6d96a",
      "7dpw" = "#1a9641"
    ))+
  scale_y_continuous(name = NULL,labels = scales::percent_format(accuracy = 1),breaks = c(0,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))+
  guides(fill = guide_legend(title = "Adherence"))+
  ggtitle("A: TFVdp concentration \nwithout dropouts")

# EAM
adh_plot_eam_dropouts <- ggplot(cont_adherence_dropouts%>%filter(!is.na(adherence_eam)),aes(x=month))+
  geom_bar(aes(fill = adherence_eam),position = "fill")+
  theme_bw()+
  scale_fill_manual(
    values = c(
      "<2dpw" = "#d7191c",
      "2 to <4dpw" = "#fdae61",
      "4 to <7dpw" = "#a6d96a",
      "7dpw" = "#1a9641"
    ))+
  scale_y_continuous(name = NULL,labels = scales::percent_format(accuracy = 1),breaks = c(0,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))+
  guides(fill = guide_legend(title = "Adherence"))+
  ggtitle("A: EAM adherence \nwithout dropouts")

adh_dpw_dropouts <- ggarrange(adh_plot_dbs_dropouts,adh_plot_eam_dropouts,nrow = 1,common.legend = TRUE, legend="bottom")
ggsave(filename="adh_dpw_dropouts.pdf",plot = adh_dpw_dropouts,width=2,height=1.6, dpi =100,scale = 3)

################ Concordance over time without dropouts

# plot concordance over time (DBS/EAM) (for dpw) for all trajectories
concordance_over_time_1_dropouts <- cont_adherence_dropouts%>%
  mutate(accuracy = case_when(adherence_dbs == adherence_eam~ "same",
                              adherence_dbs == "<2dpw" & adherence_eam %in% c("2 to <4dpw","4 to <7dpw","7dpw") ~ "less than EAM",
                              adherence_dbs == "2 to <4dpw" & adherence_eam %in% c("4 to <7dpw","7dpw") ~ "less than EAM",
                              adherence_dbs == "<2dpw" & adherence_eam == "7dpw" ~ "less than EAM",
                              TRUE ~ "more than EAM"))%>%
  group_by(month,accuracy)%>%
  count()%>%
  group_by(month)%>%
  mutate(n = n/sum(n))%>%
  ungroup()

ot_1_dropouts <- ggplot(concordance_over_time_1_dropouts,aes(y=n,x=month,group=accuracy,color=accuracy))+
  geom_line()+
  ylab("")+
  ggtitle("A: all DBS trajectories combined")+
  guides(group= guide_legend(title ="DBS compared to EAM"))+
  guides(color=guide_legend(title ="DBS compared to EAM"))+
  scale_y_continuous(labels = scales::percent)

# plot concordance over time (DBS/EAM) (for dpw) for low DBS-trajectory
concordance_over_time_2_dropouts <- cont_adherence_dropouts%>%
  filter(group_dbs == "low")%>%
  mutate(accuracy = case_when(adherence_dbs == adherence_eam~ "same",
                              adherence_dbs == "<2dpw" & adherence_eam %in% c("2 to <4dpw","4 to <7dpw","7dpw") ~ "less than EAM",
                              adherence_dbs == "2 to <4dpw" & adherence_eam %in% c("4 to <7dpw","7dpw") ~ "less than EAM",
                              adherence_dbs == "<2dpw" & adherence_eam == "7dpw" ~ "less than EAM",
                              TRUE ~ "more than EAM"))%>%
  group_by(month,accuracy)%>%
  count()%>%
  group_by(month)%>%
  mutate(n = n/sum(n))%>%
  ungroup()

ot_2_dropouts <- ggplot(concordance_over_time_2_dropouts,aes(y=n,x=month,group=accuracy,color=accuracy))+
  geom_line()+
  ylab("")+
  ggtitle("B: low DBS-tajectory")+
  guides(group= guide_legend(title ="DBS compared to EAM"))+
  guides(color=guide_legend(title ="DBS compared to EAM"))+
  scale_y_continuous(labels = scales::percent)

# plot concordance over time (DBS/EAM) (for dpw) for declining DBS-trajectory
concordance_over_time_3_dropouts <- cont_adherence_dropouts%>%
  filter(group_dbs == "declining")%>%
  mutate(accuracy = case_when(adherence_dbs == adherence_eam~ "same",
                              adherence_dbs == "<2dpw" & adherence_eam %in% c("2 to <4dpw","4 to <7dpw","7dpw") ~ "less than EAM",
                              adherence_dbs == "2 to <4dpw" & adherence_eam %in% c("4 to <7dpw","7dpw") ~ "less than EAM",
                              adherence_dbs == "<2dpw" & adherence_eam == "7dpw" ~ "less than EAM",
                              TRUE ~ "more than EAM"))%>%
  group_by(month,accuracy)%>%
  count()%>%
  group_by(month)%>%
  mutate(n = n/sum(n))%>%
  ungroup()

ot_3_dropouts <- ggplot(concordance_over_time_3_dropouts,aes(y=n,x=month,group=accuracy,color=accuracy))+
  geom_line()+
  ylab("")+
  ggtitle("C: declining DBS-trajectory")+
  guides(group= guide_legend(title ="DBS compared to EAM"))+
  guides(color=guide_legend(title ="DBS compared to EAM"))+
  scale_y_continuous(labels = scales::percent)

# plot concordance over time (DBS/EAM) (for dpw) for high DBS-trajectory
concordance_over_time_4_dropouts <- cont_adherence_dropouts%>%
  filter(group_dbs == "high")%>%
  mutate(accuracy = case_when(adherence_dbs == adherence_eam~ "same",
                              adherence_dbs == "<2dpw" & adherence_eam %in% c("2 to <4dpw","4 to <7dpw","7dpw") ~ "less than EAM",
                              adherence_dbs == "2 to <4dpw" & adherence_eam %in% c("4 to <7dpw","7dpw") ~ "less than EAM",
                              adherence_dbs == "<2dpw" & adherence_eam == "7dpw" ~ "less than EAM",
                              TRUE ~ "more than EAM"))%>%
  group_by(month,accuracy)%>%
  count()%>%
  group_by(month)%>%
  mutate(n = n/sum(n))%>%
  ungroup()

ot_4_dropouts <- ggplot(concordance_over_time_4_dropouts,aes(y=n,x=month,group=accuracy,color=accuracy))+
  geom_line()+
  ylab("")+
  ggtitle("D: high DBS-trajectory")+
  guides(group= guide_legend(title ="DBS compared to EAM"))+
  guides(color=guide_legend(title ="DBS compared to EAM"))+
  scale_y_continuous(labels = scales::percent)

# arrange and save plots
ot_dropouts <- ggarrange(ot_1_dropouts,ot_2_dropouts,ot_3_dropouts,ot_4_dropouts,ncol=2, nrow=2, common.legend = TRUE, legend="bottom")
ot_dropouts <- annotate_figure(ot_dropouts, top = text_grob("DBS adherence without dropouts compared to EAM adherence over time", 
                                    color = "black", face = "bold", size = 14))
ggsave(filename="adh_dropouts_over_time.pdf",plot = ot_dropouts,width=3,height=2, dpi =100,scale = 3)

################ Impact of SMS

# read in study arm for each participant
data_sms <- read_excel("questionnaire.xlsx")%>%
  select(ptid,arm)%>%
  distinct(ptid,arm)

# filter according to study arm
dbs_sms <- merge(merge(data_dbs_values,data_sms,by = "ptid"),data_eam_values,by = c("ptid","month"))%>%
  filter(arm == "SMS Reminders")
dbs_no_sms <- merge(merge(data_dbs_values,data_sms,by = "ptid"),data_eam_values,by = c("ptid","month"))%>%
  filter(arm == "No SMS Reminders")

# calculate contingency table/confusion matrix between DBS/EAM for participants with SMS-reminders
conf_sms <- confusionMatrix(data=dbs_sms$adherence_dbs,reference=dbs_sms$adherence_eam)
conf_sms

plt_conf_sms <- as.data.frame(conf_sms$table)
plt_conf_sms$Prediction <- factor(plt_conf_sms$Prediction, levels=rev(levels(plt_conf_sms$Prediction)))

# plot table
c_sms<- plt_conf_sms %>% 
  mutate(color = case_when(Prediction == Reference ~ 'green', 
                           Freq == 0 ~ 'white', 
                           TRUE ~ 'red')) %>%
  mutate(scaling = 2.5*(as.double(Freq)/nrow(dbs_sms)))%>%
  rowwise()%>%
  mutate(color=saturation(color,scaling)) %>%
  ggplot(aes(Prediction,Reference, fill= color)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_identity() + 
  scale_x_discrete(labels=c("<2dpw","2 to <4dpw","4 to <7dpw","7dpw")) +
  scale_y_discrete(labels=c("7dpw","4 to <7dpw","2 to <4dpw","<2dpw")) +
  labs(x = "adherence DBS",y = "adherence EAM")+
  ggtitle("Contingency table doses per week \ndaily SMS-reminders DBS,EAM")

# calculate contingency table/confusion matrix between DBS/EAM for participants without SMS-reminders
conf_nosms <- confusionMatrix(data=dbs_no_sms$adherence_dbs,reference=dbs_no_sms$adherence_eam)
conf_nosms

plt_conf_nosms <- as.data.frame(conf_nosms$table)
plt_conf_nosms$Prediction <- factor(plt_conf_nosms$Prediction, levels=rev(levels(plt_conf_nosms$Prediction)))

# plot table
c_nosms<- plt_conf_nosms %>% 
  mutate(color = case_when(Prediction == Reference ~ 'green', 
                           Freq == 0 ~ 'white', 
                           TRUE ~ 'red')) %>%
  mutate(scaling = 2.5*(as.double(Freq)/nrow(dbs_sms)))%>%
  rowwise()%>%
  mutate(color=saturation(color,scaling)) %>%
  ggplot(aes(Prediction,Reference, fill= color)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_identity() + 
  scale_x_discrete(labels=c("<2dpw","2 to <4dpw","4 to <7dpw","7dpw")) +
  scale_y_discrete(labels=c("7dpw","4 to <7dpw","2 to <4dpw","<2dpw")) +
  labs(x = "adherence DBS",y = "adherence EAM")+
  ggtitle("Contingency table doses per week\nno daily SMS-reminders DBS,EAM")

cont_sms <- ggarrange(c_sms,c_nosms,nrow = 1)
ggsave(filename="cont_tables_dbs_eam_sms.pdf",plot = cont_sms,width=2,height=1, dpi =100,scale = 4.2)

################ Stacked bar plots with adherence in dpw for SMS/no SMS

#### DBS
adh_plot_dbs_sms <- ggplot(dbs_sms%>%filter(!is.na(adherence_dbs)),aes(x=month))+
  geom_bar(aes(fill = adherence_dbs),position = "fill")+
  theme_bw()+
  scale_fill_manual(
    values = c(
      "<2dpw" = "#d7191c",
      "2 to <4dpw" = "#fdae61",
      "4 to <7dpw" = "#a6d96a",
      "7dpw" = "#1a9641"
    ))+
  scale_y_continuous(name = NULL,labels = scales::percent_format(accuracy = 1),breaks = c(0,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))+
  guides(fill = guide_legend(title = "Adherence"))+
  ggtitle("A: TFVdp concentration \nwith SMS reminders")

adh_plot_dbs_no_sms <- ggplot(dbs_no_sms%>%filter(!is.na(adherence_dbs)),aes(x=month))+
  geom_bar(aes(fill = adherence_dbs),position = "fill")+
  theme_bw()+
  scale_fill_manual(
    values = c(
      "<2dpw" = "#d7191c",
      "2 to <4dpw" = "#fdae61",
      "4 to <7dpw" = "#a6d96a",
      "7dpw" = "#1a9641"
    ))+
  scale_y_continuous(name = NULL,labels = scales::percent_format(accuracy = 1),breaks = c(0,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))+
  guides(fill = guide_legend(title = "Adherence"))+
  ggtitle("B: TFVdp concentration \nwithout SMS reminders")

ggsave(file = "adh_plot_dbs_sms.pdf",plot = ggarrange(adh_plot_dbs_sms,adh_plot_dbs_no_sms,nrow=1,common.legend = TRUE,legend = "bottom"),width=2,height=1.6, dpi =100,scale = 3)

# EAM
adh_plot_eam_sms <- ggplot(dbs_sms%>%filter(!is.na(adherence_eam)),aes(x=month))+
  geom_bar(aes(fill = adherence_eam),position = "fill")+
  theme_bw()+
  scale_fill_manual(
    values = c(
      "<2dpw" = "#d7191c",
      "2 to <4dpw" = "#fdae61",
      "4 to <7dpw" = "#a6d96a",
      "7dpw" = "#1a9641"
    ))+
  scale_y_continuous(name = NULL,labels = scales::percent_format(accuracy = 1),breaks = c(0,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))+
  guides(fill = guide_legend(title = "Adherence"))+
  ggtitle("A: EAM adherence\nwith SMS reminders")

adh_plot_eam_no_sms <- ggplot(dbs_no_sms%>%filter(!is.na(adherence_eam)),aes(x=month))+
  geom_bar(aes(fill = adherence_eam),position = "fill")+
  theme_bw()+
  scale_fill_manual(
    values = c(
      "<2dpw" = "#d7191c",
      "2 to <4dpw" = "#fdae61",
      "4 to <7dpw" = "#a6d96a",
      "7dpw" = "#1a9641"
    ))+
  scale_y_continuous(name = NULL,labels = scales::percent_format(accuracy = 1),breaks = c(0,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))+
  guides(fill = guide_legend(title = "Adherence"))+
  ggtitle("B: EAM adherence\nwithout SMS reminders")

ggsave(file = "adh_plot_eam_sms.pdf",plot = ggarrange(adh_plot_eam_sms,adh_plot_eam_no_sms,nrow=1,common.legend = TRUE,legend = "bottom"),width=2,height=1.6, dpi =100,scale = 3)

########## Compare SMS/no SMS without dropouts
plots_sms_dropouts <- dbs_sms%>%
  filter(!is.na(eam) & !is.na(dbs))%>%
  group_by(ptid)%>%
  mutate(month = as.numeric(month))%>%
  mutate(stopped = ifelse(map_lgl(row_number(),~any(month>=month[.x] & (eam > 0 | dbs >100))),
                          "no", "yes"))%>%
  group_by(month,stopped)

plots_no_sms_dropouts <- dbs_no_sms%>%
  filter(!is.na(eam) & !is.na(dbs))%>%
  group_by(ptid)%>%
  mutate(month = as.numeric(month))%>%
  mutate(stopped = ifelse(map_lgl(row_number(),~any(month>=month[.x] & (eam > 0 | dbs >100))),
                          "no", "yes"))%>%
  group_by(month,stopped)

# calculate proportion of dropouts for each time point
plot_sms_dropouts_over_time <- plots_sms_dropouts %>%
  count()%>%
  group_by(month)%>%
  mutate(n = 100*n/sum(n))%>%
  filter(stopped=="yes")%>%
  ungroup()

plot_no_sms_dropouts_over_time <- plots_no_sms_dropouts %>%
  count()%>%
  group_by(month)%>%
  mutate(n = 100*n/sum(n))%>%
  filter(stopped=="yes")%>%
  ungroup()

data_plots_sms_dropouts <- rbind(plot_sms_dropouts_over_time%>%mutate(arm="SMS reminders"), plot_no_sms_dropouts_over_time%>%mutate(arm="no SMS reminders"))

# plot results
plots_sms_dropouts_over_time <- ggplot(data_plots_sms_dropouts,aes(x=month,y=n,group = arm,colour= arm))+
  geom_line()+
  guides(group = guide_legend(title = "Study arm"))+
  guides(colour = guide_legend(title = "Study arm"))+
  scale_x_continuous(name = "month",breaks=c(3,6,9,12,15,18,21,24))+
  ylab("Proportion of dropped out \nparticipants(%)")+
  ggtitle("Dropouts over time depending on study arm")

ggsave(filename="dropouts_sms.pdf",plot = plots_sms_dropouts_over_time,width=2,height=1, dpi =100,scale = 3)


# calculate concordance when dropouts are removed
# with daily SMS-reminders
conf_sms_dropouts <- confusionMatrix(data=(plots_sms_dropouts%>%filter(stopped=="no"))$adherence_dbs,reference=(plots_sms_dropouts%>%filter(stopped=="no"))$adherence_eam)
conf_sms_dropouts
# without daily SMS-reminders
conf_no_sms_dropouts <- confusionMatrix(data=(plots_no_sms_dropouts%>%filter(stopped=="no"))$adherence_dbs,reference=(plots_no_sms_dropouts%>%filter(stopped=="no"))$adherence_eam)
conf_no_sms_dropouts

# Test whether distribution of dropouts at the end is significant
test_sms <- dbs_sms%>%
  filter(!is.na(eam) & !is.na(dbs))%>%
  group_by(ptid)%>%
  mutate(month = as.numeric(month))%>%
  mutate(stopped = ifelse(map_lgl(row_number(),~any(month>=month[.x] & (eam > 0 | dbs >100))),
                          0, 1))%>%
  filter(month == 8)%>%
  group_by(stopped)%>%
  count()

test_no_sms <- dbs_no_sms%>%
  filter(!is.na(eam) & !is.na(dbs))%>%
  group_by(ptid)%>%
  mutate(month = as.numeric(month))%>%
  mutate(stopped = ifelse(map_lgl(row_number(),~any(month>=month[.x] & (eam > 0 | dbs >100))),
                          0, 1))%>%
  filter(month == 8)%>%
  group_by(stopped)%>%
  count()

fisher.test(rbind(c(56,52),c(65,49)), 
            alternative="less")

################ Doses per week for trajectories

#### DBS

# high DBS trajectory
dpw_dbs_high <- data_dbs_values %>%
  filter(group_dbs == "high")%>%
  drop_na(adherence_dbs)

dpw_plot_dbs_high <- ggplot(dpw_dbs_high,aes(x=month))+
  geom_bar(aes(fill = adherence_dbs),position = "fill")+
  theme_bw()+
  scale_fill_manual(
    values = c(
      "<2dpw" = "#d7191c",
      "2 to <4dpw" = "#fdae61",
      "4 to <7dpw" = "#a6d96a",
      "7dpw" = "#1a9641"
    ))+
  scale_y_continuous(name = NULL,labels = scales::percent_format(accuracy = 1),breaks = c(0,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))+
  guides(fill = guide_legend(title = "Adherence"))+
  ggtitle("A: Adherence \nhigh DBS-trajectory")

# declining DBS trajectory
dpw_dbs_declining <- data_dbs_values %>%
  filter(group_dbs == "declining")%>%
  drop_na(adherence_dbs)

dpw_plot_dbs_declining <- ggplot(dpw_dbs_declining,aes(x=month))+
  geom_bar(aes(fill = adherence_dbs),position = "fill")+
  theme_bw()+
  scale_fill_manual(
    values = c(
      "<2dpw" = "#d7191c",
      "2 to <4dpw" = "#fdae61",
      "4 to <7dpw" = "#a6d96a",
      "7dpw" = "#1a9641"
    ))+
  scale_y_continuous(name = NULL,labels = scales::percent_format(accuracy = 1),breaks = c(0,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))+
  guides(fill = guide_legend(title = "Adherence"))+
  ggtitle("A: Adherence \ndeclining DBS-trajectory")

dpw_dbs_low <- data_dbs_values %>%
  filter(group_dbs == "low")%>%
  drop_na(adherence_dbs)

# low DBS trajectory
dpw_plot_dbs_low <- ggplot(dpw_dbs_low,aes(x=month))+
  geom_bar(aes(fill = adherence_dbs),position = "fill", show.legend = TRUE)+
  theme_bw()+
  scale_fill_manual(
    #limits = c("<2dpw","2 to <4dpw","4 to <7dpw","7dpw"),
    values = c(
      "<2dpw" = "#d7191c",
      "2 to <4dpw" = "#fdae61",
      "4 to <7dpw" = "#a6d96a",
      "7dpw" = "#1a9641"
    ),
    drop = FALSE)+
  scale_y_continuous(name = NULL,labels = scales::percent_format(accuracy = 1),breaks = c(0,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))+
  guides(fill = guide_legend(title = "Adherence"))+
  ggtitle("A: Adherence \nlow DBS-trajectory")

dpw_dbs_trajectories <- ggarrange(dpw_plot_dbs_high,dpw_plot_dbs_declining,dpw_plot_dbs_low,nrow = 1,common.legend = TRUE, legend="bottom")
ggsave(filename="dpw_dbs_trajectories.pdf",plot = dpw_dbs_trajectories,width=3,height=1.6, dpi =100,scale = 2.5)

#### EAM
# low EAM trajectory
dpw_eam_high <- data_eam_values %>%
  filter(group_prep == "high")%>%
  drop_na(adherence_eam)

dpw_plot_eam_high <- ggplot(dpw_eam_high,aes(x=month))+
  geom_bar(aes(fill = adherence_eam),position = "fill", show.legend = TRUE)+
  theme_bw()+
  scale_fill_manual(
    values = c(
      "<2dpw" = "#d7191c",
      "2 to <4dpw" = "#fdae61",
      "4 to <7dpw" = "#a6d96a",
      "7dpw" = "#1a9641"
    ),
    drop=FALSE)+
  scale_y_continuous(name = NULL,labels = scales::percent_format(accuracy = 1),breaks = c(0,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))+
  guides(fill = guide_legend(title = "Adherence"))+
  ggtitle("A: Adherence\nhigh EAM-trajectory")

# declining EAM trajectory
dpw_eam_declining <- data_eam_values %>%
  filter(group_prep == "declining")%>%
  drop_na(adherence_eam)

dpw_plot_eam_declining <- ggplot(dpw_eam_declining,aes(x=month))+
  geom_bar(aes(fill = adherence_eam),position = "fill", show.legend = TRUE)+
  theme_bw()+
  scale_fill_manual(
    values = c(
      "<2dpw" = "#d7191c",
      "2 to <4dpw" = "#fdae61",
      "4 to <7dpw" = "#a6d96a",
      "7dpw" = "#1a9641"
    ),
    drop=FALSE)+
  scale_y_continuous(name = NULL,labels = scales::percent_format(accuracy = 1),breaks = c(0,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))+
  guides(fill = guide_legend(title = "Adherence"))+
  ggtitle("B: Adherence\ndeclining EAM-trajectory")

# low EAM trajectory
dpw_eam_low <- data_eam_values %>%
  filter(group_prep == "low")%>%
  drop_na(adherence_eam)

dpw_plot_eam_low <- ggplot(dpw_eam_low,aes(x=month))+
  geom_bar(aes(fill = adherence_eam),position = "fill", show.legend = TRUE)+
  theme_bw()+
  scale_fill_manual(
    values = c(
      "<2dpw" = "#d7191c",
      "2 to <4dpw" = "#fdae61",
      "4 to <7dpw" = "#a6d96a",
      "7dpw" = "#1a9641"
    ),
    drop=FALSE)+
  scale_y_continuous(name = NULL,labels = scales::percent_format(accuracy = 1),breaks = c(0,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))+
  guides(fill = guide_legend(title = "Adherence"))+
  ggtitle("C: Adherence\nlow EAM-trajectory")

dpw_eam_trajectories <- ggarrange(dpw_plot_eam_high,dpw_plot_eam_declining,dpw_plot_eam_low,nrow = 1,common.legend = TRUE, legend="bottom")
ggsave(filename="dpw_eam_trajectories.pdf",plot = dpw_eam_trajectories,width=3,height=1.6, dpi =100,scale = 2.5)

#### Pharmacy refills
adherence_data <- read.csv("adherence_values.csv")
dpw_pharm <- merge(adherence_data,groups_pharm%>%select(ptid,group_pharm),by="ptid") %>%
  mutate(month=factor(visitcode,levels = c("3","6","9","12","15","18","21","24")))%>%
  mutate(adherence_pharm_adh = case_when(is.na(pharm_adh) ~ NA,
                                         pharm_adh < 2 * (100/7)~"<2dpw",
                                         pharm_adh >= 2 * (100/7) & pharm_adh < 4 * (100/7) ~ "2 to <4dpw",
                                         pharm_adh >= 4 * (100/7) & pharm_adh < 7 * (100/7) ~ "4 to <7dpw",
                                         TRUE ~ "7dpw"))%>%
  drop_na(adherence_pharm_adh)%>%
  mutate(adherence_pharm_adh = factor(adherence_pharm_adh,levels = c("7dpw","4 to <7dpw","2 to <4dpw","<2dpw")))

# high pharmacy refill trajectory
dpw_plot_pharm_high <- ggplot(dpw_pharm%>%filter(group_pharm=="high"),aes(x=month))+
  geom_bar(aes(fill = adherence_pharm_adh),position = "fill",show.legend = TRUE)+
  theme_bw()+
  scale_fill_manual(
    values = c(
      "<2dpw" = "#d7191c",
      "2 to <4dpw" = "#fdae61",
      "4 to <7dpw" = "#a6d96a",
      "7dpw" = "#1a9641"
    ))+
  scale_y_continuous(name = NULL,labels = scales::percent_format(accuracy = 1),breaks = c(0,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))+
  guides(fill = guide_legend(title = "Adherence"))+
  ggtitle("A: Adherence Pharmacy refills\nhigh trajectory")

# declining pharmacy refill trajectory
dpw_plot_pharm_declining <- ggplot(dpw_pharm%>%filter(group_pharm=="declining"),aes(x=month))+
  geom_bar(aes(fill = adherence_pharm_adh),position = "fill",show.legend = TRUE)+
  theme_bw()+
  scale_fill_manual(
    values = c(
      "<2dpw" = "#d7191c",
      "2 to <4dpw" = "#fdae61",
      "4 to <7dpw" = "#a6d96a",
      "7dpw" = "#1a9641"
    ))+
  scale_y_continuous(name = NULL,labels = scales::percent_format(accuracy = 1),breaks = c(0,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))+
  guides(fill = guide_legend(title = "Adherence"))+
  ggtitle("B: Adherence Pharmacy refills\ndeclining trajectory")

# low pharmacy refill trajectory
dpw_plot_pharm_low <- ggplot(dpw_pharm%>%filter(group_pharm=="low"),aes(x=month))+
  geom_bar(aes(fill = adherence_pharm_adh),position = "fill",show.legend = TRUE)+
  theme_bw()+
  scale_fill_manual(
    values = c(
      "<2dpw" = "#d7191c",
      "2 to <4dpw" = "#fdae61",
      "4 to <7dpw" = "#a6d96a",
      "7dpw" = "#1a9641"
    ))+
  scale_y_continuous(name = NULL,labels = scales::percent_format(accuracy = 1),breaks = c(0,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))+
  guides(fill = guide_legend(title = "Adherence"))+
  ggtitle("C: Adherence Pharmacy refills\nlow trajectory")

dpw_pharm_trajectories <- ggarrange(dpw_plot_pharm_high,dpw_plot_pharm_declining,dpw_plot_pharm_low,nrow = 1,common.legend = TRUE, legend="bottom")
ggsave(filename="dpw_pharm_trajectories.pdf",plot = dpw_pharm_trajectories,width=3,height=1.6, dpi =100,scale = 2.5)

################ Spaghetti plots for individual trajectories

#### DBS
# for high DBS trajectory
dbs_spag_high <- merge(adherence_data,groups_dbs,by = "ptid")%>%filter(group_dbs=="high")

line_dbs_high <- ggplot()+
  geom_line(data=dbs_spag_high[!is.na(dbs_spag_high$dbs),],aes(y=dbs,x = as.integer(visitcode),group=ptid), alpha=0.35)+
  ylim(100,3000)+
  scale_x_continuous(breaks=c(3,6,9,12,15,18,21,24))+
  ggtitle("B: High TVFdp trajectory")+
  xlab(NULL)+
  ylab(NULL)

# for declining DBS trajectory
dbs_spag_declining <- merge(adherence_data,groups_dbs,by = "ptid")%>%filter(group_dbs=="declining")

line_dbs_declining <- ggplot()+
  geom_line(data=dbs_spag_declining[!is.na(dbs_spag_declining$dbs),],aes(y=dbs,x = as.integer(visitcode),group=ptid), alpha=0.55)+
  ylim(100,3000)+
  scale_x_continuous(breaks=c(3,6,9,12,15,18,21,24))+
  ggtitle("C: Declining TVFdp trajectory")+
  xlab("month")+
  ylab("TVFdp concentration \n(mmol/punch)")

# for low DBS trajectory
dbs_spag_low <- merge(adherence_data,groups_dbs,by = "ptid")%>%filter(group_dbs=="low")

line_dbs_low <- ggplot()+
  geom_line(data=dbs_spag_low[!is.na(dbs_spag_low$dbs),],aes(y=dbs,x = as.integer(visitcode),group=ptid), alpha=0.55)+
  ylim(100,3000)+
  scale_x_continuous(breaks=c(3,6,9,12,15,18,21,24))+
  ggtitle("D: Low TVFdp trajectory")+
  xlab("month")+
  ylab(NULL)

# DBS trajectory model from stata

dbs_trajectories <- magick::image_read_pdf("dbs_adherence_ci.pdf")
g <- grid::rasterGrob(dbs_trajectories, interpolate=TRUE)

img_trajectories <- ggplot() +
  theme_minimal()+
  ggtitle("        A: TFCdp trajectories")+
  ylab("TVFdp concentration \n(mmol/punch)")+
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        plot.margin = unit(c(0.2,0,0,0),"cm"))

dbs_traj_spag <- ggarrange(img_trajectories,line_dbs_high,line_dbs_declining,line_dbs_low,nrow=2,ncol=2,align = "none")
ggsave(file = "dbs_traj_spag.pdf", plot = dbs_traj_spag,width=3,height=1.6, dpi =100,scale = 3)

#### EAM adherence
# high EAM trajectory
eam_spag_high <- merge(adherence_data,groups_prep,by = "ptid")%>%filter(group_prep=="high")

line_eam_high <- ggplot()+
  geom_line(data=eam_spag_high[!is.na(eam_spag_high$prep_adh),],aes(y=prep_adh,x = as.integer(visitcode),group=ptid), alpha=0.35)+
  ylim(0,100)+
  scale_x_continuous(breaks=c(3,6,9,12,15,18,21,24))+
  ggtitle("B: High EAM adherence trajectory")+
  xlab(NULL)+
  ylab(NULL)

# declining EAM trajectory
eam_spag_declining <- merge(adherence_data,groups_prep,by = "ptid")%>%filter(group_prep=="declining")

line_eam_declining <- ggplot()+
  geom_line(data=eam_spag_declining[!is.na(eam_spag_declining$prep_adh),],aes(y=prep_adh,x = as.integer(visitcode),group=ptid), alpha=0.55)+
  ylim(0,100)+
  scale_x_continuous(breaks=c(3,6,9,12,15,18,21,24))+
  ggtitle("C: Declining EAM adherence trajectory")+
  xlab("month")+
  ylab("EAM adherence (%)")

# low EAM trajectory
eam_spag_low <- merge(adherence_data,groups_prep,by = "ptid")%>%filter(group_prep=="low")

line_eam_low <- ggplot()+
  geom_line(data=eam_spag_low[!is.na(eam_spag_low$prep_adh),],aes(y=prep_adh,x = as.integer(visitcode),group=ptid), alpha=0.55)+
  ylim(0,100)+
  scale_x_continuous(breaks=c(3,6,9,12,15,18,21,24))+
  ggtitle("D: Low EAM adherence trajectory")+
  xlab("month")+
  ylab(NULL)

eam_trajectories <- magick::image_read_pdf("eam_adherence_ci.pdf")
g_eam <- grid::rasterGrob(eam_trajectories, interpolate=TRUE)

# EAM trajectory model from stata
img_trajectories_eam <- ggplot() +
  theme_minimal()+
  ggtitle("        A: EAM adherence trajectories")+
  ylab("EAM adherence (%)")+
  annotation_custom(g_eam, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        plot.margin = unit(c(0.2,0,0,0),"cm"))

eam_traj_spag <- ggarrange(img_trajectories_eam,line_eam_high,line_eam_declining,line_eam_low,nrow=2,ncol=2,align = "none")
ggsave(file = "eam_traj_spag.pdf", plot = eam_traj_spag,width=3,height=1.6, dpi =100,scale = 3)

#### Pharmacy refills
# high pharmacy refill trajectory
pharm_spag_high <- merge(adherence_data,groups_pharm,by = "ptid")%>%filter(group_pharm=="high")

line_pharm_high <- ggplot()+
  geom_line(data=pharm_spag_high[!is.na(pharm_spag_high$pharm_adh),],aes(y=pharm_adh,x = as.integer(visitcode),group=ptid), alpha=0.35)+
  ylim(0,100)+
  scale_x_continuous(breaks=c(3,6,9,12,15,18,21,24))+
  ggtitle("B: High pharmacy refill trajectory")+
  xlab(NULL)+
  ylab(NULL)

# declining pharmacy refill trajectory
pharm_spag_declining <- merge(adherence_data,groups_pharm,by = "ptid")%>%filter(group_pharm=="declining")

line_pharm_declining <- ggplot()+
  geom_line(data=pharm_spag_declining[!is.na(pharm_spag_declining$pharm_adh),],aes(y=pharm_adh,x = as.integer(visitcode),group=ptid), alpha=0.55)+
  ylim(0,100)+
  scale_x_continuous(breaks=c(3,6,9,12,15,18,21,24))+
  ggtitle("C: Declining pharmacy refill trajectory")+
  xlab("month")+
  ylab("Pharmacy refills (%)")

# low pharmacy refill trajectory
pharm_spag_low <- merge(adherence_data,groups_pharm,by = "ptid")%>%filter(group_pharm=="low")

line_pharm_low <- ggplot()+
  geom_line(data=pharm_spag_low[!is.na(pharm_spag_low$pharm_adh),],aes(y=pharm_adh,x = as.integer(visitcode),group=ptid), alpha=0.55)+
  ylim(0,100)+
  scale_x_continuous(breaks=c(3,6,9,12,15,18,21,24))+
  ggtitle("D: Low pharmacy refill trajectory")+
  xlab("month")+
  ylab(NULL)

pharm_trajectories <- magick::image_read_pdf("pharm_adherence_ci.pdf")
g_pharm <- grid::rasterGrob(pharm_trajectories, interpolate=TRUE)

# pharmacy refill trajectory model from stata
img_trajectories_pharm <- ggplot() +
  theme_minimal()+
  ggtitle("        A: Pharmacy refills trajectories")+
  ylab("Pharmacy refills (%)")+
  annotation_custom(g_pharm, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        plot.margin = unit(c(0.2,0,0,0),"cm"))

pharm_traj_spag <- ggarrange(img_trajectories_pharm,line_pharm_high,line_pharm_declining,line_pharm_low,nrow=2,ncol=2,align = "none")
ggsave(file = "pharm_traj_spag.pdf", plot = pharm_traj_spag,width=3,height=1.6, dpi =100,scale = 3)
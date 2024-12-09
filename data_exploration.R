# Data Exploration
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(purrr)

# Load in questionnaire data for population analysis

# Population
questionnaire_data <- read_excel("questionnaire.xlsx") %>%
  select(ptid,visitcode,site,arm,scrnage_cat,eduyrs,demojob,scrnsex,scrnsd,demomarry,scrnpartner,scrnsupport,scrnalcohol,scrnscore,totpart,unsafe_sex)%>%
  filter(visitcode == 1000)%>%
  group_by(ptid)

# summary of float variables
summary(questionnaire_data%>%ungroup()%>%select(eduyrs, scrnscore,totpart))

# by site
questionnaire_data%>%
  group_by(site)%>%
  count()

# by study arm
questionnaire_data%>%
  group_by(arm)%>%
  count()

# by age
questionnaire_data%>%
  group_by(scrnage_cat)%>%
  count()

# by job
questionnaire_data%>%
  group_by(demojob)%>%
  count()

# by scrnsex
questionnaire_data%>%
  group_by(scrnsex)%>%
  count()

# by scrnsd
questionnaire_data%>%
  group_by(scrnsd)%>%
  count()

# by scrnpartner
questionnaire_data%>%
  group_by(demomarry)%>%
  count()

# by scrnsupport
questionnaire_data%>%
  group_by(scrnsupport)%>%
  count()

# by totpart
questionnaire_data%>%
  group_by(totpart)%>%
  count()

# by unsafe_sex
questionnaire_data%>%
  group_by(unsafe_sex)%>%
  count()

# load in adherence data
adherence_data <- read.csv("adherence_values.csv")

# entries
nrow(adherence_data)

# summary adherence
summary(adherence_data)

# count amount of entries per data source and amount of extreme (min/max) values when relevant
nrow(adherence_data%>%group_by(dbs)%>%filter(!is.na(dbs)))
nrow(adherence_data%>%group_by(dbs)%>%filter(dbs==100))

nrow(adherence_data%>%group_by(prep_adh)%>%filter(!is.na(prep_adh)))
nrow(adherence_data%>%group_by(prep_adh)%>%filter(prep_adh==0))

nrow(adherence_data%>%group_by(pharm_adh)%>%filter(!is.na(pharm_adh)))
nrow(adherence_data%>%group_by(pharm_adh)%>%filter(pharm_adh==0))
nrow(adherence_data%>%group_by(pharm_adh)%>%filter(pharm_adh==100))

################ Box plots for adherence

# TFVdp concentration
box_dbs <- ggplot(adherence_data,aes(y=dbs))+
  geom_boxplot(outlier.colour="black", outlier.shape=16,
             outlier.size=2, notch=FALSE)+
  scale_y_log10()+
  ggtitle("A: TVFdp\nconc.")+
  ylab("TVFdp concentration (mmol/punch)")+
  theme(axis.text.x = element_blank(),axis.ticks = element_blank())

# EAM adherence
box_eam <- ggplot(adherence_data,aes(y=prep_adh))+
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE)+
  ggtitle("B: EAM\nAdherence")+
  ylab("EAM Adherence (%)")+
  theme(axis.text.x = element_blank(),axis.ticks = element_blank())

# Pharmacy refill adherence
box_pharm <- ggplot(adherence_data,aes(y=pharm_adh))+
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE)+
  scale_y_log10()+
  ggtitle("C: Pharmacy\nrefills")+
  ylab("Pharmacy refills (%)")+
theme(axis.text.x = element_blank(),axis.ticks = element_blank())

boxplot <- ggarrange(box_dbs,box_eam,box_pharm,nrow = 1)
ggsave(filename="boxplot_adherence.pdf",plot = boxplot,width=3.4,height=2, dpi = 100,scale = 1.5)

################ ECDF plots for data sources

# TFVdp concentration
ecdf_dbs <- ggplot(adherence_data,aes(dbs))+
  stat_ecdf(geom = "step")+
  ggtitle("A: ECDF TVFdp concentration")+
  xlab("TVFdp concentration (mmol/punch)")+
  ylab("Fraction of Data")+
  scale_x_continuous(limits = c(0,3050),breaks = c(0,500,1000,1500,2000,2500,3000))

# EAM adherence
ecdf_eam <- ggplot(adherence_data,aes(prep_adh))+
  stat_ecdf(geom = "step")+
  ggtitle("B: ECDF  EAM Adherence")+
  xlab("EAM Adherence (%)")+
  ylab("Fraction of Data")

# Pharmacy refill adherence
ecdf_pharm <- ggplot(adherence_data,aes(pharm_adh))+
  stat_ecdf(geom = "step")+
  ggtitle("C: ECDF Pharmacy refills")+
  xlab("Pharmacy refills (%)")+
  ylab("Fraction of Data")

ecdf <- ggarrange(ecdf_dbs,ecdf_eam,ecdf_pharm,nrow = 1)
ggsave(filename="ecdf_adherence.pdf",plot = ecdf,width=3,height=1, dpi = 100,scale = 3)

################ Spaghetti plots for data sources

# TFVdp concentration
line_dbs <- ggplot(adherence_data)+
  geom_line(data=adherence_data[!is.na(adherence_data$dbs),],aes(y=dbs,x = as.integer(visitcode),group=ptid), alpha=0.35)+
  geom_smooth(aes(y=dbs,x = as.integer(visitcode)),formula = y ~ s(x, bs = "cs", k = 8))+
  scale_y_log10()+
  scale_x_continuous(breaks=c(3,6,9,12,15,18,21,24))+
  ggtitle("A: TVFdp conc.")+
  xlab("month")+
  ylab("TVFdp concentration (mmol/punch)")

# EAM adherence
line_eam <- ggplot(adherence_data)+
  geom_line(data=adherence_data[!is.na(adherence_data$prep_adh),],aes(y=prep_adh,x = as.integer(visitcode),group=ptid), alpha=0.35)+
  geom_smooth(aes(y=prep_adh,x = as.integer(visitcode)),formula = y ~ s(x, bs = "cs", k = 8))+
  scale_x_continuous(breaks=c(3,6,9,12,15,18,21,24))+
  ggtitle("B: EAM Adherence")+
  xlab("month")+
  ylab("EAM Adherence (%)")

# Pharmacy refill adherence
line_pharm <- ggplot(adherence_data)+
  geom_line(data=adherence_data[!is.na(adherence_data$pharm_adh),],aes(y=pharm_adh,x = as.integer(visitcode),group=ptid), alpha=0.35)+
  geom_smooth(aes(y=pharm_adh,x = as.integer(visitcode)),formula = y ~ s(x, bs = "cs", k = 8))+
  scale_x_continuous(breaks=c(3,6,9,12,15,18,21,24))+
  ggtitle("C: Pharmacy refills")+
  xlab("month")+
  ylab("Pharmacy refills (%)")

spaghetti <- ggarrange(line_dbs,line_eam,line_pharm,nrow = 1)
ggsave(filename="spaghettiplot_adherence.pdf",plot = spaghetti,width=3,height=1, dpi =300,scale = 2.75)

################ Stacked bar plots with adherence in dpw for data sources

# TFVdp concentration
# convert into dpw
dpw_dbs <- adherence_data %>%
  select(ptid,visitcode,dbs)%>%
  mutate(month=factor(visitcode,levels = c("3","6","9","12","15","18","21","24")))%>%
  mutate(adherence_dbs = case_when(is.na(dbs) ~ NA,
                                   dbs < 350~"<2dpw",
                                   dbs >= 350 & dbs < 700 ~ "2 to <4dpw",
                                   dbs >= 700 & dbs < 1250 ~ "4 to <7dpw",
                                   TRUE ~ "7dpw"))%>%
  drop_na(adherence_dbs)%>%
  mutate(adherence_dbs = factor(adherence_dbs,levels = c("7dpw","4 to <7dpw","2 to <4dpw","<2dpw")))

# create plot
dpw_plot_dbs <- ggplot(dpw_dbs,aes(x=month))+
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
  ggtitle("A: Adherence DBS")

# EAM adherence
# convert into dpw
dpw_eam <- adherence_data %>%
  select(ptid,visitcode,prep_adh)%>%
  mutate(month=factor(visitcode,levels = c("3","6","9","12","15","18","21","24")))%>%
  mutate(adherence_prep_adh = case_when(is.na(prep_adh) ~ NA,
                                   prep_adh < 2 * (100/7)~"<2dpw",
                                   prep_adh >= 2 * (100/7) & prep_adh < 4 * (100/7) ~ "2 to <4dpw",
                                   prep_adh >= 4 * (100/7) & prep_adh < 7 * (100/7) ~ "4 to <7dpw",
                                   TRUE ~ "7dpw"))%>%
  drop_na(adherence_prep_adh)%>%
  mutate(adherence_prep_adh = factor(adherence_prep_adh,levels = c("7dpw","4 to <7dpw","2 to <4dpw","<2dpw")))

# create plot
dpw_plot_eam <- ggplot(dpw_eam,aes(x=month))+
  geom_bar(aes(fill = adherence_prep_adh),position = "fill")+
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
  ggtitle("B: Adherence EAM")

# Pharmacy refill adherence
# convert into dpw
dpw_pharm <- adherence_data %>%
  select(ptid,visitcode,pharm_adh)%>%
  mutate(month=factor(visitcode,levels = c("3","6","9","12","15","18","21","24")))%>%
  mutate(adherence_pharm_adh = case_when(is.na(pharm_adh) ~ NA,
                                        pharm_adh < 2 * (100/7)~"<2dpw",
                                        pharm_adh >= 2 * (100/7) & pharm_adh < 4 * (100/7) ~ "2 to <4dpw",
                                        pharm_adh >= 4 * (100/7) & pharm_adh < 7 * (100/7) ~ "4 to <7dpw",
                                        TRUE ~ "7dpw"))%>%
  drop_na(adherence_pharm_adh)%>%
  mutate(adherence_pharm_adh = factor(adherence_pharm_adh,levels = c("7dpw","4 to <7dpw","2 to <4dpw","<2dpw")))

# create plot
dpw_plot_pharm <- ggplot(dpw_pharm,aes(x=month))+
  geom_bar(aes(fill = adherence_pharm_adh),position = "fill")+
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
  ggtitle("C: Adherence Pharmacy refills")

dpw_data_sources <- ggarrange(dpw_plot_dbs,dpw_plot_eam,dpw_plot_pharm,nrow = 1,common.legend = TRUE, legend="bottom")
ggsave(filename="dpw_data_sources.pdf",plot = dpw_data_sources,width=3,height=1.6, dpi =100,scale = 3)

################ Scatterplots for correlation between data sources

# TFVdp concentration and EAM adherence
scatter_dbs_eam <- ggscatter(data=adherence_data%>%filter(!is.na(dbs)&!is.na(prep_adh)),x='prep_adh',y='dbs',
                             alpha=0.5,
                             add = "reg.line",
                             conf.int = TRUE,
                             add.params = list(color = "blue", fill = "lightgray"))+
  ylab("TVFdp concentration (mmol/punch)")+
  xlab("EAM Adherence (%)")+
  ggtitle("A: Association TVFdp conc. and\nEAM adherence")+
  scale_y_log10()

# TFVdp concentration and Pharmacy refill adherence
scatter_dbs_pharm <- ggscatter(data=adherence_data%>%filter(!is.na(dbs)&!is.na(pharm_adh)),x='pharm_adh',y='dbs',
                             alpha=0.5,
                             add = "reg.line",
                             conf.int = TRUE,
                             add.params = list(color = "blue", fill = "lightgray"))+
  ylab("TVFdp concentration (mmol/punch)")+
  xlab("Pharmacy Refills (%)")+
  ggtitle("B: Association TVFdp conc. and\nPharmacy Refills")+
  scale_y_log10()

# EAM adherence and Pharmacy refill adherence
scatter_prep_pharm <- ggscatter(data=adherence_data%>%filter(!is.na(prep_adh)&!is.na(pharm_adh)),x='pharm_adh',y='prep_adh',
                               alpha=0.5,
                               add = "reg.line",
                               conf.int = TRUE,
                               add.params = list(color = "blue", fill = "lightgray"))+
  ylab("EAM adherence (%)")+
  xlab("Pharmacy Refills (%)")+
  ggtitle("C: Association EAM adherence\nand Pharmacy Refills")

scatter_data_sources <- ggarrange(scatter_dbs_eam,scatter_dbs_pharm,scatter_prep_pharm,nrow = 1)
ggsave(filename="scatter_data_sources.pdf",plot = scatter_data_sources,width=3,height=1, dpi =100,scale = 3.5)

################ Proportion of dropped out individuals over time

# calculate dropouts
plot_dropouts <- adherence_data%>%
  filter(!is.na(prep_adh) & !is.na(dbs))%>%
  group_by(ptid)%>%
  mutate(visitcode = as.numeric(visitcode))%>%
  mutate(stopped = ifelse(map_lgl(row_number(),~any(visitcode>=visitcode[.x] & (prep_adh > 0 | dbs >100))),
                          "no", "yes"))%>%
  group_by(visitcode,stopped)%>%
  count()%>%
  group_by(visitcode)%>%
  mutate(n = 100*n/sum(n))%>%
  filter(stopped=="yes")%>%
  mutate(stopped = "Dropped out")%>%
  ungroup()

# create plot
dropouts <- ggplot(plot_dropouts,aes(x=visitcode,y=n,group = stopped,colour= stopped))+
  geom_line()+
  guides(group = guide_legend(title = ""))+
  guides(colour = guide_legend(title = ""))+
  scale_x_continuous(name = "month",breaks=c(3,6,9,12,15,18,21,24))+
  ylab("Proportion of dropped out\nparticipants(%)")+
  ggtitle("Proportion of dropped out participants over time")

ggsave(filename="dropouts.pdf",plot = dropouts,width=2,height=1, dpi =100,scale = 3)
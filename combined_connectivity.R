library(ggplot2)
library(dplyr)
library(tidyr)
library(ez)
library(tidyverse)
library(stringr)
library(magrittr)

# cfl_data  <- read_csv('cfl_coh_connectivity.csv')
# cfl_data$Group = as.factor(cfl_data$Group)
# cfl_data$Site = as.factor(cfl_data$Site)
# cfl_data$Subject = as.factor(cfl_data$Subject)
# cfl_data$Band = as.factor(cfl_data$Band)
# cfl_data$Condition = as.factor(cfl_data$Condition)
# cfl_data$Connectivity = as.factor(cfl_data$Connectivity)
# cfl_data$Age = 'CFL'
# 
# cfl_data <- cfl_data %>% select(-X1)
# 
# cfl_data[cfl_data$Connectivity != 'Interhemi',] %<>% 
#   mutate(Electrode = str_sub(Electrode,1, -3))
# 
# cfl_data$Electrode = as.factor(cfl_data$Electrode)
# 
# b2p_data  <- read_csv('b2p_coh_connectivity.csv')
# b2p_data$Group = as.factor(b2p_data$Group)
# b2p_data$Site = as.factor(b2p_data$Site)
# b2p_data$Subject <- paste("B2P", b2p_data$Subject, sep="_")
# b2p_data$Subject = as.factor(b2p_data$Subject)
# b2p_data$Band = as.factor(b2p_data$Band)
# b2p_data$Condition = as.factor(b2p_data$Condition)
# b2p_data$Connectivity = as.factor(b2p_data$Connectivity)
# b2p_data$Age = 'B2P'
# 
# b2p_data <- b2p_data %>% select(-X1)
# 
# b2p_data[b2p_data$Connectivity != 'Interhemi',] %<>% 
#   mutate(Electrode = str_sub(Electrode,1, -3))
# 
# b2p_data$Electrode = as.factor(b2p_data$Electrode)
# 
# 
# combined_data_coh <- bind_rows(cfl_data, b2p_data)
# combined_data_coh$Age = as.factor(combined_data_coh$Age)
# 
# combined_data_coh %<>%  mutate(Group = factor(Group, labels=c('conc', 'cont')))
# combined_data_coh %<>% rename(COH = WPLI)
# 
# cfl_data  <- read_csv('cfl_wpli_connectivity.csv')
# cfl_data$Group = as.factor(cfl_data$Group)
# cfl_data$Site = as.factor(cfl_data$Site)
# cfl_data$Subject = as.factor(cfl_data$Subject)
# cfl_data$Band = as.factor(cfl_data$Band)
# cfl_data$Condition = as.factor(cfl_data$Condition)
# cfl_data$Connectivity = as.factor(cfl_data$Connectivity)
# cfl_data$Age = 'CFL'
# 
# cfl_data <- cfl_data %>% select(-X1)
# 
# cfl_data[cfl_data$Connectivity != 'Interhemi',] %<>% 
#   mutate(Electrode = str_sub(Electrode,1, -3))
# 
# cfl_data$Electrode = as.factor(cfl_data$Electrode)
# 
# b2p_data  <- read_csv('b2p_wpli_connectivity.csv')
# b2p_data$Group = as.factor(b2p_data$Group)
# b2p_data$Site = as.factor(b2p_data$Site)
# b2p_data$Subject <- paste("B2P", b2p_data$Subject, sep="_")
# b2p_data$Subject = as.factor(b2p_data$Subject)
# b2p_data$Band = as.factor(b2p_data$Band)
# b2p_data$Condition = as.factor(b2p_data$Condition)
# b2p_data$Connectivity = as.factor(b2p_data$Connectivity)
# b2p_data$Age = 'B2P'
# 
# b2p_data <- b2p_data %>% select(-X1)
# 
# b2p_data[b2p_data$Connectivity != 'Interhemi',] %<>% 
#   mutate(Electrode = str_sub(Electrode,1, -3))
# 
# b2p_data$Electrode = as.factor(b2p_data$Electrode)
# 
# 
# combined_data_wpli <- bind_rows(cfl_data, b2p_data)
# combined_data_wpli$Age = as.factor(combined_data$Age)
# 
# combined_data_wpli %<>%  mutate(Group = factor(Group, labels=c('conc', 'cont')))
# 
# combined_data <- combined_data_coh %>% inner_join(combined_data_wpli)
# 
# combined_data %>% write.csv('combined_data.csv')

combined_data <- read.csv('combined_data.csv')

ggplot(combined_data %>% filter(Condition == 'Frequency Deviant'), aes(x=Site, y=WPLI, fill=Age)) + geom_boxplot(notch = FALSE) + facet_grid(Band ~ Group)


################################################################
## COHERENCE 
################################################################
type = '_coh'
#### Bands
band = 'delta'

con = 'Intrahemi-long'
data <- combined_data %>%  filter(Connectivity == con, Band == band, Site == 'Right' | Site == 'Left') %>% droplevels()
model = ezANOVA(data, dv = COH, wid = Subject, within = c(Condition, Site), between = c(Group, Age), type = 3)
ggplot(data, aes(x=Age, y=COH, fill=Group)) + geom_boxplot(notch = FALSE)
data %>% 
  group_by(Age, Group) %>% 
  summarise(mean_COH = ci(COH)[1], lci = ci(COH)[2], hci = ci(COH)[3]) %>% 
  ggplot(aes(x = Age, y = mean_COH, color = Group)) +
  geom_line(aes(group = Group)) +
  geom_point() +
  geom_errorbar(aes(ymax=hci, ymin=lci), width=.1) 
  

apa.ezANOVA.table(model, correction = "GG",filename = paste(band, '_', con, type,'.doc', sep = ''))

con = 'Intrahemi-mid'
data <- combined_data %>%  filter(Connectivity == con, Band == band, Site == 'FT-R' | Site == 'FT-L' | Site == 'TP-R' | Site == 'TP-L') %>% droplevels()
model = ezANOVA(data, dv = COH, wid = Subject, within = c(Condition, Site), between = c(Group, Age), type = 3)
ggplot(data, aes(x=Age, y=COH, fill=Group)) + geom_boxplot(notch = FALSE)
data %>% 
  group_by(Age, Group) %>% 
  summarise(mean_COH = ci(COH)[1], lci = ci(COH)[2], hci = ci(COH)[3]) %>% 
  ggplot(aes(x = Age, y = mean_COH, color = Group)) +
  geom_line(aes(group = Group)) +
  geom_point() +
  geom_errorbar(aes(ymax=hci, ymin=lci), width=.1) 
apa.ezANOVA.table(model, correction = "GG",filename = paste(band, '_', con, type,'.doc', sep = ''))

con = 'Intrahemi-within'
data <- combined_data %>%  filter(Connectivity == con, Band == band, Site == 'R-F' | Site == 'L-F' |  Site == 'R-T' | Site == 'L-T' | Site == 'R-P' | Site == 'L-P') %>% droplevels()
model = ezANOVA(data, dv = COH, wid = Subject, within = c(Condition, Site), between = c(Group, Age), type = 3)
ggplot(data, aes(x=Age, y=COH, fill=Group)) + geom_boxplot(notch = FALSE)
data %>% 
  group_by(Age, Group) %>% 
  summarise(mean_COH = ci(COH)[1], lci = ci(COH)[2], hci = ci(COH)[3]) %>% 
  ggplot(aes(x = Age, y = mean_COH, color = Group)) +
  geom_line(aes(group = Group)) +
  geom_point() +
  geom_errorbar(aes(ymax=hci, ymin=lci), width=.1) 
apa.ezANOVA.table(model, correction = "GG",filename = paste(band, '_', con, type,'.doc', sep = ''))

con = 'Interhemi'
data <- combined_data %>%  filter(Connectivity == con, Band == band, Site == 'FR' | Site == 'TP' |  Site == 'PR') %>% droplevels()
model = ezANOVA(data, dv = COH, wid = Subject, within = c(Condition, Site), between = c(Group, Age), type = 3)
ggplot(data, aes(x=Age, y=COH, fill=Group)) + geom_boxplot(notch = FALSE)
data %>% 
  group_by(Age, Group) %>% 
  summarise(mean_COH = ci(COH)[1], lci = ci(COH)[2], hci = ci(COH)[3]) %>% 
  ggplot(aes(x = Age, y = mean_COH, color = Group)) +
  geom_line(aes(group = Group)) +
  geom_point() +
  geom_errorbar(aes(ymax=hci, ymin=lci), width=.1) 
apa.ezANOVA.table(model, correction = "GG",filename = paste(band, '_', con, type,'.doc', sep = ''))


band = 'theta'
con = 'Intrahemi-long'
data <- combined_data %>%  filter(Connectivity == con, Band == band, Site == 'Right' | Site == 'Left') %>% droplevels()
model = ezANOVA(data, dv = COH, wid = Subject, within = c(Condition, Site), between = c(Group, Age), type = 3)
ggplot(data, aes(x=Age, y=COH, fill=Group)) + geom_boxplot(notch = FALSE)
data %>% 
  group_by(Age, Group) %>% 
  summarise(mean_COH = ci(COH)[1], lci = ci(COH)[2], hci = ci(COH)[3]) %>% 
  ggplot(aes(x = Age, y = mean_COH, color = Group)) +
  geom_line(aes(group = Group)) +
  geom_point() +
  geom_errorbar(aes(ymax=hci, ymin=lci), width=.1) 


apa.ezANOVA.table(model, correction = "GG",filename = paste(band, '_', con, type,'.doc', sep = ''))

con = 'Intrahemi-mid'
data <- combined_data %>%  filter(Connectivity == con, Band == band, Site == 'FT-R' | Site == 'FT-L' | Site == 'TP-R' | Site == 'TP-L') %>% droplevels()
model = ezANOVA(data, dv = COH, wid = Subject, within = c(Condition, Site), between = c(Group, Age), type = 3)
ggplot(data, aes(x=Age, y=COH, fill=Group)) + geom_boxplot(notch = FALSE)
data %>% 
  group_by(Age, Group) %>% 
  summarise(mean_COH = ci(COH)[1], lci = ci(COH)[2], hci = ci(COH)[3]) %>% 
  ggplot(aes(x = Age, y = mean_COH, color = Group)) +
  geom_line(aes(group = Group)) +
  geom_point() +
  geom_errorbar(aes(ymax=hci, ymin=lci), width=.1) 
apa.ezANOVA.table(model, correction = "GG",filename = paste(band, '_', con, type,'.doc', sep = ''))

con = 'Intrahemi-within'
data <- combined_data %>%  filter(Connectivity == con, Band == band, Site == 'R-F' | Site == 'L-F' |  Site == 'R-T' | Site == 'L-T' | Site == 'R-P' | Site == 'L-P') %>% droplevels()
model = ezANOVA(data, dv = COH, wid = Subject, within = c(Condition, Site), between = c(Group, Age), type = 3)
ggplot(data, aes(x=Age, y=COH, fill=Group)) + geom_boxplot(notch = FALSE)
data %>% 
  group_by(Age, Group) %>% 
  summarise(mean_COH = ci(COH)[1], lci = ci(COH)[2], hci = ci(COH)[3]) %>% 
  ggplot(aes(x = Age, y = mean_COH, color = Group)) +
  geom_line(aes(group = Group)) +
  geom_point() +
  geom_errorbar(aes(ymax=hci, ymin=lci), width=.1) 
apa.ezANOVA.table(model, correction = "GG",filename = paste(band, '_', con, type,'.doc', sep = ''))

con = 'Interhemi'
data <- combined_data %>%  filter(Connectivity == con, Band == band, Site == 'FR' | Site == 'TP' |  Site == 'PR') %>% droplevels()
model = ezANOVA(data, dv = COH, wid = Subject, within = c(Condition, Site), between = c(Group, Age), type = 3)
ggplot(data, aes(x=Age, y=COH, fill=Group)) + geom_boxplot(notch = FALSE)
data %>% 
  group_by(Age, Group) %>% 
  summarise(mean_COH = ci(COH)[1], lci = ci(COH)[2], hci = ci(COH)[3]) %>% 
  ggplot(aes(x = Age, y = mean_COH, color = Group)) +
  geom_line(aes(group = Group)) +
  geom_point() +
  geom_errorbar(aes(ymax=hci, ymin=lci), width=.1) 
apa.ezANOVA.table(model, correction = "GG",filename = paste(band, '_', con, type,'.doc', sep = ''))

band = 'alpha'
con = 'Intrahemi-long'
data <- combined_data %>%  filter(Connectivity == con, Band == band, Site == 'Right' | Site == 'Left') %>% droplevels()
model = ezANOVA(data, dv = COH, wid = Subject, within = c(Condition, Site), between = c(Group, Age), type = 3)
ggplot(data, aes(x=Age, y=COH, fill=Group)) + geom_boxplot(notch = FALSE)
data %>% 
  group_by(Age, Group) %>% 
  summarise(mean_COH = ci(COH)[1], lci = ci(COH)[2], hci = ci(COH)[3]) %>% 
  ggplot(aes(x = Age, y = mean_COH, color = Group)) +
  geom_line(aes(group = Group)) +
  geom_point() +
  geom_errorbar(aes(ymax=hci, ymin=lci), width=.1) 


apa.ezANOVA.table(model, correction = "GG",filename = paste(band, '_', con, type,'.doc', sep = ''))

con = 'Intrahemi-mid'
data <- combined_data %>%  filter(Connectivity == con, Band == band, Site == 'FT-R' | Site == 'FT-L' | Site == 'TP-R' | Site == 'TP-L') %>% droplevels()
model = ezANOVA(data, dv = COH, wid = Subject, within = c(Condition, Site), between = c(Group, Age), type = 3)
ggplot(data, aes(x=Age, y=COH, fill=Group)) + geom_boxplot(notch = FALSE)
data %>% 
  group_by(Age, Group) %>% 
  summarise(mean_COH = ci(COH)[1], lci = ci(COH)[2], hci = ci(COH)[3]) %>% 
  ggplot(aes(x = Age, y = mean_COH, color = Group)) +
  geom_line(aes(group = Group)) +
  geom_point() +
  geom_errorbar(aes(ymax=hci, ymin=lci), width=.1) 
apa.ezANOVA.table(model, correction = "GG",filename = paste(band, '_', con, type,'.doc', sep = ''))

con = 'Intrahemi-within'
data <- combined_data %>%  filter(Connectivity == con, Band == band, Site == 'R-F' | Site == 'L-F' |  Site == 'R-T' | Site == 'L-T' | Site == 'R-P' | Site == 'L-P') %>% droplevels()
model = ezANOVA(data, dv = COH, wid = Subject, within = c(Condition, Site), between = c(Group, Age), type = 3)
ggplot(data, aes(x=Age, y=COH, fill=Group)) + geom_boxplot(notch = FALSE)
data %>% 
  group_by(Age, Group) %>% 
  summarise(mean_COH = ci(COH)[1], lci = ci(COH)[2], hci = ci(COH)[3]) %>% 
  ggplot(aes(x = Age, y = mean_COH, color = Group)) +
  geom_line(aes(group = Group)) +
  geom_point() +
  geom_errorbar(aes(ymax=hci, ymin=lci), width=.1) 
apa.ezANOVA.table(model, correction = "GG",filename = paste(band, '_', con, type,'.doc', sep = ''))

con = 'Interhemi'
data <- combined_data %>%  filter(Connectivity == con, Band == band, Site == 'FR' | Site == 'TP' |  Site == 'PR') %>% droplevels()
model = ezANOVA(data, dv = COH, wid = Subject, within = c(Condition, Site), between = c(Group, Age), type = 3)
ggplot(data, aes(x=Age, y=COH, fill=Group)) + geom_boxplot(notch = FALSE)
data %>% 
  group_by(Age, Group) %>% 
  summarise(mean_COH = ci(COH)[1], lci = ci(COH)[2], hci = ci(COH)[3]) %>% 
  ggplot(aes(x = Age, y = mean_COH, color = Group)) +
  geom_line(aes(group = Group)) +
  geom_point() +
  geom_errorbar(aes(ymax=hci, ymin=lci), width=.1) 
apa.ezANOVA.table(model, correction = "GG",filename = paste(band, '_', con, type,'.doc', sep = ''))

band = 'beta'

con = 'Intrahemi-long'
data <- combined_data %>%  filter(Connectivity == con, Band == band, Site == 'Right' | Site == 'Left') %>% droplevels()
model = ezANOVA(data, dv = COH, wid = Subject, within = c(Condition, Site), between = c(Group, Age), type = 3)
ggplot(data, aes(x=Age, y=COH, fill=Group)) + geom_boxplot(notch = FALSE)
data %>% 
  group_by(Age, Group) %>% 
  summarise(mean_COH = ci(COH)[1], lci = ci(COH)[2], hci = ci(COH)[3]) %>% 
  ggplot(aes(x = Age, y = mean_COH, color = Group)) +
  geom_line(aes(group = Group)) +
  geom_point() +
  geom_errorbar(aes(ymax=hci, ymin=lci), width=.1) 


apa.ezANOVA.table(model, correction = "GG",filename = paste(band, '_', con, type,'.doc', sep = ''))

con = 'Intrahemi-mid'
data <- combined_data %>%  filter(Connectivity == con, Band == band, Site == 'FT-R' | Site == 'FT-L' | Site == 'TP-R' | Site == 'TP-L') %>% droplevels()
model = ezANOVA(data, dv = COH, wid = Subject, within = c(Condition, Site), between = c(Group, Age), type = 3)
ggplot(data, aes(x=Age, y=COH, fill=Group)) + geom_boxplot(notch = FALSE)
data %>% 
  group_by(Age, Group) %>% 
  summarise(mean_COH = ci(COH)[1], lci = ci(COH)[2], hci = ci(COH)[3]) %>% 
  ggplot(aes(x = Age, y = mean_COH, color = Group)) +
  geom_line(aes(group = Group)) +
  geom_point() +
  geom_errorbar(aes(ymax=hci, ymin=lci), width=.1) 
apa.ezANOVA.table(model, correction = "GG",filename = paste(band, '_', con, type,'.doc', sep = ''))

con = 'Intrahemi-within'
data <- combined_data %>%  filter(Connectivity == con, Band == band, Site == 'R-F' | Site == 'L-F' |  Site == 'R-T' | Site == 'L-T' | Site == 'R-P' | Site == 'L-P') %>% droplevels()
model = ezANOVA(data, dv = COH, wid = Subject, within = c(Condition, Site), between = c(Group, Age), type = 3)
ggplot(data, aes(x=Age, y=COH, fill=Group)) + geom_boxplot(notch = FALSE)
data %>% 
  group_by(Age, Group) %>% 
  summarise(mean_COH = ci(COH)[1], lci = ci(COH)[2], hci = ci(COH)[3]) %>% 
  ggplot(aes(x = Age, y = mean_COH, color = Group)) +
  geom_line(aes(group = Group)) +
  geom_point() +
  geom_errorbar(aes(ymax=hci, ymin=lci), width=.1) 
apa.ezANOVA.table(model, correction = "GG",filename = paste(band, '_', con, type,'.doc', sep = ''))

con = 'Interhemi'
data <- combined_data %>%  filter(Connectivity == con, Band == band, Site == 'FR' | Site == 'TP' |  Site == 'PR') %>% droplevels()
model = ezANOVA(data, dv = COH, wid = Subject, within = c(Condition, Site), between = c(Group, Age), type = 3)
ggplot(data, aes(x=Age, y=COH, fill=Group)) + geom_boxplot(notch = FALSE)
data %>% 
  group_by(Age, Group) %>% 
  summarise(mean_COH = ci(COH)[1], lci = ci(COH)[2], hci = ci(COH)[3]) %>% 
  ggplot(aes(x = Age, y = mean_COH, color = Group)) +
  geom_line(aes(group = Group)) +
  geom_point() +
  geom_errorbar(aes(ymax=hci, ymin=lci), width=.1) 
apa.ezANOVA.table(model, correction = "GG",filename = paste(band, '_', con, type,'.doc', sep = ''))


################################################################
## WPLI
################################################################
type = '_wpli'
#### Bands
band = 'delta'

con = 'Intrahemi-long'
data <- combined_data %>%  filter(Connectivity == con, Band == band, Site == 'Right' | Site == 'Left') %>% droplevels()
model = ezANOVA(data, dv = WPLI, wid = Subject, within = c(Condition, Site), between = c(Group, Age), type = 3)
ggplot(data, aes(x=Age, y=WPLI, fill=Group)) + geom_boxplot(notch = FALSE)
data %>% 
  group_by(Age, Group) %>% 
  summarise(mean_WPLI = ci(WPLI)[1], lci = ci(WPLI)[2], hci = ci(WPLI)[3]) %>% 
  ggplot(aes(x = Age, y = mean_WPLI, color = Group)) +
  geom_line(aes(group = Group)) +
  geom_point() +
  geom_errorbar(aes(ymax=hci, ymin=lci), width=.1) 
apa.ezANOVA.table(model, correction = "GG",filename = paste(band, '_', con, type,'.doc', sep = ''))

con = 'Intrahemi-mid'
data <- combined_data %>%  filter(Connectivity == con, Band == band, Site == 'FT-R' | Site == 'FT-L' | Site == 'TP-R' | Site == 'TP-L') %>% droplevels()
model = ezANOVA(data, dv = WPLI, wid = Subject, within = c(Condition, Site), between = c(Group, Age), type = 3)
ggplot(data, aes(x=Age, y=WPLI, fill=Group)) + geom_boxplot(notch = FALSE)
data %>% 
  group_by(Age, Group) %>% 
  summarise(mean_WPLI = ci(WPLI)[1], lci = ci(WPLI)[2], hci = ci(WPLI)[3]) %>% 
  ggplot(aes(x = Age, y = mean_WPLI, color = Group)) +
  geom_line(aes(group = Group)) +
  geom_point() +
  geom_errorbar(aes(ymax=hci, ymin=lci), width=.1)
apa.ezANOVA.table(model, correction = "GG",filename = paste(band, '_', con, type,'.doc', sep = ''))

con = 'Intrahemi-within'
data <- combined_data %>%  filter(Connectivity == con, Band == band, Site == 'R-F' | Site == 'L-F' |  Site == 'R-T' | Site == 'L-T' | Site == 'R-P' | Site == 'L-P') %>% droplevels()
model = ezANOVA(data, dv = WPLI, wid = Subject, within = c(Condition, Site), between = c(Group, Age), type = 3)
ggplot(data, aes(x=Age, y=WPLI, fill=Group)) + geom_boxplot(notch = FALSE)
data %>% 
  group_by(Age, Group) %>% 
  summarise(mean_WPLI = ci(WPLI)[1], lci = ci(WPLI)[2], hci = ci(WPLI)[3]) %>% 
  ggplot(aes(x = Age, y = mean_WPLI, color = Group)) +
  geom_line(aes(group = Group)) +
  geom_point() +
  geom_errorbar(aes(ymax=hci, ymin=lci), width=.1)
apa.ezANOVA.table(model, correction = "GG",filename = paste(band, '_', con, type,'.doc', sep = ''))

con = 'Interhemi'
data <- combined_data %>%  filter(Connectivity == con, Band == band, Site == 'FR' | Site == 'TP' |  Site == 'PR') %>% droplevels()
model = ezANOVA(data, dv = WPLI, wid = Subject, within = c(Condition, Site), between = c(Group, Age), type = 3)
ggplot(data, aes(x=Age, y=WPLI, fill=Group)) + geom_boxplot(notch = FALSE)
data %>% 
  group_by(Age, Group) %>% 
  summarise(mean_WPLI = ci(WPLI)[1], lci = ci(WPLI)[2], hci = ci(WPLI)[3]) %>% 
  ggplot(aes(x = Age, y = mean_WPLI, color = Group)) +
  geom_line(aes(group = Group)) +
  geom_point() +
  geom_errorbar(aes(ymax=hci, ymin=lci), width=.1)
apa.ezANOVA.table(model, correction = "GG",filename = paste(band, '_', con, type,'.doc', sep = ''))


band = 'theta'

con = 'Intrahemi-long'
data <- combined_data %>%  filter(Connectivity == con, Band == band, Site == 'Right' | Site == 'Left') %>% droplevels()
model = ezANOVA(data, dv = WPLI, wid = Subject, within = c(Condition, Site), between = c(Group, Age), type = 3)
ggplot(data, aes(x=Age, y=WPLI, fill=Group)) + geom_boxplot(notch = FALSE)
data %>% 
  group_by(Age, Group) %>% 
  summarise(mean_WPLI = ci(WPLI)[1], lci = ci(WPLI)[2], hci = ci(WPLI)[3]) %>% 
  ggplot(aes(x = Age, y = mean_WPLI, color = Group)) +
  geom_line(aes(group = Group)) +
  geom_point() +
  geom_errorbar(aes(ymax=hci, ymin=lci), width=.1) 
apa.ezANOVA.table(model, correction = "GG",filename = paste(band, '_', con, type,'.doc', sep = ''))

con = 'Intrahemi-mid'
data <- combined_data %>%  filter(Connectivity == con, Band == band, Site == 'FT-R' | Site == 'FT-L' | Site == 'TP-R' | Site == 'TP-L') %>% droplevels()
model = ezANOVA(data, dv = WPLI, wid = Subject, within = c(Condition, Site), between = c(Group, Age), type = 3)
ggplot(data, aes(x=Age, y=WPLI, fill=Group)) + geom_boxplot(notch = FALSE)
data %>% 
  group_by(Age, Group) %>% 
  summarise(mean_WPLI = ci(WPLI)[1], lci = ci(WPLI)[2], hci = ci(WPLI)[3]) %>% 
  ggplot(aes(x = Age, y = mean_WPLI, color = Group)) +
  geom_line(aes(group = Group)) +
  geom_point() +
  geom_errorbar(aes(ymax=hci, ymin=lci), width=.1)
apa.ezANOVA.table(model, correction = "GG",filename = paste(band, '_', con, type,'.doc', sep = ''))

con = 'Intrahemi-within'
data <- combined_data %>%  filter(Connectivity == con, Band == band, Site == 'R-F' | Site == 'L-F' |  Site == 'R-T' | Site == 'L-T' | Site == 'R-P' | Site == 'L-P') %>% droplevels()
model = ezANOVA(data, dv = WPLI, wid = Subject, within = c(Condition, Site), between = c(Group, Age), type = 3)
ggplot(data, aes(x=Age, y=WPLI, fill=Group)) + geom_boxplot(notch = FALSE)
data %>% 
  group_by(Age, Group) %>% 
  summarise(mean_WPLI = ci(WPLI)[1], lci = ci(WPLI)[2], hci = ci(WPLI)[3]) %>% 
  ggplot(aes(x = Age, y = mean_WPLI, color = Group)) +
  geom_line(aes(group = Group)) +
  geom_point() +
  geom_errorbar(aes(ymax=hci, ymin=lci), width=.1)
apa.ezANOVA.table(model, correction = "GG",filename = paste(band, '_', con, type,'.doc', sep = ''))

con = 'Interhemi'
data <- combined_data %>%  filter(Connectivity == con, Band == band, Site == 'FR' | Site == 'TP' |  Site == 'PR') %>% droplevels()
model = ezANOVA(data, dv = WPLI, wid = Subject, within = c(Condition, Site), between = c(Group, Age), type = 3)
ggplot(data, aes(x=Age, y=WPLI, fill=Group)) + geom_boxplot(notch = FALSE)
data %>% 
  group_by(Age, Group) %>% 
  summarise(mean_WPLI = ci(WPLI)[1], lci = ci(WPLI)[2], hci = ci(WPLI)[3]) %>% 
  ggplot(aes(x = Age, y = mean_WPLI, color = Group)) +
  geom_line(aes(group = Group)) +
  geom_point() +
  geom_errorbar(aes(ymax=hci, ymin=lci), width=.1)
apa.ezANOVA.table(model, correction = "GG",filename = paste(band, '_', con, type,'.doc', sep = ''))

band = 'alpha'

con = 'Intrahemi-long'
data <- combined_data %>%  filter(Connectivity == con, Band == band, Site == 'Right' | Site == 'Left') %>% droplevels()
model = ezANOVA(data, dv = WPLI, wid = Subject, within = c(Condition, Site), between = c(Group, Age), type = 3)
ggplot(data, aes(x=Age, y=WPLI, fill=Group)) + geom_boxplot(notch = FALSE)
data %>% 
  group_by(Age, Group) %>% 
  summarise(mean_WPLI = ci(WPLI)[1], lci = ci(WPLI)[2], hci = ci(WPLI)[3]) %>% 
  ggplot(aes(x = Age, y = mean_WPLI, color = Group)) +
  geom_line(aes(group = Group)) +
  geom_point() +
  geom_errorbar(aes(ymax=hci, ymin=lci), width=.1) 
apa.ezANOVA.table(model, correction = "GG",filename = paste(band, '_', con, type,'.doc', sep = ''))

con = 'Intrahemi-mid'
data <- combined_data %>%  filter(Connectivity == con, Band == band, Site == 'FT-R' | Site == 'FT-L' | Site == 'TP-R' | Site == 'TP-L') %>% droplevels()
model = ezANOVA(data, dv = WPLI, wid = Subject, within = c(Condition, Site), between = c(Group, Age), type = 3)
ggplot(data, aes(x=Age, y=WPLI, fill=Group)) + geom_boxplot(notch = FALSE)
data %>% 
  group_by(Age, Group) %>% 
  summarise(mean_WPLI = ci(WPLI)[1], lci = ci(WPLI)[2], hci = ci(WPLI)[3]) %>% 
  ggplot(aes(x = Age, y = mean_WPLI, color = Group)) +
  geom_line(aes(group = Group)) +
  geom_point() +
  geom_errorbar(aes(ymax=hci, ymin=lci), width=.1)
apa.ezANOVA.table(model, correction = "GG",filename = paste(band, '_', con, type,'.doc', sep = ''))

con = 'Intrahemi-within'
data <- combined_data %>%  filter(Connectivity == con, Band == band, Site == 'R-F' | Site == 'L-F' |  Site == 'R-T' | Site == 'L-T' | Site == 'R-P' | Site == 'L-P') %>% droplevels()
model = ezANOVA(data, dv = WPLI, wid = Subject, within = c(Condition, Site), between = c(Group, Age), type = 3)
ggplot(data, aes(x=Age, y=WPLI, fill=Group)) + geom_boxplot(notch = FALSE)
data %>% 
  group_by(Age, Group) %>% 
  summarise(mean_WPLI = ci(WPLI)[1], lci = ci(WPLI)[2], hci = ci(WPLI)[3]) %>% 
  ggplot(aes(x = Age, y = mean_WPLI, color = Group)) +
  geom_line(aes(group = Group)) +
  geom_point() +
  geom_errorbar(aes(ymax=hci, ymin=lci), width=.1)
apa.ezANOVA.table(model, correction = "GG",filename = paste(band, '_', con, type,'.doc', sep = ''))

con = 'Interhemi'
data <- combined_data %>%  filter(Connectivity == con, Band == band, Site == 'FR' | Site == 'TP' |  Site == 'PR') %>% droplevels()
model = ezANOVA(data, dv = WPLI, wid = Subject, within = c(Condition, Site), between = c(Group, Age), type = 3)
ggplot(data, aes(x=Age, y=WPLI, fill=Group)) + geom_boxplot(notch = FALSE)
data %>% 
  group_by(Age, Group) %>% 
  summarise(mean_WPLI = ci(WPLI)[1], lci = ci(WPLI)[2], hci = ci(WPLI)[3]) %>% 
  ggplot(aes(x = Age, y = mean_WPLI, color = Group)) +
  geom_line(aes(group = Group)) +
  geom_point() +
  geom_errorbar(aes(ymax=hci, ymin=lci), width=.1)
apa.ezANOVA.table(model, correction = "GG",filename = paste(band, '_', con, type,'.doc', sep = ''))

band = 'beta'

con = 'Intrahemi-long'
data <- combined_data %>%  filter(Connectivity == con, Band == band, Site == 'Right' | Site == 'Left') %>% droplevels()
model = ezANOVA(data, dv = WPLI, wid = Subject, within = c(Condition, Site), between = c(Group, Age), type = 3)
ggplot(data, aes(x=Age, y=WPLI, fill=Group)) + geom_boxplot(notch = FALSE)
data %>% 
  group_by(Age, Group) %>% 
  summarise(mean_WPLI = ci(WPLI)[1], lci = ci(WPLI)[2], hci = ci(WPLI)[3]) %>% 
  ggplot(aes(x = Age, y = mean_WPLI, color = Group)) +
  geom_line(aes(group = Group)) +
  geom_point() +
  geom_errorbar(aes(ymax=hci, ymin=lci), width=.1) 
apa.ezANOVA.table(model, correction = "GG",filename = paste(band, '_', con, type,'.doc', sep = ''))

con = 'Intrahemi-mid'
data <- combined_data %>%  filter(Connectivity == con, Band == band, Site == 'FT-R' | Site == 'FT-L' | Site == 'TP-R' | Site == 'TP-L') %>% droplevels()
model = ezANOVA(data, dv = WPLI, wid = Subject, within = c(Condition, Site), between = c(Group, Age), type = 3)
ggplot(data, aes(x=Age, y=WPLI, fill=Group)) + geom_boxplot(notch = FALSE)
data %>% 
  group_by(Age, Group) %>% 
  summarise(mean_WPLI = ci(WPLI)[1], lci = ci(WPLI)[2], hci = ci(WPLI)[3]) %>% 
  ggplot(aes(x = Age, y = mean_WPLI, color = Group)) +
  geom_line(aes(group = Group)) +
  geom_point() +
  geom_errorbar(aes(ymax=hci, ymin=lci), width=.1)
apa.ezANOVA.table(model, correction = "GG",filename = paste(band, '_', con, type,'.doc', sep = ''))

con = 'Intrahemi-within'
data <- combined_data %>%  filter(Connectivity == con, Band == band, Site == 'R-F' | Site == 'L-F' |  Site == 'R-T' | Site == 'L-T' | Site == 'R-P' | Site == 'L-P') %>% droplevels()
model = ezANOVA(data, dv = WPLI, wid = Subject, within = c(Condition, Site), between = c(Group, Age), type = 3)
ggplot(data, aes(x=Age, y=WPLI, fill=Group)) + geom_boxplot(notch = FALSE)
data %>% 
  group_by(Age, Group) %>% 
  summarise(mean_WPLI = ci(WPLI)[1], lci = ci(WPLI)[2], hci = ci(WPLI)[3]) %>% 
  ggplot(aes(x = Age, y = mean_WPLI, color = Group)) +
  geom_line(aes(group = Group)) +
  geom_point() +
  geom_errorbar(aes(ymax=hci, ymin=lci), width=.1)
apa.ezANOVA.table(model, correction = "GG",filename = paste(band, '_', con, type,'.doc', sep = ''))

con = 'Interhemi'
data <- combined_data %>%  filter(Connectivity == con, Band == band, Site == 'FR' | Site == 'TP' |  Site == 'PR') %>% droplevels()
model = ezANOVA(data, dv = WPLI, wid = Subject, within = c(Condition, Site), between = c(Group, Age), type = 3)
ggplot(data, aes(x=Age, y=WPLI, fill=Group)) + geom_boxplot(notch = FALSE)
data %>% 
  group_by(Age, Group) %>% 
  summarise(mean_WPLI = ci(WPLI)[1], lci = ci(WPLI)[2], hci = ci(WPLI)[3]) %>% 
  ggplot(aes(x = Age, y = mean_WPLI, color = Group)) +
  geom_line(aes(group = Group)) +
  geom_point() +
  geom_errorbar(aes(ymax=hci, ymin=lci), width=.1)
apa.ezANOVA.table(model, correction = "GG",filename = paste(band, '_', con, type,'.doc', sep = ''))

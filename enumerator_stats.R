################################################################################
## Load Libraries
################################################################################
pkgs <- c('purrr','plyr','tidyverse', 'plotrix','lubridate','kableExtra','hrbrthemes','ggplot2','extrafont','float','reshape',
          'gridExtra','rsvg','png','devtools','readxl','date', 'ggpubr', 'tidyselect', 'httr', 'jsonlite', 'extrafont', 'colorspace',
          'ggrepel', 'forcats', 'ggpubr', 'readstata13', 'cowplot', 'scales')
lapply(pkgs, require, character.only = TRUE)

dark2pal <- c("#1B9E77", "#D95F02", "#7570B3", "#66A61E", "#E6AB02")

################################################################################
## Reading in data
################################################################################

# Setting directories
r.functions.dir <- '/Users/lshvb5/Documents/rammps/'
dir.inputdata <- ('/Users/lshvb5/OneDrive - London School of Hygiene and Tropical Medicine/General/Partners/UNIKIN/SurveyCTO Audits/')


# DRC
Consented <- read.csv(paste0(dir.inputdata, "Clean data/Consented-2023-02-27.csv"))%>%
  mutate(source = ifelse(grepl('Fer', Source.prov),'Feroxus',
                         ifelse(grepl('IVR', Source.prov),'IVR',NA)),
         Outcome2 = ifelse(Outcome2 =='NNA'| Outcome2 =='NR', 'NNA/NR', as.character(Outcome2)),
         RuralUrban = case_when(E4a_lab == 'City' ~ 'Urban',
                                E4a_lab == 'Town/Trading Centre' ~ 'Urban',
                                E4a_lab == 'Rural' ~ 'Rural'))

data <- read.csv(paste0(dir.inputdata, "Clean data/Data_Long2023-02-27.csv"))%>%
  mutate(source = ifelse(grepl('Fer', Source.prov),'Feroxus',
                         ifelse(grepl('IVR', Source.prov),'IVR',NA)),
         Outcome2 = ifelse(Outcome2 =='NNA'| Outcome2 =='NR', 'NNA/NR', as.character(Outcome2)),
         RuralUrban = case_when(E4a_lab == 'City' ~ 'Urban',
                                E4a_lab == 'Town/Trading Centre' ~ 'Urban',
                                E4a_lab == 'Rural' ~ 'Rural'))
# dat.wide <- read.csv(paste0(dir.inputdata, "Clean data/Data_Wide2022-08-23.csv")) %>%
dat.wide <- read.csv(paste0(dir.inputdata, "Clean data/Data_Wide-2023-02-27.csv")) %>%
  mutate(source = ifelse(grepl('Fer',Source.prov.1),'Feroxus',
                         ifelse(grepl('IVR',Source.prov.1),'IVR', NA)),
         Eligibility = case_when(Outcome.FINAL %in% c('COMP', 'INCO', 'PART') ~ 'Eligible',
                                 Outcome.FINAL %in% c('INEL','DEFER','NNU') ~ 'Ineligible',
                                 Outcome.FINAL %in% c('REFU','NNA','NR','NNA/NR','LANG','PEND') ~ 'Eligibility unknown'),
         Eligibility = factor(Eligibility, levels = c('Eligible','Ineligible','Eligibility unknown', 'Other Ineligible','Other Eligible Unknown', 'CATI Other Eligibility Unknown')),
         Outcome.FINAL = ifelse(Outcome.FINAL =='NNA'| Outcome.FINAL =='NR', 'NNA/NR', as.character(Outcome.FINAL)))

# Malawi
Consentedmw <- read.csv(paste0(r.functions.dir, 'RaMMPS_MWApp/ConsentedMW.csv')) %>%
  mutate(Outcome2 = ifelse(Outcome2 =='NNA'| Outcome2 =='NR', 'NNA/NR', as.character(Outcome2))) 
dat.widemw <- read.csv(paste0(r.functions.dir,'RaMMPS_MWApp/DatWide.csv'))%>%
  mutate(Eligibility = case_when(Outcome.FINAL %in% c('COMP', 'INCO', 'PART') ~ 'Eligible',
                                 Outcome.FINAL %in% c('INEL','DEFER','NNU') ~ 'Ineligible',
                                 Outcome.FINAL %in% c('REFU','NNA','NR','NNA/NR','LANG','PEND') ~ 'Eligibility unknown'),
         Eligibility = factor(Eligibility, levels = c('Eligible','Ineligible','Eligibility unknown', 'Other Ineligible','Other Eligible Unknown', 'CATI Other Eligibility Unknown')),
         Outcome.FINAL = ifelse(Outcome.FINAL =='NNA'| Outcome.FINAL =='NR', 'NNA/NR', as.character(Outcome.FINAL)))
datamw <- read.csv(paste0(r.functions.dir,'RaMMPS_MWApp/DatLongMW.csv')) %>%
  dplyr::filter(!is.na(Outcome2)) %>%
  mutate(Outcome2 = ifelse(Outcome2 =='NNA'| Outcome2 =='NR', 'NNA/NR', as.character(Outcome2))) 

# Burkina Faso 
dir.input <- "/Users/lshvb5/OneDrive - London School of Hygiene and Tropical Medicine/General/Partners/Burkina/Survey Data/"
eval(parse(paste0(r.functions.dir,'CleaningScript_func_BF_RDD.R'), encoding="UTF-8"))
listbf <- clean_bf() #clean BF data

rdd.data <- listbf[[1]] %>%
  mutate(
    Outcome = ifelse(Outcome =='NNA'| Outcome =='NR', 'NNA/NR', as.character(Outcome)),
    Eligibility = case_when(Outcome %in% c('COMP','PART') ~ 'Eligible',
                            Outcome %in% c('INEL','DEFER','NNU') ~ 'Ineligible',
                            Outcome %in% c('REFU','NNA','NR','LANG','PEND', 'NNA or NR', 'Other') ~ 'Eligibility unknown'),
    Eligibility = factor(Eligibility, levels = c('Eligible','Ineligible','Eligibility unknown', 'Other Ineligible','Other Eligibility Unknown', 'CATI Other Eligibility Unknown')),
    Outcome = ifelse(Outcome =='NNA'| Outcome =='NR', 'NNA/NR', as.character(Outcome)),
    gender = case_when(gender==1 ~ "Male",
                       gender==2 ~ "Female"),
    Resp.Sex = gender)

hello <- rdd.data[grep("-Refer", rdd.data$caseid), ]
hello$caseid

# ehcvm.data <- listbf[[2]]
rdd.dat.wide <- listbf[[2]] %>%
  mutate(
    Outcome.FINAL = ifelse(Outcome.FINAL =='NNA'| Outcome.FINAL =='NR', 'NNA/NR', as.character(Outcome.FINAL)),
    Eligibility = case_when(Outcome.FINAL %in% c('COMP','PART') ~ 'Eligible',
                            Outcome.FINAL %in% c('INEL','DEFER','NNU') ~ 'Ineligible',
                            Outcome.FINAL %in% c('REFU','NNA','NR','LANG','PEND', 'NNA or NR', 'Other') ~ 'Eligibility unknown'),
    Eligibility = factor(Eligibility, levels = c('Eligible','Ineligible','Eligibility unknown', 'Other Ineligible','Other Eligibility Unknown', 'CATI Other Eligibility Unknown')),
    Outcome.FINAL = ifelse(Outcome.FINAL =='NNA'| Outcome.FINAL =='NR', 'NNA/NR', as.character(Outcome.FINAL)),
    gender = case_when(gender==1 ~ "Male",
                       gender==2 ~ "Female"),
    Resp.Sex = gender)


# ehcvm.dat.wide <- listbf[[4]]
bf.dat.wide <- listbf[[4]]
bf.data <- listbf[[3]]
Consentedrdd <- rdd.data %>%
  filter(Outcome == 'COMP') %>%
  dplyr::mutate(month.interview1 = lubridate::floor_date(as.Date(Date.Interview), 'month'),
                gender = case_when(is.na(gender) ~ 'Male', TRUE ~ as.character(gender)),
                Resp.Age.pyr = cut(as.numeric(age), c(14,19,29,39,49,59,64))
  )#month(Date.Interview),

# Consentedehcvm <- ehcvm.data %>%
#   filter(Outcome == 'COMP') %>%
#   dplyr::mutate(month.interview1 = lubridate::floor_date(as.Date(Date.Interview), 'month'),#month(Date.Interview),
#                 month.interview = case_when(month.interview == '2022-01-01'~ 'Jan-22',
#                                             month.interview == '2022-02-01'~ 'Feb-22',
#                                             month.interview == '2022-03-01'~ 'Mar-22',
#                                             month.interview == '2022-04-01'~'Apr-22',
#                                             month.interview == '2022-05-01'~'May-22',
#                                             month.interview == '2022-06-01'~'Jun-22',
#                                             month.interview == '2022-07-01'~'Jul-22',
#                                             month.interview == '2022-08-01'~'Aug-22',
#                                             month.interview == '2022-09-01'~'Sep-22',
#                                             month.interview == '2021-08-01'~'Aug-21',
#                                             month.interview == '2021-09-01'~'Sep-21',
#                                             month.interview == '2021-10-01'~'Oct-21',
#                                             month.interview == '2021-11-01'~'Nov-21',
#                                             month.interview == '2021-12-01'~'Dec-21'))
Consentedbf <- bf.data %>%
  filter(Outcome == 'COMP')

# Completed interviews per day per enumerator ----------------------------------

# DRC
Counts_drc <- aggregate(data.frame(count = Consented$Date.Interview),
                    by= list(value = Consented$Date.Interview,
                             enumerator = Consented$enumerator),length) %>%
  group_by(enumerator) %>%
  mutate(csum = cumsum(count)) %>% as.data.frame() 

Counts_drc$enumerator <- ordered(Counts_drc$enumerator, levels = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", 'E11', 'E12', 'E13', 'E14', 'E15', 'E16', 'E17', 'E18', 'E19', 'E20'))

daily_int_drc <- ggplot(data = Counts_drc) +
  geom_boxplot(aes(x = enumerator, y = count, fill = "#D95F02"), alpha =0.8) + 
  scale_y_continuous(name = 'Daily Completed interviews', limits = c(0, 35)) +
  scale_x_discrete() +
  theme(axis.title.y = element_text(color = '#D95F02', )) + 
  theme_bw() + xlab('Enumerator')+
  theme(legend.position="none") +
  scale_fill_manual(values = c("#D95F02","#7570B3")) 
daily_int_drc

# MW
Counts_mw <- aggregate(data.frame(count = Consentedmw$Date.Interview),
                    by= list(value = Consentedmw$Date.Interview,
                             enumerator = Consentedmw$enumerator),length) %>%
  group_by(enumerator) %>%
  mutate(csum = cumsum(count)) %>% as.data.frame() 

Counts_mw$enumerator <- ordered(Counts_mw$enumerator, levels = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", 'E11', 'E12', 'E13', 'E14', 'E15','E16','E17','E18','E19','E20','E21','E22','E23','E24','E25','E26'))

daily_int_mw <- ggplot(data = Counts_mw) +
  geom_boxplot(aes(x = enumerator, y = count, fill = "#D95F02")) + 
  scale_y_continuous(name = 'Daily Completed interviews', limits = c(0, 35)) +
  scale_x_discrete() +
  theme(axis.title.y = element_text(color = '#D95F02')) + 
  theme_bw() + xlab('Enumerator')+
  theme(legend.position="none") + 
  labs(fill = "Respondent characteristics")+
  scale_fill_manual(values = c("#D95F02","#7570B3"))#"#1B9E77", 
daily_int_mw

# BF
Counts_bf <- aggregate(data.frame(count = Consentedrdd$Date.Interview),
                    by= list(value = Consentedrdd$Date.Interview,
                             enumerator = Consentedrdd$enumerator),length) %>%
  group_by(enumerator) %>%
  mutate(csum = cumsum(count)) %>% as.data.frame() 

Counts_bf$enumerator <- ordered(Counts_bf$enumerator, levels = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", 'E11', 'E12', 'E13', 'E14', 'E15','E16','E17','E18','E19','E20','E21','E22','E23','E24','E25','E26'))

daily_int_bf <- ggplot(data = Counts_bf) +
  geom_boxplot(aes(x = enumerator, y = count, fill = "#D95F02")) + 
  scale_y_continuous(name = 'Daily Completed interviews', limits = c(0, 35)) +
  scale_x_discrete() +
  theme(axis.title.y = element_text(color = '#D95F02')) + 
  theme_bw() + xlab('Enumerator')+
  theme(legend.position="none") + 
  labs(fill = "Respondent characteristics")+
  scale_fill_manual(values = c("#D95F02","#7570B3"))#"#1B9E77", 
daily_int_bf

# add the plots from the three countries together
# arrange the three plots into 4 quadrants
daily_int_plots<- plot_grid(daily_int_mw + theme(legend.position="none", axis.text=element_text(size=8)), #remove legends first before combining legends
                              daily_int_drc + theme(legend.position="none", axis.text=element_text(size=8)),
                              daily_int_bf + theme(legend.position="none", axis.text=element_text(size=8)), 
                              labels = c("MW", "DRC", "BF"), #Title for each graph
                              label_size = 10)

daily_int_plots

# Completed interviews per day per enumerator by respondent characteristics ----

### Sex
# DRC
Counts_sex_drc <- aggregate(data.frame(count = Consented$Date.Interview),
                    by= list(value = Consented$Date.Interview,
                             enumerator = Consented$enumerator,
                             woman=Consented$Resp.Sex),length) %>%
  group_by(enumerator,woman) %>%
  mutate(csum = cumsum(count)) %>% as.data.frame() 

Counts_sex_drc$enumerator <- ordered(Counts_sex_drc$enumerator, levels = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", 'E11', 'E12', 'E13', 'E14', 'E15','E16','E17','E18','E19','E20','E21','E22','E23','E24','E25','E26'))

int_resp_char_drc <-ggplot(data = Counts_sex_drc) +
  geom_boxplot(aes(x = enumerator, y = count, fill = woman)) + 
  scale_y_continuous(name = 'Daily Completed interviews', limits = c(0, 25)) +
  scale_x_discrete() +
  theme(axis.title.y = element_text(color = '#D95F02')) + 
  theme_bw() + xlab('Enumerator')+
  # theme(legend.position="none") + 
  labs(fill = "Respondent characteristics")+
  scale_fill_manual(values = c('#D2042D','#0671B7'))#"#1B9E77", 
int_resp_char_drc

# MW
Counts_sex_mw <- aggregate(data.frame(count = Consentedmw$Date.Interview),
                    by= list(value = Consentedmw$Date.Interview,
                             enumerator = Consentedmw$enumerator,
                             woman=Consentedmw$Resp.Sex),length) %>%
  group_by(enumerator,woman) %>%
  mutate(csum = cumsum(count)) %>% as.data.frame() 

Counts_sex_mw$enumerator <- ordered(Counts_sex_mw$enumerator, levels = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", 'E11', 'E12', 'E13', 'E14', 'E15','E16','E17','E18','E19','E20','E21','E22','E23','E24','E25','E26'))

int_resp_char_mw <- ggplot(data = Counts_sex_mw) +
  geom_boxplot(aes(x = enumerator, y = count, fill = woman)) + 
  scale_y_continuous(name = 'Daily Completed interviews', limits = c(0, 25)) +
  scale_x_discrete() +
  theme(axis.title.y = element_text(color = '#D95F02')) + 
  theme_bw() + xlab('Enumerator')+
  # theme(legend.position="none") + 
  labs(fill = "Respondent characteristics")+
  scale_fill_manual(values = c('#D2042D','#0671B7'))#"#1B9E77", 
int_resp_char_mw

# BF
Counts_sex_bf <- aggregate(data.frame(count = Consentedrdd$Date.Interview),
                    by= list(value = Consentedrdd$Date.Interview,
                             enumerator = Consentedrdd$enumerator,
                             woman=Consentedrdd$Resp.Sex),length) %>%
  group_by(enumerator,woman) %>%
  mutate(csum = cumsum(count)) %>% as.data.frame() 

Counts_sex_bf$enumerator <- ordered(Counts_sex_bf$enumerator, levels = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", 'E11', 'E12', 'E13', 'E14', 'E15','E16','E17','E18','E19','E20','E21','E22','E23','E24','E25','E26'))

int_resp_char_bf <- ggplot(data = Counts_sex_bf) +
  geom_boxplot(aes(x = enumerator, y = count, fill = woman)) + 
  scale_y_continuous(name = 'Daily Completed interviews', limits = c(0, 25)) +
  scale_x_discrete() +
  theme(axis.title.y = element_text(color = '#D95F02')) + 
  theme_bw() + xlab('Enumerator')+
  # theme(legend.position="none") + 
  labs(fill = "Respondent characteristics")+
  scale_fill_manual(values = c('#D2042D','#0671B7'))#"#1B9E77", 
int_resp_char_bf

# add the plots from the three countries together
# arrange the three plots into 4 quadrants
daily_int_plots<- plot_grid(int_resp_char_mw + theme(legend.position="none", axis.text=element_text(size=8)), #remove legends first before combining legends
                            int_resp_char_drc + theme(legend.position="none", axis.text=element_text(size=8)),
                            int_resp_char_bf + theme(legend.position="none", axis.text=element_text(size=8)), 
                            labels = c("MW", "DRC", "BF"), #Title for each graph
                            label_size = 10)

# extract the legend from Malawi plots
daily_int_legend <- get_legend(
  int_resp_char_mw + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = c(0.65, 5),
          legend.key.size = unit(1, 'cm'),
          legend.text = element_text(size=12),
          legend.title = element_text(size=15)))

# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
plot_grid(daily_int_plots, daily_int_legend, ncol = 1, rel_heights = c(1, .1))

### Urban vs Rural
# DRC
Counts_urban_drc <- aggregate(data.frame(count = Consented$Date.Interview),
                            by= list(value = Consented$Date.Interview,
                                     enumerator = Consented$enumerator,
                                     woman=Consented$RuralUrban),length) %>%
  group_by(enumerator,woman) %>%
  mutate(csum = cumsum(count)) %>% as.data.frame() 

Counts_urban_drc$enumerator <- ordered(Counts_urban_drc$enumerator, levels = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", 'E11', 'E12', 'E13', 'E14', 'E15','E16','E17','E18','E19','E20','E21','E22','E23','E24','E25','E26'))

int_resp_urban_drc <-ggplot(data = Counts_urban_drc) +
  geom_boxplot(aes(x = enumerator, y = count, fill = woman)) + 
  scale_y_continuous(name = 'Daily Completed interviews', limits = c(0, 30)) +
  scale_x_discrete() +
  theme(axis.title.y = element_text(color = '#D95F02')) + 
  theme_bw() + xlab('Enumerator')+
  # theme(legend.position="none") + 
  labs(fill = "Respondent characteristics")+
  scale_fill_manual(values = c("#D95F02","#7570B3")) 
int_resp_urban_drc

# MW
Counts_urban_mw <- aggregate(data.frame(count = Consentedmw$Date.Interview),
                           by= list(value = Consentedmw$Date.Interview,
                                    enumerator = Consentedmw$enumerator,
                                    woman=Consentedmw$RuralUrban),length) %>%
  group_by(enumerator,woman) %>%
  mutate(csum = cumsum(count)) %>% as.data.frame() 

Counts_urban_mw$enumerator <- ordered(Counts_urban_mw$enumerator, levels = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", 'E11', 'E12', 'E13', 'E14', 'E15','E16','E17','E18','E19','E20','E21','E22','E23','E24','E25','E26'))

int_resp_urban_mw <- ggplot(data = Counts_urban_mw) +
  geom_boxplot(aes(x = enumerator, y = count, fill = woman)) + 
  scale_y_continuous(name = 'Daily Completed interviews', limits = c(0, 30)) +
  scale_x_discrete() +
  theme(axis.title.y = element_text(color = '#D95F02')) + 
  theme_bw() + xlab('Enumerator')+
  # theme(legend.position="none") + 
  labs(fill = "Respondent characteristics")+
  scale_fill_manual(values = c("#D95F02","#7570B3"))
int_resp_urban_mw

# BF
Counts_urban_bf <- aggregate(data.frame(count = Consentedrdd$Date.Interview),
                           by= list(value = Consentedrdd$Date.Interview,
                                    enumerator = Consentedrdd$enumerator,
                                    woman=Consentedrdd$urban_rural),length) %>%
  group_by(enumerator,woman) %>%
  mutate(csum = cumsum(count)) %>% as.data.frame() 

Counts_urban_bf$enumerator <- ordered(Counts_urban_bf$enumerator, levels = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", 'E11', 'E12', 'E13', 'E14', 'E15','E16','E17','E18','E19','E20','E21','E22','E23','E24','E25','E26'))

int_resp_urban_bf <- ggplot(data = Counts_urban_bf) +
  geom_boxplot(aes(x = enumerator, y = count, fill = woman)) + 
  scale_y_continuous(name = 'Daily Completed interviews', limits = c(0, 30)) +
  scale_x_discrete() +
  theme(axis.title.y = element_text(color = '#D95F02')) + 
  theme_bw() + xlab('Enumerator')+
  # theme(legend.position="none") + 
  labs(fill = "Respondent characteristics")+
  scale_fill_manual(values = c("#D95F02","#7570B3")) 
int_resp_urban_bf

# add the plots from the three countries together
# arrange the three plots into 4 quadrants
daily_int_urban_plots<- plot_grid(int_resp_urban_mw + theme(legend.position="none", axis.text=element_text(size=8)), #remove legends first before combining legends
                            int_resp_urban_drc + theme(legend.position="none", axis.text=element_text(size=8)),
                            int_resp_urban_bf + theme(legend.position="none", axis.text=element_text(size=8)), 
                            labels = c("MW", "DRC", "BF"), #Title for each graph
                            label_size = 10)

# extract the legend from Malawi plots
daily_int_urban_legend <- get_legend(
  int_resp_urban_mw + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = c(0.65, 5),
          legend.key.size = unit(1, 'cm'),
          legend.text = element_text(size=12),
          legend.title = element_text(size=15)))

# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
plot_grid(daily_int_urban_plots, daily_int_urban_legend, ncol = 1, rel_heights = c(1, .1))

# CATI Outcomes by Enumerator, excluding dysfunctional numbers -----------------

# DRC
d_drc <- dat.wide %>%
  filter(Outcome.FINAL =='REFU' | Outcome.FINAL =='COMP') %>%
  group_by(enum, Outcome.FINAL) %>%
  dplyr::summarize(n = n()) %>% filter(!is.na(enum))

d_drc$enum <- ordered(d_drc$enum, levels = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10",'E11', 'E12', 'E13', 'E14', 'E15','E16','E17','E18','E19','E20','E21','E22','E23','E24','E25','E26'))

enum_outcomes_drc <- ggplot(d_drc) +
  geom_bar(aes(x = enum, y = n, fill = Outcome.FINAL), position = 'dodge',stat = 'identity', color= 'black') +
  scale_x_discrete() +
  theme_bw() + ylab('Numbers') +
  scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666","#fb9a99","#a6cee3")) +labs(fill = 'Outcome', x = 'Enumerator') +
  scale_y_continuous(limits = c(0, 1500))
enum_outcomes_drc

# MW
d_mw <- dat.widemw %>%
  filter(Outcome.FINAL =='REFU' | Outcome.FINAL =='COMP') %>%
  group_by(enum, Outcome.FINAL) %>%
  dplyr::summarize(n = n()) %>% filter(!is.na(enum))

d_mw$enum <- ordered(d_mw$enum, levels = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10",'E11', 'E12', 'E13', 'E14', 'E15','E16','E17','E18','E19','E20','E21','E22','E23','E24','E25','E26'))

enum_outcomes_mw <- ggplot(d_mw) +
  geom_bar(aes(x = enum, y = n, fill = Outcome.FINAL), position = 'dodge',stat = 'identity', color= 'black') +
  scale_x_discrete() +
  theme_bw() + ylab('Numbers') +
  scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666","#fb9a99","#a6cee3")) +labs(fill = 'Outcome', x = 'Enumerator') +
  scale_y_continuous(limits = c(0, 1500))
enum_outcomes_mw

# BF
d_bf <- rdd.dat.wide %>%
  filter(Outcome.FINAL =='REFU' | Outcome.FINAL =='COMP') %>%
  group_by(enumerator, Outcome.FINAL) %>%
  dplyr::summarize(n = n()) %>% filter(!is.na(enumerator))

d_bf$enumerator <- ordered(d_bf$enumerator, levels = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10",'E11', 'E12', 'E13', 'E14', 'E15','E16','E17','E18','E19','E20','E21','E22','E23','E24','E25','E26'))

enum_outcomes_bf <- ggplot(d_bf) +
  geom_bar(aes(x = enumerator, y = n, fill = Outcome.FINAL), position = 'dodge',stat = 'identity', color= 'black') +
  scale_x_discrete() +
  theme_bw() + ylab('Numbers') +
  scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666","#fb9a99","#a6cee3")) +labs(fill = 'Outcome', x = 'Enumerator') +
  scale_y_continuous(limits = c(0, 1500))
enum_outcomes_bf

# add the plots from the three countries together
# arrange the three plots into 4 quadrants
enum_outcomes_plots<- plot_grid(enum_outcomes_mw + theme(legend.position="none", axis.text=element_text(size=8)), #remove legends first before combining legends
                                  enum_outcomes_drc + theme(legend.position="none", axis.text=element_text(size=8)),
                                  enum_outcomes_bf + theme(legend.position="none", axis.text=element_text(size=8)), 
                                  labels = c("MW", "DRC", "BF"), #Title for each graph
                                  label_size = 10)

# extract the legend from Malawi plots
enum_outcomes_legend <- get_legend(
  enum_outcomes_mw + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = c(0.65, 5),
          legend.key.size = unit(1, 'cm'),
          legend.text = element_text(size=12),
          legend.title = element_text(size=15)))

# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
plot_grid(enum_outcomes_plots, enum_outcomes_legend, ncol = 1, rel_heights = c(1, .1))

# Time on phone to complete CATI interview (consenting respondents) ------------ 
# Interview length
# Sex
# DRC
dur_drc_sex <- subset(Consented) %>%
  mutate(Phone.duration = as.numeric(Phone.duration)) %>%
  dplyr::select(Phone.duration, enumerator,Date.Interview,Resp.Sex)
dur_drc_sex$enumerator <- ordered(dur_drc_sex$enumerator, levels = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", 'E11', 'E12', 'E13', 'E14', 'E15','E16','E17','E18','E19','E20','E21','E22','E23','E24','E25','E26'))

dur_drc_sex_plot <- ggplot(data = dur_drc_sex, aes(x = enumerator, y = Phone.duration/60, fill = Resp.Sex)) +
  geom_boxplot() +
  theme_minimal() +
  ylab("Time to complete consented interview (minutes)") +xlab('Enumerator')+
  theme_bw() +
  labs(fill = "Respondent characteristics")+
  scale_fill_manual(values = c('#D2042D','#0671B7'))+
  scale_y_continuous(limits = c(0, 300)) 

# MW
dur_mw_sex <- subset(Consentedmw) %>%
  mutate(Phone.duration = as.numeric(Phone.duration)) %>%
  dplyr::select(Phone.duration, enumerator,Date.Interview,Resp.Sex)
dur_mw_sex$enumerator <- ordered(dur_mw_sex$enumerator, levels = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", 'E11', 'E12', 'E13', 'E14', 'E15','E16','E17','E18','E19','E20','E21','E22','E23','E24','E25','E26'))

dur_mw_sex_plot <- ggplot(data = dur_mw_sex, aes(x = enumerator, y = Phone.duration/60, fill = Resp.Sex)) +
  geom_boxplot() +
  theme_minimal() +
  ylab("Time to complete consented interview (minutes)") + xlab('Enumerator')+
  theme_bw() +
  labs(fill = "Respondent characteristics")+
  scale_fill_manual(values = c('#D2042D','#0671B7'))+
  scale_y_continuous(limits = c(0, 300)) 

# BF
Consentedrdd$phone_call_duration <- as.numeric(Consentedrdd$phone_call_duration)

dur_bf_sex <- subset(Consentedrdd) %>%
  filter(phone_call_duration>0) %>%
  mutate(Phone.duration = as.numeric(phone_call_duration)) %>%
  dplyr::select(phone_call_duration, enumerator,Date.Interview,Resp.Sex) %>%
  drop_na()
dur_bf_sex$enumerator <- ordered(dur_bf_sex$enumerator, levels = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", 'E11', 'E12', 'E13', 'E14', 'E15','E16','E17','E18','E19','E20','E21','E22','E23','E24','E25','E26'))

dur_bf_sex_plot <- ggplot(data = dur_bf_sex, aes(x = enumerator, y = phone_call_duration/60, fill = Resp.Sex)) +
  geom_boxplot() +
  theme_bw() +
  ylab("Time to complete consented interview (minutes)") + xlab('Enumerator')+
  theme() +
  labs(fill = "Respondent characteristics")+
  scale_fill_manual(values = c('#D2042D','#0671B7'))+
  scale_y_continuous(limits = c(0, 300)) 
dur_bf_sex_plot


# add the plots from the three countries together
# arrange the three plots into 4 quadrants
dur_sex_plot <- plot_grid(dur_mw_sex_plot + theme(legend.position="none", axis.text=element_text(size=8), axis.title.y = element_text(size = 8)), #remove legends first before combining legends
                                dur_drc_sex_plot + theme(legend.position="none", axis.text=element_text(size=8), axis.title.y = element_text(size = 8)),
                                dur_bf_sex_plot + theme(legend.position="none", axis.text=element_text(size=8), axis.title.y = element_text(size = 8)), 
                                labels = c("MW", "DRC", "BF"), #Title for each graph
                                label_size = 10)

# extract the legend from Malawi plots
dur_sex_legend <- get_legend(
  dur_mw_sex_plot + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = c(0.65, 5),
          legend.key.size = unit(1, 'cm'),
          legend.text = element_text(size=12),
          legend.title = element_text(size=15)))

# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
plot_grid(dur_sex_plot, dur_sex_legend, ncol = 1, rel_heights = c(1, .1))

## Urban vs Rural
# DRC
dur_drc_urban <- subset(Consented) %>%
  mutate(Phone.duration = as.numeric(Phone.duration)) %>%
  dplyr::select(Phone.duration, enumerator,Date.Interview,RuralUrban)  %>%
  drop_na()

dur_drc_urban$enumerator <- ordered(dur_drc_urban$enumerator, levels = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", 'E11', 'E12', 'E13', 'E14', 'E15','E16','E17','E18','E19','E20','E21','E22','E23','E24','E25','E26'))

dur_drc_urban_plot <- ggplot(data = dur_drc_urban, aes(x = enumerator, y = Phone.duration/60, fill = RuralUrban)) +
  geom_boxplot() +
  theme_minimal() +
  ylab("Time to complete consented interview (minutes)") +xlab('Enumerator')+
  theme_bw() +
  labs(fill = "Respondent characteristics")+
  scale_fill_manual(values = c("#D95F02","#7570B3")) +
  scale_y_continuous(limits = c(0, 290)) 
dur_drc_urban_plot

# MW
dur_mw_urban <- subset(Consentedmw) %>%
  mutate(Phone.duration = as.numeric(Phone.duration)) %>%
  dplyr::select(Phone.duration, enumerator,Date.Interview,RuralUrban)
dur_mw_urban$enumerator <- ordered(dur_mw_urban$enumerator, levels = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", 'E11', 'E12', 'E13', 'E14', 'E15','E16','E17','E18','E19','E20','E21','E22','E23','E24','E25','E26'))

dur_mw_urban_plot <- ggplot(data = dur_mw_urban, aes(x = enumerator, y = Phone.duration/60, fill = RuralUrban)) +
  geom_boxplot() +
  ylab("Time to complete consented interview (minutes)") + xlab('Enumerator')+
  theme_bw() +
  labs(fill = "Respondent characteristics")+
  scale_fill_manual(values = c("#D95F02","#7570B3"))+
  scale_y_continuous(limits = c(0, 290)) 
dur_mw_urban_plot

# BF
Consentedrdd$phone_call_duration <- as.numeric(Consentedrdd$phone_call_duration)

dur_bf_urban <- subset(Consentedrdd) %>%
  filter(phone_call_duration>0) %>%
  mutate(Phone.duration = as.numeric(phone_call_duration)) %>%
  dplyr::select(phone_call_duration, enumerator,Date.Interview,urban_rural) %>%
  drop_na()
dur_bf_urban$enumerator <- ordered(dur_bf_urban$enumerator, levels = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", 'E11', 'E12', 'E13', 'E14', 'E15','E16','E17','E18','E19','E20','E21','E22','E23','E24','E25','E26'))

dur_bf_urban_plot <- ggplot(data = dur_bf_urban, aes(x = enumerator, y = phone_call_duration/60, fill = urban_rural)) +
  geom_boxplot() +
  theme_bw() +
  ylab("Time to complete consented interview (minutes)") + xlab('Enumerator')+
  labs(fill = "Respondent characteristics")+
  scale_fill_manual(values = c("#D95F02","#7570B3"))+
  scale_y_continuous(limits = c(0, 290)) 
dur_bf_urban_plot


# add the plots from the three countries together
# arrange the three plots into 4 quadrants
dur_urban_plot <- plot_grid(dur_mw_urban_plot + theme(legend.position="none", axis.text=element_text(size=8), axis.title.y = element_text(size = 8)), #remove legends first before combining legends
                          dur_drc_urban_plot + theme(legend.position="none", axis.text=element_text(size=8), axis.title.y = element_text(size = 8)),
                          dur_bf_urban_plot + theme(legend.position="none", axis.text=element_text(size=8), axis.title.y = element_text(size = 8)), 
                          labels = c("MW", "DRC", "BF"), #Title for each graph
                          label_size = 10,
                          hjust = 0, vjust = 2.5)

# extract the legend from Malawi plots
dur_urban_legend <- get_legend(
  dur_mw_urban_plot + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = c(0.65, 5),
          legend.key.size = unit(1, 'cm'),
          legend.text = element_text(size=12),
          legend.title = element_text(size=15)))

# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
plot_grid(dur_urban_plot, dur_urban_legend, ncol = 1, rel_heights = c(1, .1))


# Daily time on the phone, per enumerator --------------------------------------
# DRC
daily_phone_drc <- data %>%
  group_by(enumerator, Date.Interview) %>%
  dplyr::summarise(timeperday = sum(Phone.duration)/60/60) %>%
  pivot_longer(timeperday, names_to = "timeonwhat",values_to = 'hours')

daily_phone_drc$enumerator <- ordered(daily_phone_drc$enumerator, levels = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10",
                                                   'E11', 'E12', 'E13', 'E14', 'E15', 'E16', 'E17', 'E18', 'E19', 'E20'))

daily_phone_drc_plot <- ggplot(daily_phone_drc) + geom_boxplot(aes(x = enumerator, y = hours, fill = timeonwhat), alpha = 0.8) +
  theme_bw() +
  xlab("") + ylab("Time per day (hours)") +
  theme(axis.text.x = element_text(angle = 0)) +
  scale_y_continuous(limits = c(0, 7.5)) +
  scale_x_discrete() +
  scale_fill_manual(values = c("#1B9E77")) +
  theme(legend.position="none")
daily_phone_drc_plot

# MW
# === Time on phone to complete CATI interview (consenting respondents)  
# === Interview length
daily_phone_mw <- datamw %>%
  mutate(Phone.duration = as.numeric(Phone.duration)) %>%
  group_by(enumerator, Date.Interview) %>%
  dplyr::summarise(timeperday = sum(Phone.duration)/60/60) %>%
  pivot_longer(timeperday, names_to = "timeonwhat",values_to = 'hours')

daily_phone_mw$enumerator <- ordered(daily_phone_mw$enumerator, levels = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", 'E11', 'E12', 'E13', 'E14', 'E15','E16','E17','E18','E19','E20','E21','E22','E23','E24','E25','E26'))

daily_phone_mw_plot <- ggplot(daily_phone_mw) + geom_boxplot(aes(x = enumerator, y = hours, fill = "#1B9E77")) +
  theme_bw() +
  xlab("") + ylab("Time on phone per day (hours)") +
  theme(axis.text.x = element_text(angle = 0)) +
  scale_y_continuous(limits = c(0, 7.5)) +
  scale_x_discrete() +
  scale_fill_manual(values = c("#1B9E77")) +
  theme(legend.position="none")
daily_phone_mw_plot

# BF
daily_phone_bf <- rdd.data %>%
  mutate(Phone.duration = as.numeric(phone_call_duration)) %>%
  filter(phone_call_duration>0) %>%
  group_by(enumerator, Date.Interview) %>%
  dplyr::summarise(timeperday = sum(Phone.duration)/60/60) %>%
  pivot_longer(timeperday, names_to = "timeonwhat",values_to = 'hours')

daily_phone_bf$enumerator <- ordered(daily_phone_bf$enumerator, levels = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", 'E11', 'E12', 'E13', 'E14', 'E15','E16','E17','E18','E19','E20','E21','E22','E23','E24','E25','E26'))

daily_phone_bf_plot <- ggplot(daily_phone_bf) + geom_boxplot(aes(x = enumerator, y = hours, fill = "#1B9E77")) +
  theme_bw() +
  xlab("") + ylab("Time on phone per day (hours)") +
  theme(axis.text.x = element_text(angle = 0)) +
  scale_y_continuous(limits = c(0, 7.5)) +
  scale_x_discrete() +
  scale_fill_manual(values = c("#1B9E77")) +
  theme(legend.position="none")
daily_phone_bf_plot

# add the plots from the three countries together
# arrange the three plots into 4 quadrants
daily_phone_plot <- plot_grid(daily_phone_mw_plot + theme(legend.position="none", axis.text=element_text(size=8), axis.title.y = element_text(size = 8)), #remove legends first before combining legends
                              daily_phone_drc_plot + theme(legend.position="none", axis.text=element_text(size=8), axis.title.y = element_text(size = 8)),
                              daily_phone_bf_plot + theme(legend.position="none", axis.text=element_text(size=8), axis.title.y = element_text(size = 8)), 
                            labels = c("MW", "DRC", "BF"), #Title for each graph
                            label_size = 9, 
                            hjust = 0, vjust = 2.3)
daily_phone_plot

# Time per module --------------------------------------------------------------
# DRC
d1 <- subset(Consented) %>%
  mutate(Elig.Time = as.numeric(Elig.Time)) %>%
  dplyr::select(Elig.Time, enumerator)  %>%
  drop_na() %>%
  group_by(enumerator) %>%
  mutate(mean_elig_time = mean(Elig.Time))

d1 <- aggregate(d1$Elig.Time,list(d1$enumerator),mean)

summary(d1)
d1$Group.1 <- ordered(d1$Group.1, levels = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", 'E11', 'E12', 'E13', 'E14', 'E15','E16','E17','E18','E19','E20','E21','E22','E23','E24','E25','E26'))

d1_plot <- ggplot(data = d1, aes(x = Group.1, y = x/60, fill='Group.1')) +
  geom_bar(stat = 'identity') +
  ylab("Time (mins)") +xlab('Enumerator')+
  theme_bw() +
  labs(fill = "Respondent characteristics")+
  scale_fill_manual(values = c("#1B9E77")) +
  theme(legend.position="none")
d1_plot


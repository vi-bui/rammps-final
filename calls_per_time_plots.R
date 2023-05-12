#_______________________________________________________________________________
#  Number of Calls based on the time of day
#  05/2023
#_______________________________________________________________________________

# Load Libraries ---------------------------------------------------------------
pkgs <- c('purrr','plyr','tidyverse', 'plotrix','lubridate','kableExtra','hrbrthemes','ggplot2','extrafont','float','reshape',
          'gridExtra','rsvg','png','devtools','readxl','date', 'ggpubr', 'tidyselect', 'httr', 'jsonlite', 'extrafont', 'colorspace',
          'ggrepel', 'forcats', 'ggpubr', 'readstata13', 'cowplot', 'scales')
lapply(pkgs, require, character.only = TRUE)

dark2pal <- c("#1B9E77", "#D95F02", "#7570B3", "#66A61E", "#E6AB02")


# Reading in data --------------------------------------------------------------

# Setting directories
r.functions.dir <- '/Users/lshvb5/Documents/rammps/'
dir.inputdata <- ('/Users/lshvb5/OneDrive - London School of Hygiene and Tropical Medicine/General/Partners/UNIKIN/SurveyCTO Audits/')


# DRC
Consented <- read.csv(paste0(dir.inputdata, "Clean data/Consented2022-08-23.csv"))%>%
  mutate(source = ifelse(grepl('Fer', Source.prov),'Feroxus',
                         ifelse(grepl('IVR', Source.prov),'IVR',NA)),
         Outcome2 = ifelse(Outcome2 =='NNA'| Outcome2 =='NR', 'NNA/NR', as.character(Outcome2)),
         RuralUrban = case_when(E4a_lab == 'City' ~ 'Urban',
                                E4a_lab == 'Town/Trading Centre' ~ 'Urban',
                                E4a_lab == 'Rural' ~ 'Rural'))

data <- read.csv(paste0(dir.inputdata, "Clean data/Data_Long2022-08-23.csv"))%>%
  mutate(source = ifelse(grepl('Fer', Source.prov),'Feroxus',
                         ifelse(grepl('IVR', Source.prov),'IVR',NA)),
         Outcome2 = ifelse(Outcome2 =='NNA'| Outcome2 =='NR', 'NNA/NR', as.character(Outcome2)),
         RuralUrban = case_when(E4a_lab == 'City' ~ 'Urban',
                                E4a_lab == 'Town/Trading Centre' ~ 'Urban',
                                E4a_lab == 'Rural' ~ 'Rural'))
dat.wide <- read.csv(paste0(dir.inputdata, "Clean data/Data_Wide2022-08-23.csv")) %>%
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
eval(parse(paste0(r.functions.dir,'CleaningScript_func_BF.R'), encoding="UTF-8"))
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

ehcvm.data <- listbf[[2]]
rdd.dat.wide <- listbf[[3]] %>%
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


ehcvm.dat.wide <- listbf[[4]]
bf.dat.wide <- listbf[[5]]
bf.data <- listbf[[6]]
Consentedrdd <- rdd.data %>%
  filter(Outcome == 'COMP') %>%
  dplyr::mutate(month.interview1 = lubridate::floor_date(as.Date(Date.Interview), 'month'),
                gender = case_when(is.na(gender) ~ 'Male', TRUE ~ as.character(gender)),
                Resp.Age.pyr = cut(as.numeric(age), c(14,19,29,39,49,59,64))
  )#month(Date.Interview),

Consentedehcvm <- ehcvm.data %>%
  filter(Outcome == 'COMP') %>%
  dplyr::mutate(month.interview1 = lubridate::floor_date(as.Date(Date.Interview), 'month'),#month(Date.Interview),
                month.interview = case_when(month.interview == '2022-01-01'~ 'Jan-22',
                                            month.interview == '2022-02-01'~ 'Feb-22',
                                            month.interview == '2022-03-01'~ 'Mar-22',
                                            month.interview == '2022-04-01'~'Apr-22',
                                            month.interview == '2022-05-01'~'May-22',
                                            month.interview == '2022-06-01'~'Jun-22',
                                            month.interview == '2022-07-01'~'Jul-22',
                                            month.interview == '2022-08-01'~'Aug-22',
                                            month.interview == '2022-09-01'~'Sep-22',
                                            month.interview == '2021-08-01'~'Aug-21',
                                            month.interview == '2021-09-01'~'Sep-21',
                                            month.interview == '2021-10-01'~'Oct-21',
                                            month.interview == '2021-11-01'~'Nov-21',
                                            month.interview == '2021-12-01'~'Dec-21'))
Consentedbf <- bf.data %>%
  filter(Outcome == 'COMP')

# Call outcome by time of day --------------------------------------------------

# Convert from 'Jan 31, 2022 8:50:52 PM' format to '2022-01-31 08:37:31' format
# Then split it into separate Date and Time variables
time_sort_mw <- Consentedmw %>%
  mutate(starttime = mdy_hms(starttime), 
         Date = as.Date(starttime), 
         Time = format(starttime, "%T"))

# Create df with just the info needed
time_mw<- time_sort_mw

# Creating the time categories
# Create breaks
breaks <- hour(hm("00:00", "4:00", "9:00",  "13:00", "18:00", "23:59"))
# Labels for the breaks
labels <- c("Night","Early Morning",  "Late Morning", "Afternoon", "Evening")
# Creating new column with the time categories
time_mw$Time_of_day <- cut(x=hour(time_mw$starttime), breaks = breaks, labels = labels, include.lowest=TRUE)
time_mw

# Ordering the time categories 
days_ordered <- c("Early Morning", "Late Morning", "Afternoon", "Evening", "Night")

# Create df grouped by Time_of_day
time_mw_comp <- time_mw %>%
  dplyr::group_by(Time_of_day, Outcome2) %>%
  mutate(Time_of_day =  factor(Time_of_day, levels = days_ordered)) %>% # rearrange plot to match 
  arrange(Time_of_day) %>%   
  dplyr::summarize(n = n()) %>%
  ungroup() %>%
  filter(Outcome2 == 'COMP')  %>%
  dplyr::mutate(totalcomp = sum(n),
                totalnums = nrow(bf.dat.wide),
                perc = round(n/totalnums*100,2),
                perccompletedpercallnum=round(n/totalcomp*100,1)) %>%
  ungroup() %>%
  dplyr::mutate(perccum = cumsum(perc),
                country = 'MW') 

# Check whether time of day categories matches up with the time
time_mw %>%
  select(Time, Time_of_day) %>%
  View()

# Get the number of calls from datamw dataset
datamw<- datamw %>% 
  mutate(starttime = mdy_hms(starttime), 
         Date = as.Date(starttime), 
         Time = format(starttime, "%T"))
datamw$Time_of_day <- cut(x=hour(datamw$starttime), breaks = breaks, labels = labels, include.lowest=TRUE)

datamw <- datamw %>%
  group_by(Time_of_day) %>%
  mutate(Time_of_day =  factor(Time_of_day, levels = days_ordered)) %>% # rearrange plot to match 
  arrange(Time_of_day) %>%  
  summarise(Calls= n())
time_mw_comp <- left_join(time_mw_comp, datamw)

# Change Time_of_day from factor to character format
time_mw_comp$Time_of_day <- as.character(time_mw_comp$Time_of_day)

# create plot
# time_mw_plot <- ggplot(data = subset(time_mw_comp)) +
#   geom_bar(aes(x=Time_of_day, y=Calls, fill=Time_of_day),stat = 'identity') +
#   scale_x_discrete(limits = days_ordered) +
#   labs(x="Time", y="Number of calls", fill='Time') +
#   theme_bw() + 
#   scale_y_continuous(limits = c(0, 75000)) +
#   scale_fill_brewer(palette = "Paired") 
# time_mw_plot

# Ratio to create second y-axis
ratio_cati_mw <- max(time_mw_comp$Calls)/max(time_mw_comp$perccum)
  
time_mw_plot <- ggplot(data = time_mw_comp, aes(group = 1)) + 
  geom_bar(aes(y = Calls, x = Time_of_day, fill = "#7570B3"), position= 'dodge',alpha = 0.75,stat = 'identity')+
  geom_line(aes(x = Time_of_day, y = perccum*ratio_cati_mw, color = '#D95F02'), size = 0.8) + 
  geom_point(aes(x = Time_of_day, y =  perccum*ratio_cati_mw, color = '#D95F02'), size = 2) +
  xlab('Call attempt order') +
  theme_bw() +
  scale_fill_manual(values = c("#7570B3", "#7570B3")) +
  scale_color_manual(values = c('#D95F02','#D95F02')) +
  theme(axis.title.y = element_text(color = "#7570B3", size=12),
        axis.title.y.right = element_text(color = "#D95F02", size=12),
        legend.position = 'none')+
  scale_x_discrete(limits = days_ordered) +
  scale_y_continuous(
    name = "Phone calls made ",
    sec.axis = sec_axis((~ . / ratio_cati_mw), name ='Cumulative percentage of phone \n numbers with completed CATI'),
    limits = c(0, 75000))
time_mw_plot


# DRC
time_sort_drc <- Consented %>%
  mutate(starttime = mdy_hms(starttime), 
         Date = as.Date(starttime), 
         Time = format(starttime, "%T"))

time_drc <- time_sort_drc

# Creating new column with the time categories
time_drc$Time_of_day <- cut(x=hour(time_drc$starttime), breaks = breaks, labels = labels, include.lowest=TRUE)
time_drc

# Create df grouped by Time_of_day
time_drc_comp <- time_drc %>%
  dplyr::group_by(Time_of_day, Outcome2) %>% 
  mutate(Time_of_day =  factor(Time_of_day, levels = days_ordered)) %>% # rearrange plot to match 
  arrange(Time_of_day) %>%   
  dplyr::summarize(n = n()) %>%
  ungroup() %>%
  filter(Outcome2 == 'COMP')  %>%
  dplyr::mutate(totalcomp = sum(n),
                totalnums = nrow(bf.dat.wide),
                perc = round(n/totalnums*100,2),
                perccompletedpercallnum=round(n/totalcomp*100,1)) %>%
  ungroup() %>%
  dplyr::mutate(perccum = cumsum(perc),
                country = 'DRC') 

# get the number of calls from datamw dataset
datadrc<- data %>% 
  mutate(starttime = mdy_hms(starttime), 
         Date = as.Date(starttime), 
         Time = format(starttime, "%T"))
datadrc$Time_of_day <- cut(x=hour(datadrc$starttime), breaks = breaks, labels = labels, include.lowest=TRUE)
# time_drc_comp$Calls <- c(table(datadrc$Time_of_day))

datadrc <- datadrc %>%
  group_by(Time_of_day) %>%
  mutate(Time_of_day =  factor(Time_of_day, levels = days_ordered)) %>% # rearrange plot to match 
  arrange(Time_of_day) %>%  
  summarise(Calls= n())
time_drc_comp <- left_join(time_drc_comp, datadrc)

# change Time_of_day from factor to character format
time_drc_comp$Time_of_day <- as.character(time_drc_comp$Time_of_day)

# create plot
# time_drc_plot <- ggplot(data = time_drc_comp) +
#   geom_bar(aes(x=Time_of_day, y= Calls,fill=Time_of_day),stat = 'identity') +
#   scale_x_discrete(limits = days_ordered) +
#   labs(x="Time", y="Number of calls") +
#   theme_bw() + 
#   scale_y_continuous(limits = c(0, 75000)) +
#   scale_fill_brewer(palette = "Paired") +
#   theme(legend.position="none")
# time_drc_plot

# Ratio to create second y-axis
ratio_cati_drc<- max(time_drc_comp$Calls)/max(time_drc_comp$perccum)

time_drc_plot <- ggplot(data = time_drc_comp, aes(group = 1)) + 
  geom_bar(aes(y = Calls, x = Time_of_day, fill = "#7570B3"), position= 'dodge',alpha = 0.75,stat = 'identity')+
  geom_line(aes(x = Time_of_day, y = perccum*ratio_cati_drc, color = '#D95F02'), size = 0.8) + 
  geom_point(aes(x = Time_of_day, y =  perccum*ratio_cati_drc, color = '#D95F02'), size = 2) +
  xlab('Call attempt order') +
  theme_bw() +
  scale_fill_manual(values = c("#7570B3", "#7570B3")) +
  scale_color_manual(values = c('#D95F02','#D95F02')) +
  theme(axis.title.y = element_text(color = "#7570B3", size=12),
        axis.title.y.right = element_text(color = '#D95F02', size=12),
        legend.position = 'none')+
  scale_x_discrete(limits = days_ordered) +
  scale_y_continuous(
    name = "Phone calls made ",
    sec.axis = sec_axis((~ . / ratio_cati_drc), name ='Cumulative percentage of phone \n numbers with completed CATI'),
    limits = c(0, 75000))
time_drc_plot

# BF
#Split date and time into different columns
Consentedrdd$Date <- as.Date(Consentedrdd$instance_time) 
Consentedrdd$Time <- format(as.POSIXct(Consentedrdd$instance_time), format = "%H:%M:%S") 

time_bf <- Consentedrdd

# Creating new column with the time categories
time_bf$Time_of_day <- cut(x=hour(time_bf$instance_time), breaks = breaks, labels = labels, include.lowest=TRUE)


# create df grouped by Time_of_day
time_bf_comp <- time_bf %>%
  dplyr::group_by(Time_of_day, Outcome) %>%
  mutate(Time_of_day =  factor(Time_of_day, levels = days_ordered)) %>% # rearrange plot to match 
  arrange(Time_of_day) %>%   
  dplyr::summarize(n = n()) %>%
  ungroup() %>%
  filter(Outcome == 'COMP')  %>%
  dplyr::mutate(totalcomp = sum(n),
                totalnums = nrow(bf.dat.wide),
                perc = round(n/totalnums*100,2),
                perccompletedpercallnum=round(n/totalcomp*100,1)) %>%
  ungroup() %>%
  dplyr::mutate(perccum = cumsum(perc),
                country = 'BF') 

# get the number of calls from datamw dataset
databf<- cut(x=hour(bf.data$instance_time), breaks = breaks, labels = labels, include.lowest=TRUE)
# time_bf_comp$Calls <- c(table(databf))


databf <- data.frame(databf)
bftimecat <- databf %>%
  mutate(Time_of_day = databf)  %>%
  group_by(Time_of_day) %>%
  mutate(Time_of_day = factor(Time_of_day, levels = days_ordered)) %>% # rearrange plot to match 
  arrange(Time_of_day) %>%  
  summarise(Calls= n()) 
time_bf_comp <- left_join(time_bf_comp, bftimecat)

# change Time_of_day from factor to character format
time_bf_comp$Time_of_day <- as.character(time_bf_comp$Time_of_day)

# Ratio to create second y-axis
ratio_cati_bf <- max(time_bf_comp$Calls)/max(time_bf_comp$perccum)

time_bf_plot <- ggplot(data = time_bf_comp, aes(group = 1)) + 
  geom_line(aes(x = Time_of_day, y = perccum*ratio_cati_bf, color = '#D95F02'), size = 0.8) + 
  geom_point(aes(x = Time_of_day, y =  perccum*ratio_cati_bf, color = '#D95F02'), size = 2) +
  geom_bar(aes(y = Calls, x = Time_of_day, fill = "#7570B3"), position= 'dodge',alpha = 0.75,stat = 'identity')+
  xlab('Call attempt order') +
  theme_bw() +
  scale_fill_manual(values = c("#7570B3", "#7570B3")) +
  scale_color_manual(values = c('#D95F02','#D95F02')) +
  theme(axis.title.y = element_text(color = "#7570B3", size=12),
        axis.title.y.right = element_text(color = "#D95F02", size=12),
        legend.position = 'none')+
  scale_x_discrete(limits = days_ordered) +
  scale_y_continuous(
    name = "Phone calls made ",
    sec.axis = sec_axis((~ . / ratio_cati_bf), name ='Cumulative percentage of phone \n numbers with completed CATI'),
    limits = c(0, 75000)) 
time_bf_plot

# arrange the three plots into 4 quadrants
plots_time <- plot_grid(time_mw_plot + theme(legend.position="none", axis.text=element_text(size=8)), #remove legends first before combining legends
                        time_drc_plot+ theme(legend.position="none", axis.text=element_text(size=8),
                                             axis.ticks.y = element_blank(), # removes the x-axis scales
                                             axis.title.y = element_blank(), # removes the x-axis title
                                             axis.ticks.y.right = element_blank(), # removes the right x-axis scales
                                             axis.title.y.right = element_blank()), # removes the right x-axis title),
                        time_bf_plot + theme(legend.position="none", axis.text=element_text(size=8),
                                             axis.ticks.y = element_blank(), # removes the x-axis scales
                                             axis.title.y = element_blank(), # removes the x-axis title
                                             axis.ticks.y.right = element_blank(), # removes the right x-axis scales
                                             axis.title.y.right = element_blank()), # removes the right x-axis title), 
                        labels = c("MW", "DRC", "BF"), #Title for each graph
                        label_size = 10
)
plots_time

# extract the legend from Malawi plots
legend_time_mw_plot <- get_legend(
  time_mw_plot + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = c(0.65, 5),
          legend.key.size = unit(1, 'cm'),
          legend.text = element_text(size=12),
          legend.title = element_text(size=15))
)

# Adding subtitle with the eligibility unknown outcomes
sublegend_time_mw_plot<- add_sub(plots_time, "Early Morning = 5:00 - 8:59 \n Late Morning = 9:00 - 11:59 \n Afternoon = 12:00 - 18:59 \n Evening = 19:00 - 23:59 \n Night = 00:00 - 4:59", x = 0.85, y=3, size=11)

# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
plot_grid(sublegend_time_mw_plot, legend_time_mw_plot, ncol = 1, rel_heights = c(1, .1))

# Call outcome by time of day --------------------------------------------------
# Based on sex -----------------------------------------------------------------

# MW

# Create df grouped by Time_of_day
time_mw_sex <- time_mw %>%
  dplyr::group_by(Time_of_day, Outcome2, Resp.Sex, .drop = FALSE) %>%
  mutate(Time_of_day =  factor(Time_of_day, levels = days_ordered)) %>% 
  arrange(Time_of_day) %>%   
  dplyr::summarize(n = n()) 

# Work out number of completed interviews and percentages
time_mw_sex <- time_mw_sex %>%
  filter(Outcome2 == 'COMP')  %>%
  dplyr::mutate(totalcomp = sum(n),
                totalnums = nrow(bf.dat.wide),
                perc = round(n/totalnums*100,2),
                perccompletedpercallnum=round(n/totalcomp*100,1)) %>%
  group_by(Resp.Sex) %>%
  dplyr::mutate(perccum = cumsum(perc),
                country = 'MW')

# Get the number of calls from datamw dataset
datamw_sex <- datamw %>% 
  select(Resp.Sex, starttime)  %>%
  drop_na(Resp.Sex, starttime) %>%
  dplyr::group_by(Resp.Sex) %>%
  mutate(starttime = mdy_hms(starttime), 
         Date = as.Date(starttime), 
         Time = format(starttime, "%T"))
# Add time of day categories
datamw_sex$Time_of_day <- cut(x=hour(datamw_sex$starttime), breaks = breaks, labels = labels, include.lowest=TRUE)
# time_mw_sex$Calls <- c(table(datamw_sex$Time_of_day, datamw_sex$Resp.Sex))

mwtimecatsex <- datamw_sex %>%
  group_by(Time_of_day) %>%
  mutate(Time_of_day =  factor(Time_of_day, levels = days_ordered)) %>% # rearrange plot to match 
  arrange(Time_of_day) %>%  
  ungroup() %>%  
  group_by(Resp.Sex, Time_of_day)  %>%
  summarise(Calls=n())

mw_time_gender <- left_join(time_mw_sex, mwtimecatsex)

ratio_cati_sex_mw <- max(mw_time_gender$Calls)/max(mw_time_gender$perccum)
sex_legend_title <- "Place of residence"

calls.time.sex.mw <- ggplot(data = mw_time_gender) + 
  geom_line(aes(x = Time_of_day, y = perccum*ratio_cati_sex_mw, group=Resp.Sex, colour=Resp.Sex), size = 0.8) + 
  geom_point(aes(x = Time_of_day, y =  perccum*ratio_cati_sex_mw, group=Resp.Sex, colour=Resp.Sex), size = 2) +
  geom_bar(aes(y = Calls, x = Time_of_day, fill=Resp.Sex), position= 'dodge',alpha = 0.75,stat = 'identity')+
  xlab('Call attempt order') +
  theme_bw() +
  scale_fill_manual(sex_legend_title, values = c('#D2042D','#0671B7')) +
  scale_color_manual(sex_legend_title, values = c('#D2042D','#0671B7')) +
  theme(axis.title.y = element_text( size=12),
        axis.title.y.right = element_text( size=12))+
  scale_y_continuous(
    name = "Phone calls made ",
    limits = c(0, 8700),
    sec.axis = sec_axis((~ . / ratio_cati_sex_mw), name ='Cumulative percentage of phone \n numbers with completed CATI'))
calls.time.sex.mw

# DRC
# create df grouped by Time_of_day
time_drc_sex <- time_drc %>%
  dplyr::group_by(Time_of_day, Outcome2, Resp.Sex, .drop = FALSE) %>%
  mutate(Time_of_day =  factor(Time_of_day, levels = days_ordered)) %>% # rearrange plot to match 
  arrange(Time_of_day) %>%   
  dplyr::summarize(n = n()) 


new_df <- data.frame(Time_of_day  = 'Night',
                     Outcome2 = 'COMP',
                     Resp.Sex = 'Male',
                     n = 0)

time_drc_sex <- rbind(time_drc_sex, new_df)

time_drc_sex <- time_drc_sex %>%
  filter(Outcome2 == 'COMP')  %>%
  dplyr::mutate(totalcomp = sum(n),
                totalnums = nrow(bf.dat.wide),
                perc = round(n/totalnums*100,2),
                perccompletedpercallnum=round(n/totalcomp*100,1)) %>%
  group_by(Resp.Sex) %>%
  dplyr::mutate(perccum = cumsum(perc))

# get the number of calls from datamw dataset
drctimecatsex <- datadrc %>%
  group_by(Time_of_day) %>%
  mutate(Time_of_day =  factor(Time_of_day, levels = days_ordered)) %>% # rearrange plot to match 
  arrange(Time_of_day) %>%  
  ungroup() %>%  
  group_by(Resp.Sex, Time_of_day)  %>%
  drop_na(Resp.Sex) %>%
  summarise(Calls=n())

time_drc_sex <- left_join(time_drc_sex, drctimecatsex)

# Ratio for second Y-axis
ratio_cati_sex_drc <- max(time_drc_sex$Calls)/max(time_drc_sex$perccum)

# Plot figure
calls.time.sex.drc <- ggplot(data = time_drc_sex) + 
  geom_line(aes(x = Time_of_day, y = perccum*ratio_cati_sex_drc, group=Resp.Sex, colour=Resp.Sex), size = 0.8) + 
  geom_point(aes(x = Time_of_day, y =  perccum*ratio_cati_sex_drc, group=Resp.Sex, colour=Resp.Sex), size = 2) +
  geom_bar(aes(y = Calls, x = Time_of_day, fill=Resp.Sex), position= 'dodge',alpha = 0.75,stat = 'identity')+
  xlab('Call attempt order') +
  theme_bw() +
  scale_fill_manual(values = c('#D2042D','#0671B7')) +
  scale_color_manual(values = c('#D2042D','#0671B7')) +
  theme(axis.title.y = element_text( size=12),
        axis.title.y.right = element_text( size=12))+
  scale_x_discrete(limits = days_ordered, labels = label_wrap(5)) +
  scale_y_continuous(
    name = "Phone calls made ",
    limits = c(0, 8700),
    sec.axis = sec_axis((~ . / ratio_cati_sex_drc), name ='Cumulative percentage of phone \n numbers with completed CATI'))
calls.time.sex.drc

# BF
# create df grouped by Time_of_day
time_bf_sex <- time_bf %>%
  dplyr::group_by(Time_of_day, Outcome,  gender, .drop = FALSE) %>%
  mutate(Time_of_day =  factor(Time_of_day, levels = days_ordered)) %>% # rearrange plot to match 
  arrange(Time_of_day) %>%   
  dplyr::summarize(n = n()) %>%
  ungroup() %>%
  filter(Outcome == 'COMP')  %>%
  dplyr::mutate(totalcomp = sum(n),
                totalnums = nrow(bf.dat.wide),
                perc = round(n/totalnums*100,2),
                perccompletedpercallnum=round(n/totalcomp*100,1)) %>%
  group_by(gender) %>%
  dplyr::mutate(perccum = cumsum(perc)) 

# get the number of calls from datamw datasetZ
# databf_sex <- bf.data %>% 
#   mutate(starttime = instance_time)
# databf_sex$Time_of_day <- cut(x=hour(databf_sex$starttime), breaks = breaks, labels = labels, include.lowest=TRUE)

databf_sex <- bf.data %>% 
  select(gender, instance_time)  %>%
  drop_na(gender, instance_time) %>%
  dplyr::group_by(gender) %>%
  mutate(starttime = instance_time,
         gender = case_when(gender == 1 ~ 'Male',
                            gender == 2 ~ 'Female'))
databf_sex$Time_of_day <- cut(x=hour(databf_sex$starttime), breaks = breaks, labels = labels, include.lowest=TRUE)
# time_bf_sex$Calls <- c(table(why$Time_of_day, why$gender))

databf_sex %>%
  group_by(gender, Time_of_day) %>%
  summarise(n=n())

bftimecatsex <- databf_sex %>%
  dplyr::group_by(Time_of_day, gender, .drop = FALSE) %>%
  mutate(Time_of_day =  factor(Time_of_day, levels = days_ordered)) %>% # rearrange plot to match 
  group_by(gender, Time_of_day)  %>%
  summarise(Calls=n())

bf_time_gender <- left_join(time_bf_sex, bftimecatsex)

# Get ratio for second y-axis
ratio_cati_sex_bf <- max(bf_time_gender$Calls)/max(bf_time_gender$perccum)

# Plot figure
calls.time.sex.bf <- ggplot(data = bf_time_gender) + 
  geom_line(aes(x = Time_of_day, y = perccum*ratio_cati_sex_bf, group=gender, colour=gender), size = 0.8) + 
  geom_point(aes(x = Time_of_day, y =  perccum*ratio_cati_sex_bf, group=gender, colour=gender), size = 2) +
  geom_bar(aes(y = Calls, x = Time_of_day, fill=gender), position= 'dodge',alpha = 0.75,stat = 'identity')+
  xlab('Call attempt order') +
  theme_bw() +
  scale_fill_manual(values = c('#D2042D','#0671B7')) +
  scale_color_manual(values = c('#D2042D','#0671B7')) +
  theme(axis.title.y = element_text( size=12),
        axis.title.y.right = element_text( size=12))+
  scale_x_discrete(limits = days_ordered, labels = label_wrap(5)) +
  scale_y_continuous(
    name = "Phone calls made ",
    limits = c(0, 8700),
    sec.axis = sec_axis((~ . / ratio_cati_sex_bf), name ='Cumulative percentage of phone \n numbers with completed CATI'))
calls.time.sex.bf

# arrange the three plots into 4 quadrants
plots_time_sex <- plot_grid(calls.time.sex.mw + theme(legend.position="none", axis.text=element_text(size=12)), #remove legends first before combining legends
                        calls.time.sex.drc+ theme(legend.position="none", axis.text=element_text(size=12),
                                                 
                                                  axis.ticks.y = element_blank(), # removes the x-axis scales
                                                  axis.title.y = element_blank(), # removes the x-axis title
                                                  axis.ticks.y.right = element_blank(), # removes the right x-axis scales
                                                  axis.title.y.right = element_blank()),
                        calls.time.sex.bf + theme(legend.position="none", axis.text=element_text(size=12),
                                                 
                                                  axis.ticks.y = element_blank(), # removes the x-axis scales
                                                  axis.title.y = element_blank(), # removes the x-axis title
                                                  axis.ticks.y.right = element_blank(), # removes the right x-axis scales
                                                  axis.title.y.right = element_blank()), 
                        labels = c("MW", "DRC", "BF"), #Title for each graph
                        label_size = 10
)

# extract the legend from Malawi plots
legend_time_mw_sex <- get_legend(
  calls.time.sex.mw + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = c(0.65, 5),
          legend.key.size = unit(1, 'cm'),
          legend.text = element_text(size=12),
          legend.title = element_text(size=15))
)

# Adding subtitle with the eligibility unknown outcomes
sublegend_time_sex<- add_sub(plots_time_sex, "Early Morning = 5:00 - 8:59 \n Late Morning = 9:00 - 11:59 \n Afternoon = 12:00 - 18:59 \n Evening = 19:00 - 23:59 \n Night = 00:00 - 4:59", x = 0.85, y=3, size=11)

# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
plot_grid(sublegend_time_sex, legend_time_mw_sex, ncol = 1, rel_heights = c(1, .1))

# Call outcome by time of day --------------------------------------------------
# Based on urban vs rural ------------------------------------------------------

# MW
# create df grouped by Time_of_day
time_mw_urban <- time_mw %>%
  dplyr::group_by(Time_of_day, Outcome2, RuralUrban, .drop = FALSE) %>%
  dplyr::summarize(n = n()) 


# get the number of calls from datamw dataset
datamw_urban <- datamw %>% 
  select(RuralUrban, starttime)  %>%
  drop_na(RuralUrban, starttime) %>%
  dplyr::group_by(RuralUrban) %>%
  mutate(starttime = mdy_hms(starttime), 
         Date = as.Date(starttime), 
         Time = format(starttime, "%T"))
# important df
datamw_urban$Time_of_day <- cut(x=hour(datamw_urban$starttime), breaks = breaks, labels = labels, include.lowest=TRUE)
# time_mw_sex$Calls <- c(table(datamw_sex$Time_of_day, datamw_sex$Resp.Sex))

calls_rural_mw <- datamw_urban %>%
  group_by(RuralUrban, Time_of_day)  %>%
  summarise(Calls=n())

mw_time_urban <- left_join(datamw_urban, calls_rural_mw)

legend_title <- "Place of residence"

calls.time.urban.mw <- ggplot(data = mw_time_urban) +
  geom_bar(aes(x=Time_of_day, y=Calls, fill=RuralUrban ),stat = 'identity', position = 'dodge') +
  scale_x_discrete(limits = days_ordered, labels = label_wrap(5)) +
  labs(x="Time", y="Number of calls", fill='Place of residennce') +
  theme_bw() + 
  scale_fill_manual(legend_title,values = c("#66A61E","#D95F02")) +
  scale_y_continuous(limits = c(0, 12500))
calls.time.urban.mw


# DRC
# create df grouped by Time_of_day
time_drc_urban <- time_drc %>%
  dplyr::group_by(Time_of_day, Outcome2, RuralUrban) %>%
  drop_na(RuralUrban)%>%
  dplyr::summarize(n = n()) %>%
  dplyr::mutate(
    country = 'DRC') 

# get the number of calls from datamw dataset
calls_rural <- datadrc %>%
  group_by(RuralUrban, Time_of_day) %>%
  drop_na(RuralUrban) %>%
  summarise(Calls=n())

time_drc_urbanrural <- left_join(time_drc_urban, calls_rural)
calls.time.urban.drc <- ggplot(data = time_drc_urbanrural) +
  geom_bar(aes(x=Time_of_day, y=Calls, fill=RuralUrban ),stat = 'identity', position = 'dodge') +
  scale_x_discrete(limits = days_ordered, labels = label_wrap(5)) +
  labs(x="Time", y="Number of calls", fill='Place of residence') +
  theme_bw() + 
  scale_fill_manual(legend_title,values = c("#66A61E","#D95F02")) +
  scale_y_continuous(limits = c(0, 12500))
calls.time.urban.drc

# BF
# create df grouped by Time_of_day
time_bf_urban <- time_bf %>%
  dplyr::group_by(Time_of_day, Outcome, urban_rural) %>%
  drop_na(urban_rural) %>%
  filter(Outcome == 'COMP')  %>%
  dplyr::summarize(n = n()) %>%
  dplyr::mutate(country = 'BF') 

# get call data
calls_rural_bf <- bf.data %>% 
  select(urban_rural, instance_time)  %>%
  drop_na(urban_rural, instance_time) %>%
  dplyr::group_by(urban_rural) %>%
  mutate(starttime = instance_time)
# get number of calls based on the time of day
calls_rural_bf$Time_of_day <- cut(x=hour(calls_rural_bf$starttime), breaks = breaks, labels = labels, include.lowest=TRUE)
# time_bf_sex$Calls <- c(table(why$Time_of_day, why$gender))

# collapse the data based on urban_rural and Time_of_day
calls_rural_bf_fin <- calls_rural_bf %>%
  group_by(urban_rural, Time_of_day)  %>%
  summarise(Calls=n())

time_bf_urban <- left_join(time_bf_urban, calls_rural_bf_fin)

calls.time.urban.bf <- ggplot(data = time_bf_urban) +
  geom_bar(aes(x=Time_of_day, y=Calls, fill=urban_rural ),stat = 'identity', position = 'dodge') +
  scale_x_discrete(limits = days_ordered, labels = label_wrap(5)) +
  labs(x="Time", y="Number of calls", fill='Place of residence') +
  theme_bw() + 
  scale_fill_manual(legend_title,values = c("#66A61E","#D95F02")) +
  scale_y_continuous(limits = c(0, 12500))
calls.time.urban.bf

# add the plots from the three countries together
# arrange the three plots into 4 quadrants
plots_time_urban <- plot_grid(calls.time.urban.mw + theme(legend.position="none", axis.text=element_text(size=12)), #remove legends first before combining legends
                            calls.time.urban.drc + theme(legend.position="none", axis.text=element_text(size=12)),
                            calls.time.urban.bf + theme(legend.position="none", axis.text=element_text(size=12)), 
                            labels = c("MW", "DRC", "BF"), #Title for each graph
                            label_size = 10
)

# extract the legend from Malawi plots
legend_time_mw_urban <- get_legend(
  calls.time.urban.mw + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = c(0.65, 5),
          legend.key.size = unit(1, 'cm'),
          legend.text = element_text(size=12),
          legend.title = element_text(size=15))
)

# Adding subtitle with the eligibility unknown outcomes
sublegend_time_urban<- add_sub(plots_time_urban, "Early Morning = 5:00 - 8:59 \n Late Morning = 9:00 - 11:59 \n Afternoon = 12:00 - 18:59 \n Evening = 19:00 - 23:59 \n Night = 00:00 - 4:59", x = 0.85, y=3, size=11)

# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
plot_grid(sublegend_time_urban, legend_time_mw_urban, ncol = 1, rel_heights = c(1, .1))

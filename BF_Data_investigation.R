# Burkina Faso data quality investigation 
library(tidyverse)

# r.functions.dir <- 'C:/Users/KellyMcCain/London School of Hygiene and Tropical Medicine/rammps/'
r.functions.dir <- '/Users/lshvb5/Documents/rammps/'
# dir.input <- "C:/Users/KellyMcCain/OneDrive - London School of Hygiene and Tropical Medicine/Documents/General/Partners/Burkina/Survey Data/"
dir.input <- ('/Users/lshvb5/OneDrive - London School of Hygiene and Tropical Medicine/General/Partners/UNIKIN/SurveyCTO Audits/')


eval(parse(paste0(r.functions.dir,'CleaningScript_func_BF.R'), encoding="UTF-8"))
listbf <- clean_bf()

rdd.data <- listbf[[1]]
ehcvm.data <- listbf[[2]]
rdd.dat.wide <- listbf[[3]]
ehcvm.dat.wide <- listbf[[4]]
bf.dat.wide <- listbf[[5]]
bf.data <- listbf[[6]]
Consentedrdd <- rdd.data %>%
  filter(Outcome == 'COMP') %>%
  dplyr::mutate(month.interview1 = lubridate::floor_date(as.Date(Date.Interview), 'month'),#month(Date.Interview),
                month.interview = case_when(month.interview == '2021-08-01'~'Aug-21',
                                            month.interview == '2021-09-01'~'Sep-21',
                                            month.interview == '2021-10-01'~'Oct-21',
                                            month.interview == '2021-11-01'~'Nov-21',
                                            month.interview == '2021-12-01'~'Dec-21',
                                            month.interview == '2022-01-01'~'Jan-22',
                                            month.interview == '2022-02-01'~'Feb-22',
                                            month.interview == '2022-03-01'~'Mar-22',
                                            month.interview == '2022-04-01'~'Apr-22',
                                            month.interview == '2022-05-01'~'May-22',
                                            month.interview == '2022-06-01'~'Jun-22',
                                            month.interview == '2022-07-01'~'Jul-22',
                                            month.interview == '2022-08-01'~'Aug-22',
                                            month.interview == '2022-09-01'~'Sep-22',
                                            month.interview == '2022-10-01'~'Oct-22',
                                            month.interview == '2022-11-01'~'Nov-22',
                                            month.interview == '2022-12-01'~'Dec-22',
                                            month.interview == '2023-01-01'~'Jan-23',
                                            month.interview == '2023-02-01'~'Feb-23',
                                            month.interview == '2023-03-01'~'Mar-23',
                                            month.interview == '2023-04-01'~'Apr-23',
                                            month.interview == '2023-05-01'~'May-23',
                                            month.interview == '2023-06-01'~'Jun-23'))
Consentedehcvm <- ehcvm.data %>%
  filter(Outcome == 'COMP') %>%
  dplyr::mutate(month.interview1 = lubridate::floor_date(as.Date(Date.Interview), 'month'),#month(Date.Interview),
                month.interview = case_when(month.interview == '2021-08-01'~'Aug-21',
                                            month.interview == '2021-09-01'~'Sep-21',
                                            month.interview == '2021-10-01'~'Oct-21',
                                            month.interview == '2021-11-01'~'Nov-21',
                                            month.interview == '2021-12-01'~'Dec-21',
                                            month.interview == '2022-01-01'~'Jan-22',
                                            month.interview == '2022-02-01'~'Feb-22',
                                            month.interview == '2022-03-01'~'Mar-22',
                                            month.interview == '2022-04-01'~'Apr-22',
                                            month.interview == '2022-05-01'~'May-22',
                                            month.interview == '2022-06-01'~'Jun-22',
                                            month.interview == '2022-07-01'~'Jul-22',
                                            month.interview == '2022-08-01'~'Aug-22',
                                            month.interview == '2022-09-01'~'Sep-22',
                                            month.interview == '2022-10-01'~'Oct-22',
                                            month.interview == '2022-11-01'~'Nov-22',
                                            month.interview == '2022-12-01'~'Dec-22',
                                            month.interview == '2023-01-01'~'Jan-23',
                                            month.interview == '2023-02-01'~'Feb-23',
                                            month.interview == '2023-03-01'~'Mar-23',
                                            month.interview == '2023-04-01'~'Apr-23',
                                            month.interview == '2023-05-01'~'May-23',
                                            month.interview == '2023-06-01'~'Jun-23'))
Consentedbf <- bf.data %>%
  filter(Outcome == 'COMP')
Consentedbf$dur <- ifelse(Consentedbf$phone_call_duration=='0','0','>0')

# Phone call duration of 0
rdd0 <- Consentedrdd %>% filter(phone_call_duration == '0') # 3384 w/ 0
Consentedrdd$dur <- ifelse(Consentedrdd$phone_call_duration=='0','0','>0')
ggplot() + 
  geom_histogram(data = Consentedrdd, aes(x = users, group = dur, fill = dur), stat = 'count', position = 'dodge') +
  theme(axis.text.x = element_text(angle = 90))

ehcvm0 <- Consentedehcvm %>% filter(phone_call_duration == '0') # 2042 w/ 0
Consentedehcvm$dur <- ifelse(Consentedehcvm$phone_call_duration=='0','0','>0')
ggplot() + 
  geom_histogram(data = Consentedehcvm, aes(x = users, group = dur, fill = dur), stat = 'count', position = 'dodge') +
  theme(axis.text.x = element_text(angle = 90))

# Checking teh distrubiton of ages, region, education, etc between two groups
Consentedbf <- Consentedbf %>%
  mutate(Sex = case_when(gender==1 ~ "Men",
                         gender==2 ~ "Women",
                         is.na(gender)~ "Men")) %>%
  mutate(Education = case_when(education==1 ~ "Primaire",
                               education==2 ~ "Secondaire",
                               education==3 ~ "Superieur"))%>%
  mutate(Vaccination = case_when(vax==1 ~ "Une dose",
                                 vax==2 ~ "Secondaire",
                                 vax==3 ~ "Superieur"))

Consentedbf$vax

p1 <- ggplot(data = Consentedbf) + 
  geom_density( aes(x = region, group = dur, fill = dur),alpha = 0.4,position = 'dodge') + 
  theme(axis.text.x = element_text(angle = 90))

p2 <- ggplot(data = Consentedbf) + 
  geom_bar( aes(group = (Sex), x = dur, fill = Sex), position = 'fill')
p3 <- ggplot(data = Consentedbf %>% filter(age < 100)) + 
  geom_density( aes(x = age, group = dur, fill = dur),position = 'dodge', alpha = .4)

p4 <- ggplot(data = Consentedbf %>% filter(age < 100)) + 
  geom_bar( aes(group = Education, x = dur, fill = Education),position = 'fill')

p5 <- ggplot(data = Consentedbf %>% filter(age < 100)) + 
  geom_bar( aes(group = as.factor(marital), x = dur, fill = as.factor(marital)),position = 'fill')
p5a <- ggplot(data = Consentedbf) + 
  geom_bar( aes(x = survey, group = dur, fill = dur), position = 'fill') + 
  theme(axis.text.x = element_text(angle = 90))
# covid vax
p8 <- ggplot(data = Consentedbf) + 
  geom_bar( aes(group = Vaccination, x = dur, fill = Vaccination), position = 'fill')
p8
library(patchwork)

(p1+p3+p8)/(p2+p4+p5)

# by date 
p6 <- ggplot(data = Consentedbf) + 
  geom_density( aes(x = as.Date(Date.Interview), group = dur, fill = dur),alpha = 0.4,position = 'dodge') + 
  scale_x_date(breaks = '2 weeks') +
  theme(axis.text.x = element_text(angle = 90))
p6
# by enumerator
p7 <- ggplot(data = Consentedbf) + 
  geom_bar( aes(x = users, group = dur, fill = dur),alpha = 0.8,position = 'dodge') + 
  theme(axis.text.x = element_text(angle = 90))
p7



# Duplication
library(janitor)
rdd_dups <- Consentedrdd %>%
  distinct(phone_1, .keep_all = TRUE)
# 39 were duplicates by phone number

ehcvm_dups <- Consentedehcvm %>%
  distinct(phone_1, ID_Men, .keep_all = TRUE)
# 92 were duplicates by phone number/ID_Men



# Examination of the call 
Consentedehcvm$Log.info <- NA
Consentedrdd$Log.info <- NA

Consentedehcvm$Log.info[grepl("Connected", Consentedehcvm$phone_call_log, ignore.case=TRUE) ] <- 1
Consentedehcvm$Log.info[!grepl("Connected", Consentedehcvm$phone_call_log, ignore.case=TRUE) ] <- 0
table(Consentedehcvm$Log.info)

Consentedrdd$Log.info[grepl("Connected", Consentedrdd$phone_call_log, ignore.case=TRUE) ] <- 1
Consentedrdd$Log.info[!grepl("Connected", Consentedrdd$phone_call_log, ignore.case=TRUE) ] <- 0
table(Consentedrdd$Log.info)

Consentedehcvm %>%
  group_by(dur) %>%
  count()

Consentedehcvm %>%
  group_by(dur, Log.info) %>%
  count()

Consentedrdd%>%
  group_by(dur, Log.info) %>%
  count()

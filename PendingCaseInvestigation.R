pending <- dat.wide %>% filter(Outcome.FINAL == 'PEND') %>%
  select(Outcome.FINAL, phone_1, caseid, time, enum, Source.1, call_num)
pendivr <- pending %>% filter(Source.1!='Feroxus') 
cases <- read.csv('C:/Users/KellyMcCain/Downloads/cases (74).csv') %>%
  select(c(phone_1, id, num_calls, id, label, users))

nums <- left_join(pending,cases, by = 'phone_1') 

missing <- nums %>%
  filter(is.na(id)) 
missnums <- missing$phone_1


# Get missing numbers from long dataset
missingnums <- data %>%
  filter(phone_1 %in% missnums) %>%
  select(c(enumerator, call_num, phone_1, Outcome2, call_status, Date.Interview, caseid, Source.number)) %>%
  group_by(phone_1) %>%
  summarise(call_num = max(call_num),
            enumerator = enumerator) %>% distinct()
write.csv(missingnums, 'C:/Users/KellyMcCain/Downloads/MissingCases.csv')
# On 2.18.22, I added all of the numbers in missing (using the most recent call_num and enumerator from missingnums) to the cases dataset
#so that they can be finished and get a final outcome.

check2 <- dat.wide %>% filter(phone_1 == '808300190') %>% select(Outcome.FINAL)
check2 <- dat.wide %>% filter(phone_1 == '808503054')
check2 <- data %>% filter(phone_1 == '808503054') %>% select(c(Outcome2, call_num, caseid, phone_1, enumerator))
check1 <- data_raw1 %>% filter(phone_1 == '808503054') %>% select(c(call_status, priority,call_num, caseid, phone_1, username, SubmissionDate))


check1 <- check1[order(check1$phone_1, check1$priority, check1$SubmissionDate),]
check1 <- check1[!duplicated(check1[c("phone_1","call_num",'caseid')]),]
check1 <- check1[!duplicated(check1[c("phone_1","call_num")]),]
check1 <- rbind(
  check1 %>%
    filter(call_status==1 & !duplicated(phone_1) ),
  check1 %>%
    filter(call_status!=1))
#check1 <- check1[!duplicated(check1[c('phone_1')]) & check1$call_status == '1',]
check1 <- check1 %>%
  group_by(phone_1) %>%
  mutate(closed = ifelse(call_status %in% c(1, '2a','4a',6, 7, 9,11),call_num,NA)) %>%
  fill(closed) %>%
  group_by(phone_1, caseid) %>%
  mutate(remove = ifelse(call_num > closed, 1, NA)) %>%
  filter(is.na(remove))

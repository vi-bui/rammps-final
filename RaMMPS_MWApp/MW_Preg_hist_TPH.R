r.functions.dir <- "/Users/lshvb5/Documents/rammps/"

mwpregfunc <- function(Consentedmw){

Clean.T1 <- function(i){

  i <- as.character(i)
  
  #  Early batch
  Malawi.1 <- Consentedmw %>%
    select(ph14a_1, ph14b_1,
           !!paste0('ph16_1_',i),
           !!paste0('ph17_1_',i),
           !!paste0('ph19_1_',i),
           !!paste0('ph20_1_',i),
           !!paste0('ph20b_1_',i),
           !!paste0('ph22_1_',i),
           !!paste0('ph24_1_',i),
           !!paste0('ph24b_1_',i),
           !!paste0('ph24c_1_',i),
           !!paste0('ph24d_1_',i),
           Date.Interview, starttime, endtime,
           caseid, users, Resp.Age, e2_label_1, e3_a_1, e3_exact_estimate_1, region_1, E4_1, e4b_1, e4c_1, C1_1, i1_1, i2_1, 
           b1_1, b2_1, b3_1, b4_1, b5_1, b5_number_1, b6_1, b6b_1, b7_1, b8_1, b8b_1, 
           starts_with('hd'),
           interviewer_gender_1) %>%
    filter(Date.Interview <= "2022-05-25") %>%
    dplyr::rename(N.birth1 = ph14a_1,
                  N.birth2 = ph14b_1,
                  B.Alive.Dead = !!paste0('ph16_1_',i),
                  Cry.Move.Breathe = !!paste0('ph17_1_',i),
                  Gender = !!paste0('ph19_1_',i),
                  Month.Born = !!paste0('ph20_1_',i),
                  Year.Born = !!paste0('ph20b_1_',i),
                  N.Alive.Dead = !!paste0('ph22_1_',i),
                  Age.D = !!paste0('ph24_1_',i),
                  Age.D.days = !!paste0('ph24b_1_',i),
                  Age.D.months = !!paste0('ph24c_1_',i),
                  Age.D.years = !!paste0('ph24d_1_',i)) %>%
    dplyr::mutate(pregbaby = !!paste0("1-",i),
                  B.Alive.Dead = ifelse(is.na(Cry.Move.Breathe), B.Alive.Dead,
                                        ifelse(Cry.Move.Breathe== 1, 1,2)) )%>%
    filter(!is.na(B.Alive.Dead)) 
  
  # }
  
  #  Create number of births variable which is a combo of 14a and 14b and drop 14a and 14b
  Malawi.1$N.birth <- Malawi.1$N.birth1 
  Malawi.1$N.birth[which(!is.na(Malawi.1$N.birth2))] <- Malawi.1$N.birth2[which(!is.na(Malawi.1$N.birth2))]
  
  Malawi.1$N.birth[which(Malawi.1$N.birth==1)] <- "Single"
  Malawi.1$N.birth[which(Malawi.1$N.birth==2)] <- "Multiple"
  Malawi.1$N.birth[which(Malawi.1$N.birth==3)] <- "Don't know"
  Malawi.1$N.birth1 <- NULL; Malawi.1$N.birth2 <- NULL

  
  Malawi.1 <- Malawi.1 %>%
    filter(!is.na(N.birth))
  
  #  Baby born alive or dead
  Malawi.1$Born.A.D <- Malawi.1$B.Alive.Dead 
  Malawi.1$Born.A.D[which(Malawi.1$B.Alive.Dead ==1)] <- "Alive"
  Malawi.1$Born.A.D[which(Malawi.1$B.Alive.Dead ==2)] <- "Dead"
  Malawi.1$Born.A.D[which(Malawi.1$B.Alive.Dead ==3)] <- "Miscarriage"
  Malawi.1$Born.A.D[which(Malawi.1$B.Alive.Dead ==4)] <- "Abortion"
  Malawi.1$Born.A.D[which(Malawi.1$B.Alive.Dead ==99)] <- "Refuse"
  Malawi.1$Born.A.D[which(Malawi.1$B.Alive.Dead ==98)] <- "Don't know"
  Malawi.1$B.Alive.Dead  <-NULL
  Malawi.1$Born.A.D <- ordered(Malawi.1$Born.A.D, levels = c('Alive', 'Dead','Miscarriage','Abortion','Refuse',"Don't know"))
  
  #  Month and year of death - make 6 months
  
  Malawi.1$ImputedMonthBorn <- ifelse(Malawi.1$Month.Born==98|Malawi.1$Month.Born==99, 1,0)
  Malawi.1$Month.Born[which(Malawi.1$Month.Born==98|
                              Malawi.1$Month.Born==99)] <- 6
  
  Malawi.1$Year.Born[which(Malawi.1$Year.Born==3)] <- 2003
  Malawi.1$Year.Born[which(Malawi.1$Year.Born==10)] <- 2010
  Malawi.1$Year.Born[which(Malawi.1$Year.Born==14)] <- 2014
  Malawi.1$Year.Born[which(Malawi.1$Year.Born==0)] <- 2000
  
  # Now concatenate Month and Year of birth to get Date
  Malawi.1$Day.Born<- NA
  Malawi.1$ImputedDayBorn <- ifelse(!is.na(Malawi.1$Year.Born), 1,0)
  Malawi.1$Day.Born[which(!is.na(Malawi.1$Year.Born))] <- 15
  
  Malawi.1$Date.Born <- paste(Malawi.1$Year.Born, Malawi.1$Month.Born, Malawi.1$Day.Born, sep="-")
  Malawi.1$Date.Born[which(Malawi.1$Date.Born=="NA-NA-NA")] <- NA
  Malawi.1$Date.Born = as.Date.character(Malawi.1$Date.Born,"%Y-%m-%d")
  
  Malawi.1$Day.Born <- NULL
  Malawi.1$Month.Born <- NULL
  Malawi.1$Year.Born <- NULL
  
  
  # Now are they alive or dead
  Malawi.1$Now.A.D <- NA
  Malawi.1$Now.A.D[which(Malawi.1$N.Alive.Dead==1)] <- "Alive"
  Malawi.1$Now.A.D[which(Malawi.1$N.Alive.Dead==2)] <- "Dead"
  Malawi.1$Now.A.D[which(Malawi.1$N.Alive.Dead==99)] <- "Refuse"

  Malawi.1$Age.Death <- NA
  Malawi.1$Age.Death[which(Malawi.1$Age.D==1 |
                             Malawi.1$Age.D==2 |
                             (Malawi.1$Age.D==3 &
                                Malawi.1$Age.D.years<5))] <- "U5"
  
  Malawi.1$N.Alive.Dead <- NULL
  Malawi.1$Age.D <- NULL
  Malawi.1$Age.D.days <- NULL
  Malawi.1$Age.D.months <- NULL
  Malawi.1$Age.D.years <- NULL
  
  # Filter out to only live births and count those who have a value for Now.A.D of NA - this will
  # be replaced with a value of "Alive" or "Dead" based on the 
  # Malawi.1 <- Malawi.1 %>%
  #   filter(Born.A.D=="Alive") %>%
  #   filter(Date.Born>="2014-01-01")
  
  return(Malawi.1)
  
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Clean.T2 <- function(j, i){
  
  i <- as.character(i)
  j <- as.character(j)
  
Consentedmw1 <- Consentedmw %>%
    rowwise() %>%
    mutate(ph24b_1_1_1 = ifelse('ph24b_1_1_1' %in% names(.), ph24b_1_1_1, NA),
           ph24c_1_1_1 = ifelse('ph24c_1_1_1' %in% names(.), ph24c_1_1_1, NA),
           ph14a_1_2 = ifelse('ph14a_1_2' %in% names(.), ph14a_1_2, NA),
           ph17_1_2_2 = ifelse('ph17_1_2_2' %in% names(.), ph17_1_2_2, NA),
           ph17_1_2_3 = ifelse('ph17_1_2_3' %in% names(.), ph17_1_2_3, NA),
           ph17_1_3_2 = ifelse('ph17_1_3_2' %in% names(.), ph17_1_3_2, NA),
           ph17_1_4_1 = ifelse('ph17_1_4_1' %in% names(.), ph17_1_4_1, NA),
           ph17_1_6_1 = ifelse('ph17_1_6_1' %in% names(.), ph17_1_6_1, NA),
           ph19_1_2_3 = ifelse('ph19_1_2_3' %in% names(.), ph19_1_2_3, NA),
           ph20_1_2_3 = ifelse('ph20_1_2_3' %in% names(.), ph20_1_2_3, NA),
           ph20b_1_2_3 = ifelse('ph20b_1_2_3' %in% names(.), ph20b_1_2_3, NA),
           ph22_1_2_3 = ifelse('ph22_1_2_3' %in% names(.), ph22_1_2_3, NA),
           ph24_1_1_2 = ifelse('ph24_1_1_2' %in% names(.), ph24_1_1_2, NA),
           ph24_1_2_3 = ifelse('ph24_1_2_3' %in% names(.), ph24_1_2_3, NA),
           ph24_1_6_1 = ifelse('ph24_1_6_1' %in% names(.), ph24_1_6_1, NA),
           ph24b_1_1_2 = ifelse('ph24b_1_1_2' %in% names(.), ph24b_1_1_2, NA),
           ph24c_1_1_2 = ifelse('ph24c_1_1_2' %in% names(.), ph24c_1_1_2, NA),
           ph24d_1_1_2 = ifelse('ph24d_1_1_2' %in% names(.), ph24d_1_1_2, NA),
           ph24c_1_2_1 = ifelse('ph24c_1_2_1' %in% names(.), ph24c_1_2_1, NA),
           ph24d_1_2_1 = ifelse('ph24d_1_2_1' %in% names(.), ph24d_1_2_1, NA),
           ph24b_1_2_2 = ifelse('ph24b_1_2_2' %in% names(.), ph24b_1_2_2, NA),
           ph24b_1_2_3 = ifelse('ph24b_1_2_3' %in% names(.), ph24b_1_2_3, NA),
           ph24c_1_2_2 = ifelse('ph24c_1_2_2' %in% names(.), ph24c_1_2_2, NA),
           ph24c_1_2_3 = ifelse('ph24c_1_2_3' %in% names(.), ph24c_1_2_3, NA),
           ph24d_1_2_3 = ifelse('ph24d_1_2_3' %in% names(.), ph24d_1_2_3, NA),
           ph24b_1_3_1 = ifelse('ph24b_1_3_1' %in% names(.), ph24b_1_3_1, NA),
           ph24c_1_3_1 = ifelse('ph24c_1_3_1' %in% names(.), ph24c_1_3_1, NA),
           ph24_1_3_2 = ifelse('ph24_1_3_2' %in% names(.), ph24_1_3_2, NA),
           ph24b_1_3_2 = ifelse('ph24b_1_3_2' %in% names(.), ph24b_1_3_2, NA),
           ph24c_1_3_2 = ifelse('ph24c_1_3_2' %in% names(.), ph24c_1_3_2, NA),
           ph24d_1_3_2 = ifelse('ph24d_1_3_2' %in% names(.), ph24d_1_3_2, NA),
           ph24_1_4_1 = ifelse('ph24_1_4_1' %in% names(.), ph24_1_4_1, NA),
           ph24b_1_4_1 = ifelse('ph24b_1_4_1' %in% names(.), ph24b_1_4_1, NA),
           ph24c_1_4_1 = ifelse('ph24c_1_4_1' %in% names(.), ph24c_1_4_1, NA),
           ph24d_1_4_1 = ifelse('ph24d_1_4_1' %in% names(.), ph24d_1_4_1, NA),
           ph24_1_5_1 = ifelse('ph24_1_5_1' %in% names(.), ph24_1_5_1, NA),
           ph24b_1_5_1 = ifelse('ph24b_1_5_1' %in% names(.), ph24b_1_5_1, NA),
           ph24c_1_5_1 = ifelse('ph24c_1_5_1' %in% names(.), ph24c_1_5_1, NA),
           ph24d_1_5_1 = ifelse('ph24d_1_5_1' %in% names(.), ph24d_1_5_1, NA),
           ph24b_1_6_1 = ifelse('ph24b_1_6_1' %in% names(.), ph24b_1_6_1, NA),
           ph24c_1_6_1 = ifelse('ph24c_1_6_1' %in% names(.), ph24c_1_6_1, NA),
           ph24d_1_6_1 = ifelse('ph24d_1_6_1' %in% names(.), ph24d_1_6_1, NA))

  Malawi.1 <- Consentedmw1 %>%
    select(ph14a_1_1, 
           !!paste0('ph14b_1_',j),
           !!paste0('ph16_1_',j,'_',i), 
           !!paste0('ph17_1_',j,'_',i),
           !!paste0('ph19_1_',j,'_',i), 
           !!paste0('ph20_1_',j,'_',i), 
           !!paste0('ph20b_1_',j,'_',i), 
           !!paste0('ph22_1_',j,'_',i), 
           !!paste0('ph24_1_',j,'_',i), 
           !!paste0('ph24b_1_',j,'_',i), 
           !!paste0('ph24c_1_',j,'_',i), 
           !!paste0('ph24d_1_',j,'_',i), 
           Date.Interview, starttime, endtime,
           caseid, users, Resp.Age, e2_label_1, e3_a_1, e3_exact_estimate_1, region_1, E4_1, e4b_1, e4c_1, C1_1, i1_1, i2_1, 
           b1_1, b2_1, b3_1, b4_1, b5_1, b5_number_1, b6_1, b6b_1, b7_1, b8_1, b8b_1, 
           starts_with('hd'),
           interviewer_gender_1) %>%
    filter(Date.Interview > "2022-05-25") %>%
    dplyr::rename(N.birth1 = ph14a_1_1,
                  N.birth2 = !!paste0('ph14b_1_',j),
                  B.Alive.Dead = !!paste0('ph16_1_',j,'_',i),
                  Cry.Move.Breathe = !!paste0('ph17_1_',j,"_",i),
                  Gender = !!paste0('ph19_1_',j,'_',i),
                  Month.Born = !!paste0('ph20_1_',j,'_',i),
                  Year.Born = !!paste0('ph20b_1_',j,'_',i),
                  N.Alive.Dead = !!paste0('ph22_1_',j,'_',i),
                  Age.D = !!paste0('ph24_1_',j,'_',i),
                  Age.D.days = !!paste0('ph24b_1_',j,'_',i),
                  Age.D.months = !!paste0('ph24c_1_',j,'_',i),
                  Age.D.years = !!paste0('ph24d_1_',j,'_',i)) %>%
    dplyr::mutate(pregbaby = !!paste0(j,"-",i),
                  B.Alive.Dead = ifelse(is.na(as.numeric(Cry.Move.Breathe)), as.numeric(B.Alive.Dead),
                                        ifelse(as.numeric(Cry.Move.Breathe)== 1, 1,2))) %>%
    filter(!is.na(B.Alive.Dead)) 

  #  Create number of births variable which is a combo of 14a and 14b and drop 14a and 14b
  Malawi.1$N.birth <- Malawi.1$N.birth2 
  Malawi.1$N.birth[which(!is.na(Malawi.1$N.birth1) &
                           is.na(Malawi.1$N.birth2))] <- Malawi.1$N.birth1[which(!is.na(Malawi.1$N.birth1) &
                                                                                   is.na(Malawi.1$N.birth2))]
  
  Malawi.1$N.birth[which(Malawi.1$N.birth==1)] <- "Single"
  Malawi.1$N.birth[which(Malawi.1$N.birth==2)] <- "Multiple"
  Malawi.1$N.birth[which(Malawi.1$N.birth==3)] <- "DK"
  Malawi.1$N.birth1 <- NULL; Malawi.1$N.birth2 <- NULL
  
  Malawi.1 <- Malawi.1 %>%
    filter(!is.na(N.birth))
  
  #  Baby born alive or dead
  Malawi.1$Born.A.D <- Malawi.1$B.Alive.Dead 
  Malawi.1$Born.A.D[which(Malawi.1$B.Alive.Dead ==1)] <- "Alive"
  Malawi.1$Born.A.D[which(Malawi.1$B.Alive.Dead ==2)] <- "Dead"
  Malawi.1$Born.A.D[which(Malawi.1$B.Alive.Dead ==3)] <- "Miscarriage"
  Malawi.1$Born.A.D[which(Malawi.1$B.Alive.Dead ==4)] <- "Abortion"
  Malawi.1$Born.A.D[which(Malawi.1$B.Alive.Dead ==99)] <- "Refuse"
  Malawi.1$Born.A.D[which(Malawi.1$B.Alive.Dead ==98)] <- "Don't know"
  Malawi.1$B.Alive.Dead  <-NULL
  Malawi.1$Born.A.D <- ordered(Malawi.1$Born.A.D, levels = c('Alive', 'Dead','Miscarriage','Abortion','Refuse',"Don't know"))
  
  #  Month and year of death - make 6 months
  Malawi.1$ImputedMonthBorn <- ifelse(Malawi.1$Month.Born==98|Malawi.1$Month.Born==99, 1,0)
  Malawi.1$Month.Born[which(Malawi.1$Month.Born==98|
                              Malawi.1$Month.Born==99)] <- 6
  
  # Now concatenate Month and Year of birth to get Date
  Malawi.1$Day.Born<- NA
  Malawi.1$ImputedDayBorn <- ifelse(!is.na(Malawi.1$Year.Born), 1,0)
  Malawi.1$Day.Born[which(!is.na(Malawi.1$Year.Born))] <- 15
  
  Malawi.1$Date.Born <- paste(Malawi.1$Year.Born, Malawi.1$Month.Born, Malawi.1$Day.Born, sep="-")
  Malawi.1$Date.Born[which(Malawi.1$Date.Born=="NA-NA-NA")] <- NA
  Malawi.1$Date.Born = as.Date.character(Malawi.1$Date.Born,"%Y-%m-%d")
  
  Malawi.1$Day.Born <- NULL
  Malawi.1$Month.Born <- NULL
  Malawi.1$Year.Born <- NULL
  
  # Now are they alive or dead
  Malawi.1$Now.A.D <- NA
  Malawi.1$Now.A.D[which(Malawi.1$N.Alive.Dead==1)] <- "Alive"
  Malawi.1$Now.A.D[which(Malawi.1$N.Alive.Dead==2)] <- "Dead"
  Malawi.1$Now.A.D[which(Malawi.1$N.Alive.Dead==99)] <- "Refuse"
  Malawi.1$Now.A.D <- ordered(Malawi.1$Now.A.D, levels = c('Alive','Dead',"Refuse"))

  Malawi.1$Age.Death <- NA
  Malawi.1$Age.Death[which(Malawi.1$Age.D==1 |
                             Malawi.1$Age.D==2 |
                             (Malawi.1$Age.D==3 &
                                Malawi.1$Age.D.years<5))] <- "U5"
  
  Malawi.1$N.Alive.Dead <- NULL
  Malawi.1$Age.D <- NULL
  Malawi.1$Age.D.days <- NULL
  Malawi.1$Age.D.months <- NULL
  Malawi.1$Age.D.years <- NULL
  
  # Filter out to only live births and count those who have a value for Now.A.D of NA - this will
  # be replaced with a value of "Alive" or "Dead" based on the 
  # Malawi.1 <- Malawi.1 %>%
    # filter(Born.A.D=="Alive") %>%
    # filter(Date.Born>="2014-01-01")
  
  # Remove extra unneeded variables
  
  
  return(Malawi.1)
  
}


# Pregnancy number - j
# Birth order - i
Term2 <- rbind(Clean.T2(j=1, i=1),
      Clean.T2(j=1, i=2),
      Clean.T2(j=1, i=3),
      Clean.T2(j=2, i=1),
      Clean.T2(j=2, i=2),
      Clean.T2(j=2, i=3),
      Clean.T2(j=3, i=1),
      Clean.T2(j=3, i=2),
      Clean.T2(j=3, i=3),
      Clean.T2(j=4, i=1),
      Clean.T2(j=4, i=2),
      Clean.T2(j=4, i=3),
      Clean.T2(j=5, i=1),
      Clean.T2(j=5, i=2),
      Clean.T2(j=5, i=3),
      Clean.T2(j=6, i=1),
      Clean.T2(j=6, i=2),
      Clean.T2(j=6, i=3))


Term1 <- rbind(Clean.T1(1), Clean.T1(2))
# 
Live.births <- rbind(Term1, Term2)

Live.birthsdta <- Live.births
names(Live.birthsdta) <- gsub(x = names(Live.birthsdta), pattern = "\\.", replacement = "_")  
# write_dta(Live.birthsdta, paste0("C:\\Users\\KellyMcCain/OneDrive - London School of Hygiene and Tropical Medicine/Documents/General/Partners/IPOR/SurveyData/Birth History/TPH_MW_",Sys.Date(),'.dta'))
write.csv(Live.birthsdta, paste0("/Users/lshvb5/OneDrive - London School of Hygiene and Tropical Medicine/General/Partners/IPOR/SurveyData/Birth History/TPH_MW_",Sys.Date(),'.csv'))
write.csv(Live.births, paste0(r.functions.dir,"RaMMPS_MWApp/TPH_MW.csv"))
# Live.births <- Term2

# LB <- Live.births %>%
#   group_by(Age.Death) %>%
#   dplyr::count(n()) %>%
#   summarise(n / `n()` * 1000) #%>%
  # filter(Age.Death=="U5")
return(Live.births)
}
# LB

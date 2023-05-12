library(haven)
mwpregfunc_fph <- function(Consentedmw){
r.functions.dir <- "/Users/lshvb5/Documents/rammps/"
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Clean.T2 <- function(j, i){
  
  i <- as.character(i)
  j <- as.character(j)
  
Consentedmw1 <- Consentedmw %>%
    rowwise() %>%
    mutate(fph215b_1_5 = ifelse('fph215b_1_5' %in% names(.), fph215b_1_5, NA),
           fph215b_1_6 = ifelse('fph215b_1_6' %in% names(.), fph215b_1_6, NA),
           fph215b_1_7 = ifelse('fph215b_1_7' %in% names(.), fph215b_1_7, NA),
           fph215b_number_1_5 = ifelse('fph215b_number_1_5' %in% names(.), fph215b_number_1_5, NA),
           fph215b_number_1_6 = ifelse('fph215b_number_1_6' %in% names(.), fph215b_number_1_6, NA),
           fph215b_number_1_7 = ifelse('fph215b_number_1_7' %in% names(.), fph215b_number_1_7, NA),
           fph217_1_3_2 = ifelse('fph217_1_3_2' %in% names(.), fph217_1_3_2, NA),
           fph217_1_4_2 = ifelse('fph217_1_4_2' %in% names(.), fph217_1_4_2, NA),
           fph217_1_5_1 = ifelse('fph217_1_5_1' %in% names(.), fph217_1_5_1, NA),
           fph217_1_5_2 = ifelse('fph217_1_5_2' %in% names(.), fph217_1_5_2, NA),
           fph217_1_6_1 = ifelse('fph217_1_6_1' %in% names(.), fph217_1_6_1, NA),
           fph217_1_8_1 = ifelse('fph217_1_8_1' %in% names(.), fph217_1_8_1, NA),
           fph217_1_8_2 = ifelse('fph217_1_8_2' %in% names(.), fph217_1_8_2, NA),
           fph228_1_1_2 = ifelse('fph228_1_1_2' %in% names(.), fph228_1_1_2, NA),
           fph228_1_2_2 = ifelse('fph228_1_2_2' %in% names(.), fph228_1_2_2, NA),
           fph228_1_4_2 = ifelse('fph228_1_4_2' %in% names(.), fph228_1_4_2, NA),
           fph228_1_5_1 = ifelse('fph228_1_5_1' %in% names(.), fph228_1_5_1, NA),
           fph228_1_5_2 = ifelse('fph228_1_5_2' %in% names(.), fph228_1_5_2, NA),
           fph228_1_6_1 = ifelse('fph228_1_6_1' %in% names(.), fph228_1_6_1, NA),
           fph228_1_6_2 = ifelse('fph228_1_6_2' %in% names(.), fph228_1_6_2, NA),
           fph228_1_7_1 = ifelse('fph228_1_7_1' %in% names(.), fph228_1_7_1, NA),
           fph228_1_7_2 = ifelse('fph228_1_7_2' %in% names(.), fph228_1_7_2, NA),
           fph228_1_8_1 = ifelse('fph228_1_8_1' %in% names(.), fph228_1_8_1, NA),
           fph228_1_8_2 = ifelse('fph228_1_8_2' %in% names(.), fph228_1_8_2, NA),
           fph228_1_1_1_2 = ifelse('fph228_1_1_1_2' %in% names(.), fph228_1_1_1_2, NA),
           fph228_1_1_2_2 = ifelse('fph228_1_1_2_2' %in% names(.), fph228_1_1_2_2, NA),
           fph228_1_1_4_2 = ifelse('fph228_1_1_4_2' %in% names(.), fph228_1_1_4_2, NA),
           fph228_1_1_5_1 = ifelse('fph228_1_1_5_1' %in% names(.), fph228_1_1_5_1, NA),
           fph228_1_1_5_2 = ifelse('fph228_1_1_5_2' %in% names(.), fph228_1_1_5_2, NA),
           fph228_1_1_6_1 = ifelse('fph228_1_1_6_1' %in% names(.), fph228_1_1_6_1, NA),
           fph228_1_1_6_2 = ifelse('fph228_1_1_6_2' %in% names(.), fph228_1_1_6_2, NA),
           fph228_1_1_7_1 = ifelse('fph228_1_1_7_1' %in% names(.), fph228_1_1_7_1, NA),
           fph228_1_1_7_2 = ifelse('fph228_1_1_7_2' %in% names(.), fph228_1_1_7_2, NA),
           fph228_1_1_8_1 = ifelse('fph228_1_1_8_1' %in% names(.), fph228_1_1_8_1, NA),
           fph228_1_1_8_2 = ifelse('fph228_1_1_8_2' %in% names(.), fph228_1_1_8_2, NA),
           fph228_2_1_1_2 = ifelse('fph228_2_1_1_2' %in% names(.), fph228_2_1_1_2, NA),
           fph228_2_1_2_2 = ifelse('fph228_2_1_2_2' %in% names(.), fph228_2_1_2_2, NA),
           fph228_2_1_3_1 = ifelse('fph228_2_1_3_1' %in% names(.), fph228_2_1_3_1, NA),
           fph228_2_1_3_2 = ifelse('fph228_2_1_3_2' %in% names(.), fph228_2_1_3_2, NA),
           fph228_2_1_4_1 = ifelse('fph228_2_1_4_1' %in% names(.), fph228_2_1_4_1, NA),
           fph228_2_1_4_2 = ifelse('fph228_2_1_4_2' %in% names(.), fph228_2_1_4_2, NA),
           fph228_2_1_5_1 = ifelse('fph228_2_1_5_1' %in% names(.), fph228_2_1_5_1, NA),
           fph228_2_1_5_2 = ifelse('fph228_2_1_5_2' %in% names(.), fph228_2_1_5_2, NA),
           fph228_2_1_6_1 = ifelse('fph228_2_1_6_1' %in% names(.), fph228_2_1_6_1, NA),
           fph228_2_1_6_2 = ifelse('fph228_2_1_6_2' %in% names(.), fph228_2_1_6_2, NA),
           fph228_2_1_7_1 = ifelse('fph228_2_1_7_1' %in% names(.), fph228_2_1_7_1, NA),
           fph228_2_1_7_2 = ifelse('fph228_2_1_7_2' %in% names(.), fph228_2_1_7_2, NA),
           fph228_2_1_8_1 = ifelse('fph228_2_1_8_1' %in% names(.), fph228_2_1_8_1, NA),
           fph228_2_1_8_2 = ifelse('fph228_2_1_8_2' %in% names(.), fph228_2_1_8_2, NA),
           fph228_3_1_1_2 = ifelse('fph228_3_1_1_2' %in% names(.), fph228_3_1_1_2, NA),
           fph228_3_1_2_2 = ifelse('fph228_3_1_2_2' %in% names(.), fph228_3_1_2_2, NA),
           fph228_3_1_3_2 = ifelse('fph228_3_1_3_2' %in% names(.), fph228_3_1_3_2, NA),
           fph228_3_1_4_1 = ifelse('fph228_3_1_4_1' %in% names(.), fph228_3_1_4_1, NA),
           fph228_3_1_4_2 = ifelse('fph228_3_1_4_2' %in% names(.), fph228_3_1_4_2, NA),
           fph228_3_1_5_1 = ifelse('fph228_3_1_5_1' %in% names(.), fph228_3_1_5_1, NA),
           fph228_3_1_5_2 = ifelse('fph228_3_1_5_2' %in% names(.), fph228_3_1_5_2, NA),
           fph228_3_1_6_1 = ifelse('fph228_3_1_6_1' %in% names(.), fph228_3_1_6_1, NA),
           fph228_3_1_6_2 = ifelse('fph228_3_1_6_2' %in% names(.), fph228_3_1_6_2, NA),
           fph228_3_1_7_1 = ifelse('fph228_3_1_7_1' %in% names(.), fph228_3_1_7_1, NA),
           fph228_3_1_7_2 = ifelse('fph228_3_1_7_2' %in% names(.), fph228_3_1_7_2, NA),
           fph228_3_1_8_1 = ifelse('fph228_3_1_8_1' %in% names(.), fph228_3_1_8_1, NA),
           fph228_3_1_8_2 = ifelse('fph228_3_1_8_2' %in% names(.), fph228_3_1_8_2, NA))

    # Consentedmw$fph215b_1_7 <- 2
  # Consentedmw$fph215b_number_1_7 <- 1

  Malawi.1 <- Consentedmw1 %>%
    select(fph201_1, fph202_1, fph203a_1, fph203b_1, fph203t_1,
           fph204_1, fph205a_1, fph205b_1, fph205t_1,
           fph206_1, fph207a_1, fph207b_1, fph207t_1,
           fph210_1, fph_po_1,
           starts_with('fph215_1_'), starts_with('fph215b'),
           starts_with('fph216_1_'),
           starts_with('fph217_1_'), 
           starts_with('fph219_1_'), starts_with('fph221_1_'),
           starts_with('fph220_1_'), starts_with('fph224_1_'),
           starts_with('fph228'),
           Date.Interview, starttime, endtime,
           caseid, users, Resp.Age, e2_label_1, e3_a_1, e3_exact_estimate_1, region_1, E4_1, e4b_1, e4c_1, C1_1, i1_1, i2_1, 
           b1_1, b2_1, b3_1, b4_1, b5_1, b5_number_1, b6_1, b6b_1, b7_1, b8_1, b8b_1, 
           starts_with('hd'), 
           # hd1_1, hd2_1, hd2_number_1, hd2b_1, hd2b_number_1, hd2c_1, hd2c_number_1, hd3_1, hd3_number_1, hd3b_1, hd3b_number_1,
           # hd3c_1, hd4_1, hd4_number_1, 
           interviewer_gender_1) %>%
    dplyr::rename(Total.births = fph_po_1, # need to make sure that they haven't added another loop because in this case fph_po_1 would be < total births
                  Sing.Mult = !!paste0('fph215_1_',j), # single or multiple pregnancy?
                  N.birth = !!paste0('fph215b_number_1_',j), # if multiple, then how many babies?
                  B.Alive.Dead = !!paste0('fph216_1_',j, "_",i), # born alive or dead
                  Cry.Move.Breathe = !!paste0('fph217_1_',j,"_",i), # cry move or breathed if stated born dead in fph216
                  Gender = !!paste0('fph219_1_',j, '_',i), #gender of baby at birth
                  Month.Born = !!paste0('fph221_1_',j,"_",i), # month born
                  Year.Born = !!paste0('fph220_1_',j,'_',i), # year born
                  N.Alive.Dead = !!paste0('fph224_1_',j,'_',i), #if born alive, now alive or dead? 
                  Age.D = !!paste0('fph228_1_',j,'_',i), # unit for age of death (1=days, 2=months, 3=years)
                  Age.D.days = !!paste0('fph228_1_1_',j,'_',i), # age in days at death
                  Age.D.months = !!paste0('fph228_2_1_',j,'_',i), #age in months at death
                  Age.D.years = !!paste0('fph228_3_1_',j,'_',i)) %>% #age in years at death
    dplyr::mutate(pregbaby = !!paste0(j,"-",i),
                  Age.D.years = as.numeric(Age.D.years),
                  B.Alive.Dead = ifelse(is.na(as.numeric(Cry.Move.Breathe)), as.numeric(B.Alive.Dead),
                                        ifelse(as.numeric(Cry.Move.Breathe)== 1, 1, 2))) %>%
    # select(c(Total.births, Sing.Mult, N.birth, B.Alive.Dead, Month.Born, Year.Born, N.Alive.Dead, Age.D, Age.D.days, Age.D.months, 
    #          Age.D.years, pregbaby, Date.Interview, starttime, endtime,
    #          caseid, users, Resp.Age, e2_label_1, e3_a_1, e3_exact_estimate_1, region_1, E4_1, e4b_1, e4c_1, C1_1, i1_1, i2_1, 
    #          b1_1, b2_1, b3_1, b4_1, b5_1, b5_number_1, b6_1, b6b_1, b7_1, b8_1, b8b_1, 
    #          starts_with('hd'),
    #          # hd1_1, hd2_1, hd2_number_1, hd2b_1, hd2b_number_1, hd2c_1, hd2c_number_1, hd3_1, hd3_number_1, hd3b_1, hd3b_number_1,
    #          # hd3c_1, hd4_1, hd4_number_1, 
    #          interviewer_gender_1
    #          )) %>%
    filter(!is.na(B.Alive.Dead))

#   
  Malawi.1$N.birth[which(Malawi.1$Sing.Mult==1)] <- "Single"
  Malawi.1$N.birth[which(Malawi.1$Sing.Mult==2)] <- "Multiple"
  Malawi.1$N.birth[which(Malawi.1$Sing.Mult==3)] <- "Don't know"

  Malawi.1 <- Malawi.1 %>%
    filter(N.birth %in% c('Single','Multiple',"Don't know"))

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
  Malawi.1$Now.A.D[which(Malawi.1$N.Alive.Dead==3)] <- "Don't know"
  Malawi.1$Now.A.D <- ordered(Malawi.1$Now.A.D, levels = c('Alive','Dead',"Don't know"))

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
  
  Malawi.1 <- Malawi.1 %>%
    select(-c(starts_with('fph215_1_'), starts_with('fph215b'),
             starts_with('fph216_1_'),
             starts_with('fph217_1_'), 
             starts_with('fph219_1_'), starts_with('fph221_1_'),
             starts_with('fph220_1_'), starts_with('fph224_1_'),
             starts_with('fph228'),))
  # Filter out to only live births and count those who have a value for Now.A.D of NA - this will
  # be replaced with a value of "Alive" or "Dead" based on the
  # Malawi.1 <- Malawi.1 %>%
    # filter(Born.A.D=="Alive") %>%
    # filter(Date.Born>="2014-01-01")
#   
  return(Malawi.1)
#   
}
# 
# 
# # Pregnancy number - j
# # Birth order - i

## These will need to be updated as more birth order combos are recorded
Live.births <- rbind.fill(Clean.T2(j=1, i=1),
      Clean.T2(j=1, i=2),
      Clean.T2(j=2, i=1),
      Clean.T2(j=2, i=2),
      Clean.T2(j=3, i=1),
      Clean.T2(j=3, i=2),
      Clean.T2(j=4, i=1),
      Clean.T2(j=4, i=2),
      Clean.T2(j=5, i=1),
      Clean.T2(j=5, i=2),
      Clean.T2(j=6, i=1),
      Clean.T2(j=6, i=2),
      Clean.T2(j=7, i=1),
      Clean.T2(j=7, i=2),
      Clean.T2(j=8, i=1),
      Clean.T2(j=8, i=2)) 

Live.birthsdta <- Live.births
names(Live.birthsdta) <- gsub(x = names(Live.birthsdta), pattern = "\\.", replacement = "_")  
# write_dta(Live.birthsdta, paste0("C:\\Users\\KellyMcCain/OneDrive - London School of Hygiene and Tropical Medicine/Documents/General/Partners/IPOR/SurveyData/Birth History/FPH_MW_",Sys.Date(),'.dta'))
write.csv(Live.birthsdta, paste0("/Users/lshvb5/OneDrive - London School of Hygiene and Tropical Medicine/General/Partners/IPOR/SurveyData/Birth History/FPH_MW_",Sys.Date(),'.csv'))
write.csv(Live.births, paste0(r.functions.dir,"RaMMPS_MWApp/FPH_MW.csv"))
return(Live.births)
# df11 <- Clean.T2(j = 1, i = 1)
# df12 <- Clean.T2(j = 1, i = 2)
# df21 <- Clean.T2(j = 2, i = 1)
# df22 <- Clean.T2(j = 2, i = 2)
# df31 <- Clean.T2(j = 3, i = 1)
# df32 <- Clean.T2(j = 3, i = 2)
# df41 <- Clean.T2(j = 4, i = 1)
# df42 <- Clean.T2(j = 4, i = 2)
# df51 <- Clean.T2(j = 5, i = 1)
# df61 <- Clean.T2(j = 6, i = 1)
# df71 <- Clean.T2(j = 7, i = 1)
# df81 <- Clean.T2(j = 8, i = 1)
# df82 <- Clean.T2(j = 8, i = 2)
# return(list(
#   df11, df12
# df21, df22, df31, df32, df41, df42, df51, df61,
#   df71
#   # ,
#   df81, df82
#   ))

}
# LB

#################################################
#                                               #
#                                               #
#     BEEN REPLACED BY RESPONSERATEFUNC.R       #
#                                               #
#                                               #
#################################################

# rr table function 
# library(tidyverse)
# library(readxl)
# library(tidyselect)


function(datalong, datawide, Consented){ 
  # --- Create a dataframe of Outcomes by the source, with a counter
  plot.data <- data.frame(Outcome = datalong$Outcome2,
                          Source.number = datalong$Source.number,
                          val = 1)
  # --- Collapse data for totals and relative freq of outcomes BY the source from which it was sampled 
  plot.data.S <- plot.data %>%
    dplyr::group_by(Outcome,Source.number) %>%
    dplyr::summarise(total = sum(val)) %>%
    dplyr::group_by(Source.number) %>%
    dplyr::mutate(rel.freq = (round(100 * (total/sum(total)), 2))) %>% as.data.frame()
  
  
  # --- Collapse data for totals and relative freq of outcomes in the whole data
  plot.data.T <- plot.data %>%
    dplyr::group_by(Outcome) %>%
    dplyr::summarise(total = sum(val)) %>%
    dplyr::mutate(rel.freq = (round(100 * total/sum(total), 2)),
                  Source.number ="Overall") %>% as.data.frame()
  
  
  
  # --- Stick the Source disaggragated ones and total ones together and reorder the factors
  final.plot.data <- rbind(plot.data.T, plot.data.S)
  final.plot.data$Source.number <- factor(final.plot.data$Source.number, levels = c('Overall','IVR group 1','IVR group 2','Feroxus'))
  final.plot.data <- final.plot.data[!(final.plot.data$Outcome=="missing"),]
  
  final.plot.data$Outcome <- factor(final.plot.data$Outcome,
                                    levels = c("COMP", "PART", "REFU","NNU", "INEL","LANG", "INCO","NR","NNA","REFER", "REASS", "Other")); final.plot.data <- drop_na(final.plot.data)
  
  
  
  # --- Clean by removing the unneccesary cols
  datawide1 <- dplyr::select(datawide, -c("pull_IVRprovince.1", "pull_IVRprovince.2", "pull_IVRprovince.3","pull_IVRprovince.4", "pull_IVRprovince.5","pull_label.1", "pull_label.2", "pull_label.3", "pull_label.4","pull_label.5", "enum.1", "enum.2", "enum.3", "enum.4","enum.5", "inel.age.1", "inel.age.2", "inel.age.3", "inel.age.4", "inel.age.5", 'Source.1','Source.prov.1'))
  
  
  # --- In wide form data, create a column of Source of the number by region called Source.number
  datawide$Source.number <- datawide$Source.1
 
  
  
  
  ###########################
  # === Response rate overall
  datawide$Counter <- 1
  
  # --- Create separate data frames of the diff samples overall and samples by region
  Ferox <- datawide[which(datawide$Source.1=="Feroxus"),]
  I1 <- datawide[which(datawide$Source.1=="IVR group 1"),]
  I2 <- datawide[which(datawide$Source.1=="IVR group 2"),]
  
  IVR.Kin <- datawide[which(datawide$Source.prov.1=="IVR group 1-Kin"),]
  Feroxus.Kin <- datawide[which(datawide$Source.prov.1=="Feroxus-Kin"),]
  IVR.NK <- datawide[which(datawide$Source.prov.1=="IVR group 1-NK"),]
  Ferox.NK <- datawide[which(datawide$Source.prov.1=="Feroxus-NK"),]
  
  
  # --- Collapse the data frames to obtain the totals by their outcome
  RR.tab <- (data.frame(Number = tapply(datawide$Counter, datawide$Outcome.FINAL, FUN=sum),Status = rownames(tapply(datawide$Counter, datawide$Outcome.FINAL, FUN=sum))))
  RR.tab.F <- (data.frame(Number = tapply(Ferox$Counter, Ferox$Outcome.FINAL, FUN=sum),Status = rownames(tapply(Ferox$Counter, Ferox$Outcome.FINAL, FUN=sum))))
  RR.tab.I1 <- (data.frame(Number = tapply(I1$Counter, I1$Outcome.FINAL, FUN=sum),Status = rownames(tapply(I1$Counter, I1$Outcome.FINAL, FUN=sum))))
  RR.tab.I2 <- (data.frame(Number = tapply(I2$Counter, I2$Outcome.FINAL, FUN=sum),Status = rownames(tapply(I2$Counter, I2$Outcome.FINAL, FUN=sum))))
  
  
  RR.tab.IVR.Kin <- (data.frame(Number = tapply(IVR.Kin$Counter, IVR.Kin$Outcome.FINAL, FUN=sum),Status = rownames(tapply(IVR.Kin$Counter, IVR.Kin$Outcome.FINAL, FUN=sum))))
  RR.tab.Feroxus.Kin <- (data.frame(Number = tapply(Feroxus.Kin$Counter, Feroxus.Kin$Outcome.FINAL, FUN=sum),Status = rownames(tapply(Feroxus.Kin$Counter, Feroxus.Kin$Outcome.FINAL, FUN=sum))))
  RR.tab.IVR.NK <- (data.frame(Number = tapply(IVR.NK$Counter, IVR.NK$Outcome.FINAL, FUN=sum), Status = rownames(tapply(IVR.NK$Counter, IVR.NK$Outcome.FINAL, FUN=sum))))
  RR.tab.Feroxus.NK <- (data.frame(Number = tapply(Ferox.NK$Counter, Ferox.NK$Outcome.FINAL, FUN=sum), Status = rownames(tapply(Ferox.NK$Counter, Ferox.NK$Outcome.FINAL, FUN=sum))))
  
  
  # --- Create objects of the response rates by sampling
  RR1 <- RR.tab$Number[which(RR.tab$Status=="COMP")] /sum(RR.tab$Number[which(RR.tab$Status=="COMP" | RR.tab$Status=="REFU" | RR.tab$Status=="LANG-CLOSE" | RR.tab$Status=="NR" | RR.tab$Status=="NNA")] )*100
  RR1.F <- RR.tab.F$Number[which(RR.tab.F$Status=="COMP")] /sum(RR.tab.F$Number[which(RR.tab.F$Status=="COMP" | RR.tab.F$Status=="REFU" | RR.tab.F$Status=="LANG-CLOSE" | RR.tab.F$Status=="NR"| RR.tab.F$Status=="NNA")] )*100
  RR1.I1 <- RR.tab.I1$Number[which(RR.tab.I1$Status=="COMP")] /sum(RR.tab.I1$Number[which(RR.tab.I1$Status=="COMP" | RR.tab.I1$Status=="REFU" | RR.tab.I1$Status=="LANG-CLOSE" | RR.tab.I1$Status=="NR" | RR.tab.I1$Status=="NNA")] )*100
  RR1.I2 <- RR.tab.I2$Number[which(RR.tab.I2$Status=="COMP")] /sum(RR.tab.I2$Number[which(RR.tab.I2$Status=="COMP" | RR.tab.I2$Status=="REFU" | RR.tab.I2$Status=="LANG-CLOSE" | RR.tab.I2$Status=="NR"| RR.tab.I2$Status=="NNA")] )*100
  
  
  
  RR1.IVR.Kin <- RR.tab.IVR.Kin$Number[which(RR.tab.IVR.Kin$Status=="COMP")] / sum(RR.tab.IVR.Kin$Number[which(RR.tab.IVR.Kin$Status=="COMP" | RR.tab.IVR.Kin$Status=="REFU" | RR.tab.IVR.Kin$Status=="LANG-CLOSE" | RR.tab.IVR.Kin$Status=="NR"| RR.tab.IVR.Kin$Status=="NNA")] )*100
  
  RR1.Feroxus.Kin <- RR.tab.Feroxus.Kin$Number[which(RR.tab.Feroxus.Kin$Status=="COMP")] / sum(RR.tab.Feroxus.Kin$Number[which(RR.tab.Feroxus.Kin$Status=="COMP" | RR.tab.Feroxus.Kin$Status=="REFU" | RR.tab.Feroxus.Kin$Status=="LANG-CLOSE" | RR.tab.Feroxus.Kin$Status=="NR"| RR.tab.Feroxus.Kin$Status=="NNA")] )*100
  RR1.IVR.NK <- RR.tab.IVR.NK$Number[which(RR.tab.IVR.NK$Status=="COMP")] / sum(RR.tab.IVR.NK$Number[which(RR.tab.IVR.NK$Status=="COMP" | RR.tab.IVR.NK$Status=="REFU" | RR.tab.IVR.NK$Status=="LANG-CLOSE" | RR.tab.IVR.NK$Status=="NR" | RR.tab.IVR.NK$Status=="NNA")] )*100
  RR1.Feroxus.NK <- RR.tab.Feroxus.NK$Number[which(RR.tab.Feroxus.NK$Status=="COMP")] / sum(RR.tab.Feroxus.NK$Number[which(RR.tab.Feroxus.NK$Status=="COMP" | RR.tab.Feroxus.NK$Status=="REFU" | RR.tab.Feroxus.NK$Status=="LANG-CLOSE" | RR.tab.Feroxus.NK$Status=="NR" | RR.tab.Feroxus.NK$Status=="NNA")] )*100
  
  
  # --- Collapse the data frames to obtain the totals by their outcome
  RR.tab <- (data.frame(Number = tapply(datawide$Counter, datawide$Outcome.FINAL, FUN=sum),Status = rownames(tapply(datawide$Counter, datawide$Outcome.FINAL, FUN=sum))))
  RR.tab.F <- (data.frame(Number = tapply(Ferox$Counter, Ferox$Outcome.FINAL, FUN=sum),Status = rownames(tapply(Ferox$Counter, Ferox$Outcome.FINAL, FUN=sum))))
  RR.tab.I1 <- (data.frame(Number = tapply(I1$Counter, I1$Outcome.FINAL, FUN=sum),Status = rownames(tapply(I1$Counter, I1$Outcome.FINAL, FUN=sum))))
  RR.tab.I2 <- (data.frame(Number = tapply(I2$Counter, I2$Outcome.FINAL, FUN=sum),Status = rownames(tapply(I2$Counter, I2$Outcome.FINAL, FUN=sum))))
  
  RR.tab.IVR.Kin <- (data.frame(Number = tapply(IVR.Kin$Counter, IVR.Kin$Outcome.FINAL, FUN=sum),Status = rownames(tapply(IVR.Kin$Counter, IVR.Kin$Outcome.FINAL, FUN=sum))))
  RR.tab.Feroxus.Kin <- (data.frame(Number = tapply(Feroxus.Kin$Counter, Feroxus.Kin$Outcome.FINAL, FUN=sum),Status = rownames(tapply(Feroxus.Kin$Counter, Feroxus.Kin$Outcome.FINAL, FUN=sum))))
  RR.tab.IVR.NK <- (data.frame(Number = tapply(IVR.NK$Counter, IVR.NK$Outcome.FINAL, FUN=sum), Status = rownames(tapply(IVR.NK$Counter, IVR.NK$Outcome.FINAL, FUN=sum))))
  RR.tab.Feroxus.NK <- (data.frame(Number = tapply(Ferox.NK$Counter, Ferox.NK$Outcome.FINAL, FUN=sum), Status = rownames(tapply(Ferox.NK$Counter, Ferox.NK$Outcome.FINAL, FUN=sum))))
  
  
  # --- Create objects of the response rates by sampling
  RR1 <- RR.tab$Number[which(RR.tab$Status=="COMP")] /sum(RR.tab$Number[which(RR.tab$Status=="COMP" | RR.tab$Status=="REFU" | RR.tab$Status=="LANG-CLOSE" | RR.tab$Status=="NR" | RR.tab$Status=="NNA")] )*100
  RR1.F <- RR.tab.F$Number[which(RR.tab.F$Status=="COMP")] /sum(RR.tab.F$Number[which(RR.tab.F$Status=="COMP" | RR.tab.F$Status=="REFU" | RR.tab.F$Status=="LANG-CLOSE" | RR.tab.F$Status=="NR" | RR.tab.F$Status=="NNA")] )*100
  RR1.I1 <- RR.tab.I1$Number[which(RR.tab.I1$Status=="COMP")] /sum(RR.tab.I1$Number[which(RR.tab.I1$Status=="COMP" | RR.tab.I1$Status=="REFU" | RR.tab.I1$Status=="LANG-CLOSE" | RR.tab.I1$Status=="NR" | RR.tab.I1$Status=="NNA")] )*100
  RR1.I2 <- RR.tab.I2$Number[which(RR.tab.I2$Status=="COMP")] /sum(RR.tab.I2$Number[which(RR.tab.I2$Status=="COMP" | RR.tab.I2$Status=="REFU" | RR.tab.I2$Status=="LANG-CLOSE" | RR.tab.I2$Status=="NR" | RR.tab.I2$Status=="NNA")] )*100
  
  RR1.IVR.Kin <- RR.tab.IVR.Kin$Number[which(RR.tab.IVR.Kin$Status=="COMP")] / sum(RR.tab.IVR.Kin$Number[which(RR.tab.IVR.Kin$Status=="COMP" | RR.tab.IVR.Kin$Status=="REFU" | RR.tab.IVR.Kin$Status=="LANG-CLOSE" | RR.tab.IVR.Kin$Status=="NR"| RR.tab.IVR.Kin$Status=="NNA")] )*100
  RR1.Feroxus.Kin <- RR.tab.Feroxus.Kin$Number[which(RR.tab.Feroxus.Kin$Status=="COMP")] / sum(RR.tab.Feroxus.Kin$Number[which(RR.tab.Feroxus.Kin$Status=="COMP" | RR.tab.Feroxus.Kin$Status=="REFU" | RR.tab.Feroxus.Kin$Status=="LANG-CLOSE" | RR.tab.Feroxus.Kin$Status=="NR"| RR.tab.Feroxus.Kin$Status=="NNA")] )*100
  RR1.IVR.NK <- RR.tab.IVR.NK$Number[which(RR.tab.IVR.NK$Status=="COMP")] / sum(RR.tab.IVR.NK$Number[which(RR.tab.IVR.NK$Status=="COMP" | RR.tab.IVR.NK$Status=="REFU" | RR.tab.IVR.NK$Status=="LANG-CLOSE" | RR.tab.IVR.NK$Status=="NR"| RR.tab.IVR.NK$Status=="NNA")] )*100
  RR1.Feroxus.NK <- RR.tab.Feroxus.NK$Number[which(RR.tab.Feroxus.NK$Status=="COMP")] / sum(RR.tab.Feroxus.NK$Number[which(RR.tab.Feroxus.NK$Status=="COMP" | RR.tab.Feroxus.NK$Status=="REFU" | RR.tab.Feroxus.NK$Status=="LANG-CLOSE" | RR.tab.Feroxus.NK$Status=="NR"| RR.tab.Feroxus.NK$Status=="NNA")] )*100
  
  
  # === Create a matrix of completed calls (based on the long form data)
  dat.long <- datalong
  Completedcalls <- plyr::count(dat.long$Outcome2[which(dat.long$Outcome2=="COMP")])$freq
  print('made it here :)')
  Lifted <- sum(plyr::count(dat.long$Outcome2[which(dat.long$Outcome2!="NNU" & dat.long$Outcome2!="NR")])$freq)
  print('made it to lifted')
  
  dat.long$Source.number.cat <- dat.long$Source.prov
  
  # # === Call attempts 
  # Call.attempts <- nrow(dat.long)
  IVR2 <- dat.long[which(dat.long$Source.number.cat=="IVR group 2"),]; Call.attempts.IVR2 <- nrow(IVR2)
  
  # 
  IVR1.Kin <- dat.long[which(dat.long$Source.number.cat=="IVR group 1-Kin"),]; Call.attempts.IVR1.Kin <- nrow(IVR1.Kin)
  Feroxus.Kin <- dat.long[which(dat.long$Source.number.cat=="Feroxus-Kin"),]; Call.attempts.Feroxus.Kin <- nrow(Feroxus.Kin)
  IVR1.NK <- dat.long[which(dat.long$Source.number.cat=="IVR group 1-NK"),]; Call.attempts.IVR1.NK <- nrow(IVR1.NK)
  Feroxus.NK <- dat.long[which(dat.long$Source.number.cat=="Feroxus-NK"),]; Call.attempts.Feroxus.NK <- nrow(Feroxus.NK)
  
  sum(table(dat.long$Source.number.cat))
  # === Table of call attempts and completed calls 
  matrix.cc<- matrix(NA, 2,5); matrix.cc[1,] <- table(dat.long$Source.number.cat)[c(1,2,3,4,5)]
  matrix.cc[2,] <- c(plyr::count(Feroxus.Kin$Outcome2[which(Feroxus.Kin$Outcome2=="COMP")])$freq, 
                     plyr::count(Feroxus.NK$Outcome2[which(Feroxus.NK$Outcome2=="COMP")])$freq,
                     plyr::count(IVR1.Kin$Outcome2[which(IVR1.Kin$Outcome2=="COMP")])$freq,
                     plyr::count(IVR1.NK$Outcome2[which(IVR1.NK$Outcome2=="COMP")])$freq, 
                     plyr::count(IVR2$Outcome2[which(IVR2$Outcome2=="COMP")])$freq)
  print('matrix.cc worked')
  matrix.cc <- cbind(matrix.cc, c(sum(table(dat.long$Source.number.cat)),sum(matrix.cc[2,])))
  rownames(matrix.cc) <- c("Calls placed", "Completed calls")
  colnames(matrix.cc) <- c("Feroxus-Kin", "Feroxus-NK", "IVR-Kin", "IVR-NK", "IVR2", "Total")
  
  # === Call attempts per completed call
  CPCC <- matrix.cc[1,"Total"]/matrix.cc[2,"Total"]
  CPCC.IVR2 <- matrix.cc[1,"IVR2"]/matrix.cc[2,"IVR2"]
  CPCC.IVR1 <- (matrix.cc[1,"IVR-Kin"] + matrix.cc[1,"IVR-NK"])/(matrix.cc[2,"IVR-Kin"] + matrix.cc[2,"IVR-NK"])
  CPCC.Ferox <- (matrix.cc[1,"Feroxus-Kin"] + matrix.cc[1,"Feroxus-NK"])/(matrix.cc[2,"Feroxus-Kin"] + matrix.cc[2,"Feroxus-NK"])
  
  CPCC.IVR1.Kin <- matrix.cc[1,"IVR-Kin"]/matrix.cc[2,"IVR-Kin"]
  CPCC.Ferox.Kin <- matrix.cc[1,"Feroxus-Kin"]/matrix.cc[2,"Feroxus-Kin"]
  CPCC.IVR1.NK <- matrix.cc[1,"IVR-NK"]/matrix.cc[2,"IVR-NK"]
  CPCC.Ferox.NK <- matrix.cc[1,"Feroxus-NK"]/matrix.cc[2,"Feroxus-NK"]
  
  # === % of numbers with completed CATI
  Perc.comp <- RR.tab$Number[which(RR.tab$Status=="COMP")] / sum(RR.tab$Number) *100
  Perc.comp.IVR2 <- RR.tab.I2$Number[which(RR.tab.I2$Status=="COMP")] / sum(RR.tab.I2$Number) *100
  Perc.comp.IVR1 <- RR.tab.I1$Number[which(RR.tab.I1$Status=="COMP")] / sum(RR.tab.I1$Number) *100
  Perc.comp.Ferox <- RR.tab.F$Number[which(RR.tab.F$Status=="COMP")] / sum(RR.tab.F$Number) *100
  
  Perc.comp.IVR1.Kin <- RR.tab.IVR.Kin$Number[which(RR.tab.IVR.Kin$Status=="COMP")] / sum(RR.tab.IVR.Kin$Number) *100
  Perc.comp.Ferox.Kin <- RR.tab.Feroxus.Kin$Number[which(RR.tab.Feroxus.Kin$Status=="COMP")] / sum(RR.tab.Feroxus.Kin$Number) *100
  Perc.comp.IVR1.NK <- RR.tab.IVR.NK$Number[which(RR.tab.IVR.Kin$Status=="COMP")] / sum(RR.tab.IVR.NK$Number) *100
  Perc.comp.Ferox.NK <- RR.tab.Feroxus.NK$Number[which(RR.tab.Feroxus.NK$Status=="COMP")] / sum(RR.tab.Feroxus.NK$Number) *100
  
  # --- Insert the values into a matrix for presentation - 1st col= Overall; 2nd=Kinshasa; 3rd=NK
  matrix.rates <- matrix(NA, 20, 3)
  matrix.rates[,1] <- c(RR1, RR1.I1, RR1.I2, RR1.F,
                        CPCC, CPCC.IVR1, CPCC.IVR2, CPCC.Ferox,
                        Perc.comp, Perc.comp.IVR1, Perc.comp.IVR2, Perc.comp.Ferox,
                        c(matrix.cc[1,6], sum(matrix.cc[1,3:4]),matrix.cc[1,5], sum(matrix.cc[1,1:2])),
                        c(matrix.cc[2,6], sum(matrix.cc[2,3:4]),matrix.cc[2,5], sum(matrix.cc[2,1:2]))
  )
  
  IVR.vals <- Consented %>%
    dplyr::mutate(counter =1) %>%
    dplyr::group_by(Source.number, Resp.Region) %>%
    dplyr::summarise(n=sum(counter)) %>%
    filter(Source.number=="IVR group 2" )
  vals <- Consented %>%
    dplyr::mutate(counter =1) %>%
    dplyr::group_by(Resp.Region) %>%
    dplyr::summarise(n=sum(counter)) 
  vals$n[1]
  
  
  
  matrix.rates[,2] <- c(NA, RR1.IVR.Kin, NA, RR1.Feroxus.Kin,
                        NA, CPCC.IVR1.Kin, NA, CPCC.Ferox.Kin,
                        NA, Perc.comp.IVR1.Kin, NA, Perc.comp.Ferox.Kin,
                        c(NA, matrix.cc[1,3], NA, matrix.cc[1,1]),
                        c(vals$n[1], matrix.cc[2,3], IVR.vals$n[1], matrix.cc[2,1]))
  
  matrix.rates[,3] <- c(NA, RR1.IVR.NK, NA, RR1.Feroxus.NK,
                        NA, CPCC.IVR1.NK, NA, CPCC.Ferox.NK,
                        NA, Perc.comp.IVR1.NK, NA, Perc.comp.Ferox.NK,
                        c(NA,matrix.cc[1,4] , NA, matrix.cc[1,2]),
                        c(vals$n[2],matrix.cc[2,4] , IVR.vals$n[2], matrix.cc[2,2]))
  
  
  
  matrix.rates <- round(matrix.rates,2)
  matrix.rates[13:20,] <- as.character(round(matrix.rates[13:20,],1))
  matrix.rates <- as.data.frame(matrix.rates)
  colnames(matrix.rates) <- c("Overall", "Kinshasa", "Nord Kivu")
  # --- reorder the columns and name them/convert to DF
  matrix.rates<- matrix.rates %>%
    mutate(Statistic = c("Response rate (%)", "", "", "",
                         "Attempts per complete CATI","", "", "",
                         "Numbers w/complete CATI (%)", "", "", "",
                         "Calls placed", "", "", "",
                         "CATIs completed", "", "", ""),
           Sample = rep(c("Overall", "IVR1", "IVR2", "Feroxus"),5)) %>%
    relocate(Statistic, Sample) %>%
    as.data.frame()
  
   #%>%
    # footnote("NA for IVR group 2 as province of residence was not established before making the phonecall;   
    #        Response Rate = COMP / [COMP+ PART+ REFU + NR (5 attempts) + NNA (5attempts)+ ELIG unknown)]. Cases where the eligibility of the respondent is unknown may arise, for example, when someone answered the phone but the enumerator could not establish a conversation;    
    #        Attempts per complete CATI = Total phone calls made/total completed interviews;    
    #        % numbers w/complete CATI = Total phone numbers yielding a complete interview/Total unique phone numbers")  
  
    return(matrix.rates)
  
  }
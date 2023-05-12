# Response Rates function

function(data, dat.wide, Consented){
  
  # My version
  dat.wide <- dat.wide %>%
    mutate(Counter = 1, 
           month.interview = month(time),
           month.interview = case_when(month.interview == 1~ 'January',
                                       month.interview == 2~ 'February',
                                       month.interview == 3~ 'March',
                                       month.interview == 4~'April',
                                       month.interview == 5~'May',
                                       month.interview == 6~'June',
                                       month.interview == 7~'July',
                                       month.interview == 8~'August',
                                       month.interview == 9~'September',
                                       month.interview == 10~'October',
                                       month.interview == 11~'November',
                                       month.interview == 12~'December'))
  wide.plot.data.T <- rbind(
    dat.wide %>%
    dplyr::group_by(Outcome.FINAL, Source.1, month.interview) %>%
    dplyr::summarise(total = sum(Counter)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Source.1, month.interview) %>%
    dplyr::mutate(rel.freq = (round(100 * total/sum(total), 2))) %>% as.data.frame()
  ,
  dat.wide %>%
    dplyr::group_by(Outcome.FINAL, month.interview) %>%
    dplyr::summarise(total = sum(Counter)) %>%
    dplyr::group_by(month.interview) %>%
    dplyr::mutate(rel.freq = (round(100 * total/sum(total), 2)),
                  Source.1 = 'Overall') %>% as.data.frame()
  ) %>%
    mutate(Source.1 = factor(Source.1, levels = c('Overall','IVR group 1','IVR group 2','Feroxus')),
           Outcome.FINAL = factor(Outcome.FINAL, 
                                  levels= c("COMP", "REFU","NR", "NNU", "INEL","LANG", "NNA", "PEND", "PART", 'DEFER')))
  
  
  ###########################################
  # === Response rate overall
  dat.wide$Counter <- 1
  
  # --- Create separate data frames of the diff samples overall and samples by region
  Ferox <- dat.wide[which(dat.wide$Source.1=="Feroxus"),]
  I1 <- dat.wide[which(dat.wide$Source.1=="IVR group 1"),]
  I2 <- dat.wide[which(dat.wide$Source.1=="IVR group 2"),]
  
  IVR.Kin <- dat.wide[which(dat.wide$Source.prov.1=="IVR group 1-Kin"),]
  Feroxus.Kin <- dat.wide[which(dat.wide$Source.prov.1=="Feroxus-Kin"),]
  IVR.NK <- dat.wide[which(dat.wide$Source.prov.1=="IVR group 1-NK"),]
  Ferox.NK <- dat.wide[which(dat.wide$Source.prov.1=="Feroxus-NK"),]
  
  
  # --- Collapse the data frames to obtain the totals by their outcome
  RR.tab <- (data.frame(Number = tapply(dat.wide$Counter, dat.wide$Outcome.FINAL, FUN=sum),Status = rownames(tapply(dat.wide$Counter, dat.wide$Outcome.FINAL, FUN=sum))))
  RR.tab.F <- (data.frame(Number = tapply(Ferox$Counter, Ferox$Outcome.FINAL, FUN=sum),Status = rownames(tapply(Ferox$Counter, Ferox$Outcome.FINAL, FUN=sum))))
  RR.tab.I1 <- (data.frame(Number = tapply(I1$Counter, I1$Outcome.FINAL, FUN=sum),Status = rownames(tapply(I1$Counter, I1$Outcome.FINAL, FUN=sum))))
  RR.tab.I2 <- (data.frame(Number = tapply(I2$Counter, I2$Outcome.FINAL, FUN=sum),Status = rownames(tapply(I2$Counter, I2$Outcome.FINAL, FUN=sum))))
  
  
  RR.tab.IVR.Kin <- (data.frame(Number = tapply(IVR.Kin$Counter, IVR.Kin$Outcome.FINAL, FUN=sum),Status = rownames(tapply(IVR.Kin$Counter, IVR.Kin$Outcome.FINAL, FUN=sum))))
  RR.tab.Feroxus.Kin <- (data.frame(Number = tapply(Feroxus.Kin$Counter, Feroxus.Kin$Outcome.FINAL, FUN=sum),Status = rownames(tapply(Feroxus.Kin$Counter, Feroxus.Kin$Outcome.FINAL, FUN=sum))))
  RR.tab.IVR.NK <- (data.frame(Number = tapply(IVR.NK$Counter, IVR.NK$Outcome.FINAL, FUN=sum), Status = rownames(tapply(IVR.NK$Counter, IVR.NK$Outcome.FINAL, FUN=sum))))
  RR.tab.Feroxus.NK <- (data.frame(Number = tapply(Ferox.NK$Counter, Ferox.NK$Outcome.FINAL, FUN=sum), Status = rownames(tapply(Ferox.NK$Counter, Ferox.NK$Outcome.FINAL, FUN=sum))))
  
  
  # --- Create objects of the response rates by sampling
  RR1 <- RR.tab$Number[which(RR.tab$Status=="COMP")] /sum(RR.tab$Number[which(RR.tab$Status=="COMP" | RR.tab$Status=="PART" |RR.tab$Status=="REFU" | RR.tab$Status=="LANG" | RR.tab$Status=="NR" | RR.tab$Status=="NNA")] )*100
  RR1.F <- RR.tab.F$Number[which(RR.tab.F$Status=="COMP")] /sum(RR.tab.F$Number[which(RR.tab.F$Status=="COMP" | RR.tab.F$Status=="PART" |RR.tab.F$Status=="REFU" | RR.tab.F$Status=="LANG" | RR.tab.F$Status=="NR"| RR.tab.F$Status=="NNA")] )*100
  RR1.I1 <- RR.tab.I1$Number[which(RR.tab.I1$Status=="COMP")] /sum(RR.tab.I1$Number[which(RR.tab.I1$Status=="COMP" | RR.tab.I1$Status=="PART" |RR.tab.I1$Status=="REFU" | RR.tab.I1$Status=="LANG" | RR.tab.I1$Status=="NR" | RR.tab.I1$Status=="NNA")] )*100
  RR1.I2 <- RR.tab.I2$Number[which(RR.tab.I2$Status=="COMP")] /sum(RR.tab.I2$Number[which(RR.tab.I2$Status=="COMP" | RR.tab.I2$Status=="PART" |RR.tab.I2$Status=="REFU" | RR.tab.I2$Status=="LANG" | RR.tab.I2$Status=="NR"| RR.tab.I2$Status=="NNA")] )*100
  
  RR1.IVR.Kin <- RR.tab.IVR.Kin$Number[which(RR.tab.IVR.Kin$Status=="COMP")] / sum(RR.tab.IVR.Kin$Number[which(RR.tab.IVR.Kin$Status=="COMP" | RR.tab.IVR.Kin$Status=="PART" |RR.tab.IVR.Kin$Status=="REFU" | RR.tab.IVR.Kin$Status=="LANG" | RR.tab.IVR.Kin$Status=="NR"| RR.tab.IVR.Kin$Status=="NNA")] )*100
  RR1.Feroxus.Kin <- RR.tab.Feroxus.Kin$Number[which(RR.tab.Feroxus.Kin$Status=="COMP")] / sum(RR.tab.Feroxus.Kin$Number[which(RR.tab.Feroxus.Kin$Status=="COMP" | RR.tab.Feroxus.Kin$Status=="PART" |RR.tab.Feroxus.Kin$Status=="REFU" | RR.tab.Feroxus.Kin$Status=="LANG" | RR.tab.Feroxus.Kin$Status=="NR"| RR.tab.Feroxus.Kin$Status=="NNA")] )*100
  RR1.IVR.NK <- RR.tab.IVR.NK$Number[which(RR.tab.IVR.NK$Status=="COMP")] / sum(RR.tab.IVR.NK$Number[which(RR.tab.IVR.NK$Status=="COMP" | RR.tab.IVR.NK$Status=="PART" |RR.tab.IVR.NK$Status=="REFU" | RR.tab.IVR.NK$Status=="LANG" | RR.tab.IVR.NK$Status=="NR" | RR.tab.IVR.NK$Status=="NNA")] )*100
  RR1.Feroxus.NK <- RR.tab.Feroxus.NK$Number[which(RR.tab.Feroxus.NK$Status=="COMP")] / sum(RR.tab.Feroxus.NK$Number[which(RR.tab.Feroxus.NK$Status=="COMP" | RR.tab.Feroxus.NK$Status=="PART" |RR.tab.Feroxus.NK$Status=="REFU" | RR.tab.Feroxus.NK$Status=="LANG" | RR.tab.Feroxus.NK$Status=="NR" | RR.tab.Feroxus.NK$Status=="NNA")] )*100
  
  
  
  # --- Create objects of the REFUSAL rates by sampling
  RefR1 <- RR.tab$Number[which(RR.tab$Status=="REFU")] /sum(RR.tab$Number[which(RR.tab$Status=="COMP" | RR.tab$Status=="PART" |RR.tab$Status=="REFU" | RR.tab$Status=="LANG" | RR.tab$Status=="NR" | RR.tab$Status=="NNA")] )*100
  RefR1.F <- RR.tab.F$Number[which(RR.tab.F$Status=="REFU")] /sum(RR.tab.F$Number[which(RR.tab.F$Status=="COMP" |RR.tab.F$Status=="PART" | RR.tab.F$Status=="REFU" | RR.tab.F$Status=="LANG" | RR.tab.F$Status=="NR"| RR.tab.F$Status=="NNA")] )*100
  RefR1.I1 <- RR.tab.I1$Number[which(RR.tab.I1$Status=="REFU")] /sum(RR.tab.I1$Number[which(RR.tab.I1$Status=="COMP" | RR.tab.I1$Status=="PART" |RR.tab.I1$Status=="REFU" | RR.tab.I1$Status=="LANG" | RR.tab.I1$Status=="NR" | RR.tab.I1$Status=="NNA")] )*100
  RefR1.I2 <- RR.tab.I2$Number[which(RR.tab.I2$Status=="REFU")] /sum(RR.tab.I2$Number[which(RR.tab.I2$Status=="COMP" | RR.tab.I2$Status=="PART" |RR.tab.I2$Status=="REFU" | RR.tab.I2$Status=="LANG" | RR.tab.I2$Status=="NR"| RR.tab.I2$Status=="NNA")] )*100
  
  
  RefR1.IVR.Kin <- RR.tab.IVR.Kin$Number[which(RR.tab.IVR.Kin$Status=="REFU")] / sum(RR.tab.IVR.Kin$Number[which(RR.tab.IVR.Kin$Status=="COMP" | RR.tab.IVR.Kin$Status=="PART" |RR.tab.IVR.Kin$Status=="REFU" | RR.tab.IVR.Kin$Status=="LANG" | RR.tab.IVR.Kin$Status=="NR"| RR.tab.IVR.Kin$Status=="NNA")] )*100
  RefR1.Feroxus.Kin <- RR.tab.Feroxus.Kin$Number[which(RR.tab.Feroxus.Kin$Status=="REFU")] / sum(RR.tab.Feroxus.Kin$Number[which(RR.tab.Feroxus.Kin$Status=="COMP" | RR.tab.Feroxus.Kin$Status=="PART" |RR.tab.Feroxus.Kin$Status=="REFU" | RR.tab.Feroxus.Kin$Status=="LANG" | RR.tab.Feroxus.Kin$Status=="NR"| RR.tab.Feroxus.Kin$Status=="NNA")] )*100
  RefR1.IVR.NK <- RR.tab.IVR.NK$Number[which(RR.tab.IVR.NK$Status=="REFU")] / sum(RR.tab.IVR.NK$Number[which(RR.tab.IVR.NK$Status=="COMP" | RR.tab.IVR.NK$Status=="PART" |RR.tab.IVR.NK$Status=="REFU" | RR.tab.IVR.NK$Status=="LANG" | RR.tab.IVR.NK$Status=="NR" | RR.tab.IVR.NK$Status=="NNA")] )*100
  RefR1.Feroxus.NK <- RR.tab.Feroxus.NK$Number[which(RR.tab.Feroxus.NK$Status=="REFU")] / sum(RR.tab.Feroxus.NK$Number[which(RR.tab.Feroxus.NK$Status=="COMP" | RR.tab.Feroxus.NK$Status=="PART" |RR.tab.Feroxus.NK$Status=="REFU" | RR.tab.Feroxus.NK$Status=="LANG" | RR.tab.Feroxus.NK$Status=="NR" | RR.tab.Feroxus.NK$Status=="NNA")] )*100
  
  
  # === Create a matrix of completed calls (based on the long form data)
  dat.long <- data
  Completedcalls <- plyr::count(dat.long$Outcome2[which(dat.long$Outcome2=="COMP")])$freq
  Lifted <- sum(plyr::count(dat.long$Outcome2[which(dat.long$Outcome2!="NNU" & dat.long$Outcome2!="NR")])$freq)
  
  dat.long$Source.number.cat <- dat.long$Source.prov
  
  # # === Call attempts 
  # Call.attempts <- nrow(dat.long)
  IVR2 <- dat.long[which(dat.long$Source.number.cat=="IVR group 2"),]; Call.attempts.IVR2 <- nrow(IVR2)
  
  # 
  IVR1.Kin <- dat.long[which(dat.long$Source.number.cat=="IVR group 1-Kin"),]; Call.attempts.IVR1.Kin <- nrow(IVR1.Kin)
  Feroxus.Kin <- dat.long[which(dat.long$Source.number.cat=="Feroxus-Kin"),]; Call.attempts.Feroxus.Kin <- nrow(Feroxus.Kin)
  IVR1.NK <- dat.long[which(dat.long$Source.number.cat=="IVR group 1-NK"),]; Call.attempts.IVR1.NK <- nrow(IVR1.NK)
  Feroxus.NK <- dat.long[which(dat.long$Source.number.cat=="Feroxus-NK"),]; Call.attempts.Feroxus.NK <- nrow(Feroxus.NK)
  
  
  
  #n === Table of call attempts and completed calls 
  matrix.cc<- matrix(NA, 2,5); matrix.cc[1,] <- table(dat.long$Source.number.cat)[c(1,2,3,4,5)]
  matrix.cc[2,] <- c(nrow(subset(Consented, Source.prov=="Feroxus-Kin")), 
                     nrow(subset(Consented, Source.prov=="Feroxus-NK")),
                     nrow(subset(Consented, Source.prov=="IVR group 1-Kin")),
                     nrow(subset(Consented, Source.prov=="IVR group 1-NK")), 
                     nrow(subset(Consented, Source.prov=="IVR group 2")))
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
  matrix.rates <- matrix(NA, 24, 3)
  matrix.rates[,1] <- c(RR1, RR1.I1, RR1.I2, RR1.F,
                        RefR1, RefR1.I1, RefR1.I2, RefR1.F,
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
                        NA, RefR1.IVR.Kin, NA, RefR1.Feroxus.Kin,
                        NA, CPCC.IVR1.Kin, NA, CPCC.Ferox.Kin,
                        NA, Perc.comp.IVR1.Kin, NA, Perc.comp.Ferox.Kin,
                        c(NA, matrix.cc[1,3], NA, matrix.cc[1,1]),
                        c(vals$n[1], matrix.cc[2,3], IVR.vals$n[1], matrix.cc[2,1]))
  
  matrix.rates[,3] <- c(NA, RR1.IVR.NK, NA, RR1.Feroxus.NK,
                        NA, RefR1.IVR.NK, NA, RefR1.Feroxus.NK,
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
    mutate(Statistic = c("Response rate", "", "", "",
                         'Refusal rate', '','','',
                         "Attempts per complete CATI","", "", "",
                         "% numbers w/complete CATI", "", "", "",
                         "Calls placed", "", "", "",
                         "CATIs completed", "", "", ""),
           Sample = rep(c("Overall", "IVR1", "IVR2", "Feroxus"),6)) %>%
    relocate(Statistic, Sample)%>%
    as.data.frame()
    return(matrix.rates)
  
}

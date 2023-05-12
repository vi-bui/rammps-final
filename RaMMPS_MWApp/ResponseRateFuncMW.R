# Response Rates function

function(data_clean3, dat.widemw, Consentedmw){
  dat.widemw <- dat.widemw %>%
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
  North <- dat.widemw[which(dat.widemw$region=="1"),]
  Cen <- dat.widemw[which(dat.widemw$region=="2"),]
  South <- dat.widemw[which(dat.widemw$region=="3"),]
  
  
  # --- Collapse the data frames to obtain the totals by their outcome
  RR.tab <- (data.frame(Number = tapply(dat.widemw$Counter, dat.widemw$Outcome.FINAL, FUN=sum),
                        Status = rownames(tapply(dat.widemw$Counter, dat.widemw$Outcome.FINAL, FUN=sum))))
  
  RR.tab.North <- (data.frame(Number = tapply(North$Counter, North$Outcome.FINAL, FUN=sum),Status = rownames(tapply(North$Counter, North$Outcome.FINAL, FUN=sum))))
  RR.tab.Central <- (data.frame(Number = tapply(Cen$Counter, Cen$Outcome.FINAL, FUN=sum),Status = rownames(tapply(Cen$Counter, Cen$Outcome.FINAL, FUN=sum))))
  RR.tab.South <- (data.frame(Number = tapply(South$Counter, South$Outcome.FINAL, FUN=sum), Status = rownames(tapply(South$Counter, South$Outcome.FINAL, FUN=sum))))
  
  
  # --- Create objects of the response rates by sampling
  RR1 <- RR.tab$Number[which(RR.tab$Status=="COMP")] /sum(RR.tab$Number[which(RR.tab$Status=="COMP" | RR.tab$Status=="PART" |RR.tab$Status=="REFU" | RR.tab$Status=="LANG" | RR.tab$Status=="NR" | RR.tab$Status=="NNA")] )*100
  
  RR1.North <- RR.tab.North$Number[which(RR.tab.North$Status=="COMP")] / sum(RR.tab.North$Number[which(RR.tab.North$Status=="COMP" | RR.tab.North$Status=="PART" |RR.tab.North$Status=="REFU" | RR.tab.North$Status=="LANG" | RR.tab.North$Status=="NR"| RR.tab.North$Status=="NNA")] )*100
  RR1.Central <- RR.tab.Central$Number[which(RR.tab.Central$Status=="COMP")] / sum(RR.tab.Central$Number[which(RR.tab.Central$Status=="COMP" | RR.tab.Central$Status=="PART" |RR.tab.Central$Status=="REFU" | RR.tab.Central$Status=="LANG" | RR.tab.Central$Status=="NR"| RR.tab.Central$Status=="NNA")] )*100
  RR1.South <- RR.tab.South$Number[which(RR.tab.South$Status=="COMP")] / sum(RR.tab.South$Number[which(RR.tab.South$Status=="COMP" | RR.tab.South$Status=="PART" |RR.tab.South$Status=="REFU" | RR.tab.South$Status=="LANG" | RR.tab.South$Status=="NR" | RR.tab.South$Status=="NNA")] )*100
  
  
  
  RR1.North <- RR.tab.North$Number[which(RR.tab.North$Status=="COMP")] / sum(RR.tab.North$Number[which(RR.tab.North$Status=="COMP" | RR.tab.North$Status=="PART" |RR.tab.North$Status=="REFU" | RR.tab.North$Status=="LANG" | RR.tab.North$Status=="NR"| RR.tab.North$Status=="NNA")] )*100
  RR1.Central <- RR.tab.Central$Number[which(RR.tab.Central$Status=="COMP")] / sum(RR.tab.Central$Number[which(RR.tab.Central$Status=="COMP" | RR.tab.Central$Status=="PART" |RR.tab.Central$Status=="REFU" | RR.tab.Central$Status=="LANG" | RR.tab.Central$Status=="NR"| RR.tab.Central$Status=="NNA")] )*100
  RR1.South <- RR.tab.South$Number[which(RR.tab.South$Status=="COMP")] / sum(RR.tab.South$Number[which(RR.tab.South$Status=="COMP" | RR.tab.South$Status=="PART" |RR.tab.South$Status=="REFU" | RR.tab.South$Status=="LANG" | RR.tab.South$Status=="NR"| RR.tab.South$Status=="NNA")] )*100
  
  
  # --- Create objects of the REFUSAL rates by sampling
  RefR1 <- RR.tab$Number[which(RR.tab$Status=="REFU")] /sum(RR.tab$Number[which(RR.tab$Status=="COMP" | RR.tab$Status=="PART" |RR.tab$Status=="REFU" | RR.tab$Status=="LANG" | RR.tab$Status=="NR" | RR.tab$Status=="NNA")] )*100
  RefR1.North <- RR.tab.North$Number[which(RR.tab.North$Status=="REFU")] / sum(RR.tab.North$Number[which(RR.tab.North$Status=="COMP" | RR.tab.North$Status=="PART" |RR.tab.North$Status=="REFU" | RR.tab.North$Status=="LANG" | RR.tab.North$Status=="NR"| RR.tab.North$Status=="NNA")] )*100
  RefR1.Central <- RR.tab.Central$Number[which(RR.tab.Central$Status=="REFU")] / sum(RR.tab.Central$Number[which(RR.tab.Central$Status=="COMP" | RR.tab.Central$Status=="PART" |RR.tab.Central$Status=="REFU" | RR.tab.Central$Status=="LANG" | RR.tab.Central$Status=="NR"| RR.tab.Central$Status=="NNA")] )*100
  RefR1.South <- RR.tab.South$Number[which(RR.tab.South$Status=="REFU")] / sum(RR.tab.South$Number[which(RR.tab.South$Status=="COMP" | RR.tab.South$Status=="PART" |RR.tab.South$Status=="REFU" | RR.tab.South$Status=="LANG" | RR.tab.South$Status=="NR" | RR.tab.South$Status=="NNA")] )*100
  
  
  
  # === Create a matrix of completed calls (based on the long form data)
  dat.long <- data_clean3
  Completedcalls <- dat.long[which(dat.long$Outcome2=="COMP"),]
  Lifted <- sum(plyr::count(dat.long$Outcome2[which(dat.long$Outcome2!="NNU" & dat.long$Outcome2!="NR")])$freq)
  
  # # === Call attempts 
  # Call.attempts <- nrow(dat.long)
  # 
  North <- dat.long[which(dat.long$Resp.Region=="Northern"),]; Call.attempts.North <- nrow(North)
  Central <- dat.long[which(dat.long$Resp.Region=="Central"),]; Call.attempts.Central <- nrow(Central)
  South <- dat.long[which(dat.long$Resp.Region=="Southern"),]; Call.attempts.South <- nrow(South)
  
  #n === Table of call attempts and completed calls 
  matrix.cc<- matrix(NA, 2,3); matrix.cc[1,] <- table(dat.long$Resp.Region)[c(1,2,3)]
  matrix.cc[2,] <- c(nrow(subset(Consentedmw, Resp.Region == 'Northern')), 
                     nrow(subset(Consentedmw, Resp.Region == 'Central')),
                     nrow(subset(Consentedmw, Resp.Region == 'Southern')))
  matrix.cc <- cbind(matrix.cc, c(sum(table(dat.long$Resp.Region)),sum(matrix.cc[2,])))
  rownames(matrix.cc) <- c("Calls placed", "Completed calls")
  colnames(matrix.cc) <- c("Northern", "Central", "Southern", "Total")
  
  # === Call attempts per completed call
  CPCC <- nrow(dat.long)/nrow(Completedcalls)
  CPCC.North <- matrix.cc[1,"Northern"]/matrix.cc[2,"Northern"]
  CPCC.Central <- matrix.cc[1,"Central"]/matrix.cc[2,"Central"]
  CPCC.South <- matrix.cc[1,"Southern"]/matrix.cc[2,"Southern"]
  
  # === % of numbers with completed CATI
  Perc.comp <- RR.tab$Number[which(RR.tab$Status=="COMP")] / sum(RR.tab$Number) *100
  Perc.comp.North <- RR.tab.North$Number[which(RR.tab.North$Status=="COMP")] / sum(RR.tab.North$Number) *100
  Perc.comp.Central <- RR.tab.Central$Number[which(RR.tab.Central$Status=="COMP")] / sum(RR.tab.Central$Number) *100
  Perc.comp.South <- RR.tab.South$Number[which(RR.tab.South$Status=="COMP")] / sum(RR.tab.South$Number) *100
  
  # --- Insert the values into a matrix for presentation - 1st col= Overall; 2nd=Kinshasa; 3rd=NK
  matrix.rates <- matrix(NA, 6, 4)
  matrix.rates[,1] <- c(RR1,# RR1.I1, RR1.I2, RR1.F,
                        RefR1,
                        CPCC, #CPCC.IVR1, CPCC.IVR2, CPCC.Ferox,
                        Perc.comp, #Perc.comp.IVR1, Perc.comp.IVR2, Perc.comp.Ferox,
                        matrix.cc[1,4],# sum(matrix.cc[1,3:4]),matrix.cc[1,5], sum(matrix.cc[1,1:2])),
                        matrix.cc[2,4]#, sum(matrix.cc[2,3:4]),matrix.cc[2,5], sum(matrix.cc[2,1:2]))
  )
  
  vals <- Consentedmw %>%
    dplyr::mutate(counter =1) %>%
    dplyr::group_by(Resp.Region) %>%
    dplyr::summarise(n=sum(counter)) 
  #vals$n[1]
  
  
  
  matrix.rates[,2] <- c(RR1.North ,#NA, RR1.IVR.Kin, NA, RR1.Feroxus.Kin,
                        RefR1.North,
                        CPCC.North,#NA, CPCC.IVR1.Kin, NA, CPCC.Ferox.Kin,
                        Perc.comp.North,#NA, Perc.comp.IVR1.Kin, NA, Perc.comp.Ferox.Kin,
                        matrix.cc[1,1],#c(NA, matrix.cc[1,3], NA, matrix.cc[1,1]),
                        matrix.cc[2,1])#c(vals$n[1], matrix.cc[2,3], IVR.vals$n[1], matrix.cc[2,1]))
  
  matrix.rates[,3] <- c(RR1.Central,#NA, RR1.IVR.NK, NA, RR1.Feroxus.NK,
                        RefR1.Central,
                        CPCC.Central,#NA, CPCC.IVR1.NK, NA, CPCC.Ferox.NK,
                        Perc.comp.Central,#NA, Perc.comp.IVR1.NK, NA, Perc.comp.Ferox.NK,
                        matrix.cc[1,2],#c(NA,matrix.cc[1,4] , NA, matrix.cc[1,2]),
                        matrix.cc[2,2])#c(vals$n[2],matrix.cc[2,4] , IVR.vals$n[2], matrix.cc[2,2]))
  matrix.rates[,4] <- c(RR1.South,#NA, RR1.IVR.NK, NA, RR1.Feroxus.NK,
                        RefR1.South,
                        CPCC.South,#NA, CPCC.IVR1.NK, NA, CPCC.Ferox.NK,
                        Perc.comp.South,#NA, Perc.comp.IVR1.NK, NA, Perc.comp.Ferox.NK,
                        matrix.cc[1,3],#c(NA,matrix.cc[1,4] , NA, matrix.cc[1,2]),
                        matrix.cc[2,3])#c(vals$n[2],matrix.cc[2,4] , IVR.vals$n[2], matrix.cc[2,2]))
  
  
  
  matrix.rates <- round(matrix.rates,2)
  matrix.rates[4:5,] <- as.character(round(matrix.rates[4:5,],1))
  matrix.rates <- as.data.frame(matrix.rates)
  colnames(matrix.rates) <- c("Malawi", "Northern", "Central",'Southern')
  # --- reorder the columns and name them/convert to DF
  matrix.rates<- matrix.rates %>%
    as.data.frame() %>%
    mutate(Statistic = c("Response rate", 
                         'Refusal rate',
                         "Attempts per complete CATI",
                         "% numbers w/complete CATI", 
                         "Calls placed", 
                         "CATIs completed")) %>%
    relocate(Statistic)
  
  return(matrix.rates)
  
}

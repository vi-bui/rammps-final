# function for call versus cati outcomes
function(data, dat.wide, type){
  # === Plot of the the Unique phone number outcomes
  dat.wide$Counter <- 1
  dat.wide$Source.number <- dat.wide$Source.1
  
  wide.plot.data.T <- rbind(dat.wide %>%
                              dplyr::group_by(Outcome.FINAL) %>%
                              dplyr::summarise(total = sum(Counter)) %>%
                              dplyr::mutate(rel.freq = (round(100 * total/sum(total), 2)),
                                            Source.1 = 'Overall') %>% as.data.frame(),
                            dat.wide %>%
                              dplyr::group_by(Outcome.FINAL,Source.1) %>%
                              dplyr::summarise(total = sum(Counter)) %>%
                              dplyr::ungroup() %>%
                              dplyr::group_by(Source.1) %>%
                              dplyr::mutate(rel.freq = (round(100 * total/sum(total), 2))) %>% as.data.frame())
  
  wide.plot.data.T$Source.1 <- factor(wide.plot.data.T$Source.1, levels = c('Overall','IVR group 1','IVR group 2','Feroxus'))
  
  
  wide.plot.data.T$Outcome.FINAL <- factor(wide.plot.data.T$Outcome.FINAL, 
                                           levels= c("COMP", "REFU","NR", "NNU", "INEL","LANG", "NNA", "PEND", "PART", 'DEFER'))
  
  # --- Plot as a bar chart
  wide.plot.data <- wide.plot.data.T %>%
    mutate(Eligibility = case_when(Outcome.FINAL %in% c('COMP','PART') ~ 'Eligible',
                                   Outcome.FINAL %in% c('INEL','DEFER','NNU') ~ 'Ineligible',
                                   Outcome.FINAL %in% c('REFU','NNA','NR','LANG','PEND') ~ 'Eligibility unknown'),
           Eligibility = factor(Eligibility, levels = c('Eligible','Ineligible','Eligibility unknown')),
           Outcome.FINAL = factor(Outcome.FINAL, levels = c('COMP','PART','REFU','NNU','NNA','NR','INEL','LANG','DEFER','PEND','OTHER')))
  Unique.bar <- ggplot(wide.plot.data) +
    geom_bar(aes(fill=Eligibility, y=rel.freq, x=Outcome.FINAL),position="dodge", stat="identity",alpha = 0.8) +
    geom_bar(aes(color=Eligibility, y=rel.freq, x=Outcome.FINAL),alpha = 0,size = 1,position="dodge", stat="identity" ) +
    geom_text(aes(x=Outcome.FINAL, y=rel.freq, label=rel.freq), vjust=1, color="black", size=2.5)+
    scale_fill_brewer(palette = 'Dark2') +
    scale_color_brewer(palette = 'Dark2') +
    facet_wrap(~Source.1) +
    theme_minimal() +
    theme(#legend.position="none",
      axis.text.x = element_text(angle = 15, vjust = 0.5, hjust=1, size = 9)) +
    xlab("")+ ylab("Percent of unique phone numbers") + 
    labs(caption=paste0("Feroxus = ",sum(wide.plot.data.T$total[wide.plot.data.T$Source.1=='Feroxus']), "; IVR group 1 = ",sum(wide.plot.data.T$total[wide.plot.data.T$Source.1=='IVR group 1']), "; IVR group 2 = ",sum(wide.plot.data.T$total[wide.plot.data.T$Source.1=='IVR group 2']),"; Overall = ",sum(wide.plot.data.T$total[wide.plot.data.T$Source.1=='Overall'])))
  
  IVR2 <- data[which(data$Source.prov=="IVR group 2"),]; Call.attempts.IVR2 <- nrow(IVR2)
  
  IVR1.Kin <- data[which(data$Source.prov=="IVR group 1-Kin"),]; Call.attempts.IVR1.Kin <- nrow(IVR1.Kin)
  Feroxus.Kin <- data[which(data$Source.prov=="Feroxus-Kin"),]; Call.attempts.Feroxus.Kin <- nrow(Feroxus.Kin)
  IVR1.NK <- data[which(data$Source.prov=="IVR group 1-NK"),]; Call.attempts.IVR1.NK <- nrow(IVR1.NK)
  Feroxus.NK <- data[which(data$Source.prov=="Feroxus-NK"),]; Call.attempts.Feroxus.NK <- nrow(Feroxus.NK)
  
  # === Table of call attempts and completed calls
  matrix.cc<- matrix(NA, 2,5); matrix.cc[1,] <- table(data$Source.prov)[c(1,2,3,4,5)]
  matrix.cc[2,] <- c(plyr::count(Feroxus.Kin$Outcome2[which(Feroxus.Kin$Outcome2=="COMP")])$freq,
                     plyr::count(Feroxus.NK$Outcome2[which(Feroxus.NK$Outcome2=="COMP")])$freq,
                     plyr::count(IVR1.Kin$Outcome2[which(IVR1.Kin$Outcome2=="COMP")])$freq,
                     plyr::count(IVR1.NK$Outcome2[which(IVR1.NK$Outcome2=="COMP")])$freq,
                     plyr::count(IVR2$Outcome2[which(IVR2$Outcome2=="COMP")])$freq)
  matrix.cc <- cbind(matrix.cc, c(sum(table(data$Source.prov)),sum(matrix.cc[2,])))
  rownames(matrix.cc) <- c("Calls placed", "Completed calls")
  colnames(matrix.cc) <- c("Feroxus-Kin", "Feroxus-NK", "IVR-Kin", "IVR-NK", "IVR2", "Total")
  
  # --- Create a dataframe of Outcomes by the source, with a counter
  plot.data <- data.frame(Outcome = data$Outcome2,
                          Source.number = data$Source.number,
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
                                    levels= c("COMP", "REFU","NR", "NNU", "INEL","LANG", "NNA", "PART",  "INCO", "REFER",  "REASS", 'DEFER',"Other"))
  final.plot.data <- drop_na(final.plot.data)
  
  # --- Plot as a bar chart
  Attempt.bar <- final.plot.data %>%
    mutate(Eligibility = case_when(Outcome %in% c('COMP') ~ 'Eligible',
                                   Outcome %in% c('INEL','DEFER','NNU') ~ 'Ineligible',
                                   Outcome %in% c('REFU','NNA','NR','LANG','REASS') ~ 'Eligibility unknown'),
           Eligibility = factor(Eligibility, levels = c('Eligible','Ineligible','Eligibility unknown')),
           Outcome = factor(Outcome, levels = c('COMP','REFU','NNU','NNA','NR','INEL','LANG','REASS','DEFER'))) %>%
    ggplot() + 
    geom_bar(aes(fill=Eligibility, y=rel.freq, x=Outcome),position="dodge", stat="identity",alpha = 0.8) +
    geom_bar(aes(color=Eligibility, y=rel.freq, x=Outcome),alpha = 0,size = 1,position="dodge", stat="identity" ) +
    scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#fb9a99","#66C2A5","#FC8D62", "#8dd3c7",'#a6cee3','#6a3d9a')) +
    scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#fb9a99","#66C2A5","#FC8D62", "#8dd3c7",'#a6cee3','#6a3d9a'))+
    geom_text(aes(x=Outcome, y=rel.freq, label=rel.freq), vjust=1, color="black", size=2.5)+
    facet_wrap(~Source.number) +
    theme_minimal() +
    theme(legend.position="none",
          axis.text.x = element_text(angle = 15, vjust = 0.5, hjust=1, size = 7)) +
    xlab("")+ ylab("Relative frequency of phone calls (%)") + 
    labs(caption=paste0("\nFeroxus n = ",(matrix.cc[1,2] + matrix.cc[1,1]), "; IVR group 1 = ",(matrix.cc[1,4] + matrix.cc[1,3]), "; IVR group 2 = ",matrix.cc[1,5],"; Overall = ",matrix.cc[1,6]))
  
    if (type == 'cati'){
    return(Unique.bar)
  } else if (type == 'call') {
    return(Attempt.bar)
  }
  
}
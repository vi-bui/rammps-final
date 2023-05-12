# function for call versus cati outcomes
function(data_clean3, dat.widemw, type){
  # === Plot of the the Unique phone number outcomes
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
  
  wide.plot.data.T <- dat.widemw %>%
    dplyr::group_by(Outcome.FINAL) %>%
    dplyr::summarise(total = sum(Counter)) %>%
    dplyr::mutate(rel.freq = (round(100 * total/sum(total), 2))) %>% as.data.frame() %>%
    dplyr::mutate(Eligibility = case_when(Outcome.FINAL %in% c('COMP','PART') ~ 'Eligible',
                                          Outcome.FINAL %in% c('INEL','DEFER','NNU') ~ 'Ineligible',
                                          Outcome.FINAL %in% c('REFU','NNA','NR','LANG','PEND') ~ 'Eligibility unknown'),
                  Eligibility = factor(Eligibility, levels = c('Eligible','Ineligible','Eligibility unknown')),
                  Outcome.FINAL = factor(Outcome.FINAL, levels = c('COMP','PART','REFU','NNU','NNA','NR','INEL','LANG','DEFER','PEND','OTHER')))
  
  # --- Plot as a bar chart
  Unique.bar <- ggplot(data = wide.plot.data.T)+ 
    geom_bar(aes(fill=Eligibility, y=rel.freq, x=Outcome.FINAL),position="dodge", stat="identity",alpha = 0.8) +
    geom_bar(aes(color=Eligibility, y=rel.freq, x=Outcome.FINAL),alpha = 0,size = 1,position="dodge", stat="identity" ) +
    geom_text(aes(x=Outcome.FINAL, y=rel.freq, label=rel.freq), vjust=1, color="black", size=2.5)+
    scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666","#fb9a99","#a6cee3")) +
    scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666","#fb9a99","#a6cee3")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 15, vjust = 0.5, hjust=1, size = 7)) +
    xlab("")+ ylab("Percent of unique phone numbers")  +
    labs(title = 'Overall CATI Outcomes')
  
  
  # --- Create a dataframe of Outcomes by the source, with a counter
  plot.data <- data.frame(Outcome = data_clean3$Outcome2,
                          month = data_clean3$month.interview)
  final.plot.data <- plot.data %>%
    dplyr::group_by(Outcome) %>%
    dplyr::tally() %>%
    dplyr::mutate(rel.freq = (round(100 * n/sum(n), 2))) %>% as.data.frame() %>%
    mutate(Eligibility = case_when(Outcome %in% c('COMP') ~ 'Eligible',
                                   Outcome %in% c('INEL','DEFER','NNU') ~ 'Ineligible',
                                   Outcome %in% c('REFU','NNA','NR','LANG','REASS') ~ 'Eligibility unknown'),
           Eligibility = factor(Eligibility, levels = c('Eligible','Ineligible','Eligibility unknown')),
           Outcome = factor(Outcome, levels = c('COMP','REFU','NNU','NNA','NR','INEL','LANG','REASS','DEFER')))
  
  final.plot.data <- drop_na(final.plot.data)
  
  # --- Plot as a bar chart
  Attempt.bar <- ggplot(final.plot.data) +
    geom_bar(aes(fill=Eligibility, y=rel.freq, x=Outcome),position="dodge", stat="identity",alpha = 0.8) +
    geom_bar(aes(color=Eligibility, y=rel.freq, x=Outcome),alpha = 0,size = 1,position="dodge", stat="identity" ) +
    scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#fb9a99","#66C2A5","#FC8D62", "#8dd3c7",'#a6cee3','#6a3d9a')) +
    scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#fb9a99","#66C2A5","#FC8D62", "#8dd3c7",'#a6cee3','#6a3d9a'))+
    geom_text(aes(x=Outcome, y=rel.freq, label=rel.freq), vjust=1, color="black", size=2.5)+
    theme(axis.text.x = element_text(angle = 15, vjust = 0.5, hjust=1, size = 7)) +
    xlab("")+ ylab("Relative frequency of phone calls (%)") + 
    labs(title = 'Overall Call Outcomes')
  
  if (type == 'cati'){
    return(Unique.bar)
  } else if (type == 'call') {
    return(Attempt.bar)
  }
  
}
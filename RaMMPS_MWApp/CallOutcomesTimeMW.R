# call outcoems over time
function(data_clean3, monthtoplot) {
# --- Create a dataframe of Outcomes by the source, with a counter
  plot.data <- data.frame(Outcome = data_clean3$Outcome2,
                          month = data_clean3$month.interview)

  final.plot.data <- plot.data %>%
    dplyr::group_by(Outcome,month) %>%
    dplyr::tally() %>%
    dplyr::group_by(month) %>%
    dplyr::mutate(rel.freq = (round(100 * n/sum(n), 2)),
                  Eligibility = case_when(Outcome %in% c('COMP','INCO') ~ 'Eligible',
                                 Outcome %in% c('INEL','DEFER','NNU') ~ 'Ineligible',
                                 Outcome %in% c('REFU','NNA','NR','LANG','REASS','Other') ~ 'Eligibility unknown'),
         Eligibility = factor(Eligibility, levels = c('Eligible','Ineligible','Eligibility unknown')),
         Outcome = factor(Outcome, levels = c('COMP','REFU','NNU','NNA','NR','INEL','LANG','REASS','DEFER','Other')))

final.plot.data <- drop_na(final.plot.data)
final.plot.data$month <- factor(final.plot.data$month, 
                                levels = c('January', 'February','March','April','May','June','July','August','September','October','November','December'))

calloutcomestime <- function(monthtoplot){
  bymonth <- final.plot.data %>% 
    filter(month == monthtoplot) 
  
  ggplot(data = bymonth) +
    geom_bar(aes(fill=Eligibility, y=rel.freq, x=Outcome),position="dodge", stat="identity",alpha = 0.8) +
    geom_bar(aes(color=Eligibility, y=rel.freq, x=Outcome),alpha = 0,size = 1,position="dodge", stat="identity" ) +
    scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#fb9a99","#66C2A5","#FC8D62", "#8dd3c7",'#a6cee3','#6a3d9a')) +
    scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#fb9a99","#66C2A5","#FC8D62", "#8dd3c7",'#a6cee3','#6a3d9a'))+
    geom_text(aes(x=Outcome, y=rel.freq, label=rel.freq), vjust=1, color="black", size=2.5)+
    theme(axis.text.x = element_text(angle = 15, vjust = 0.5, hjust=1, size = 7)) +
    xlab("")+ ylab("Relative frequency of phone calls (%)") + 
    labs(title = monthtoplot)
}

return(calloutcomestime(monthtoplot))

}
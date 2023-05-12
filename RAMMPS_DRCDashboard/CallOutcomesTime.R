# call outcoems over time
function(data, monthtoplot) {
# --- Create a dataframe of Outcomes by the source, with a counter
plot.data <- data.frame(Outcome = data$Outcome2,
                        Source.number = data$Source.number,
                        month = data$month.interview,
                        val = 1)
# --- Collapse data for totals and relative freq of outcomes BY the source from which it was sampled 
plot.data.S <- plot.data %>%
  dplyr::group_by(Outcome,Source.number, month) %>%
  dplyr::summarise(total = sum(val)) %>%
  dplyr::group_by(Source.number, month) %>%
  dplyr::mutate(rel.freq = (round(100 * (total/sum(total)), 2))) %>% as.data.frame()


# --- Collapse data for totals and relative freq of outcomes in the whole data
plot.data.T <- plot.data %>%
  dplyr::group_by(Outcome, month) %>%
  dplyr::summarise(total = sum(val)) %>%
  dplyr::group_by(month) %>%
  dplyr::mutate(rel.freq = (round(100 * total/sum(total), 2)),
                Source.number ="Overall") %>% as.data.frame()
# --- Stick the Source disaggragated ones and total ones together and reorder the factors
final.plot.data <- rbind(plot.data.T, plot.data.S) %>%
  dplyr::mutate(Eligibility = case_when(Outcome %in% c('COMP') ~ 'Eligible',
                                 Outcome %in% c('INEL','DEFER','NNU') ~ 'Ineligible',
                                 Outcome %in% c('REFU','NNA','NR','LANG','REASS') ~ 'Eligibility unknown'),
         Eligibility = factor(Eligibility, levels = c('Eligible','Ineligible','Eligibility unknown')),
         Outcome = factor(Outcome, levels = c('COMP','REFU','NNU','NNA','NR','INEL','LANG','REASS','DEFER')))

final.plot.data$Source.number <- factor(final.plot.data$Source.number, levels = c('Overall','IVR group 1','IVR group 2','Feroxus'))
final.plot.data <- final.plot.data[!(final.plot.data$Outcome=="missing"),]

# final.plot.data$Outcome <- factor(final.plot.data$Outcome, 
#                                   levels = c("COMP", "PART", "REFU","NNA","NR","NNU", "LANG","INEL", "INCO","REFER", "REASS",'DEFER', "Other"))

final.plot.data <- drop_na(final.plot.data)
final.plot.data$month <- factor(final.plot.data$month, 
                                levels = c('Aug-21', 'Sep-21','Oct-21','Nov-21','Dec-21','Jan-22', 'Feb-22','Mar-22','Apr-22','May-22','Jun-22','Jul-22','Aug-22'))

calloutcomestime <- function(monthtoplot){
  bymonth <- final.plot.data %>% 
    filter(month == monthtoplot) 
  
  ggplot(data = bymonth) +
    geom_bar(aes(fill=Eligibility, y=rel.freq, x=Outcome),position="dodge", stat="identity",alpha = 0.8) +
    geom_bar(aes(color=Eligibility, y=rel.freq, x=Outcome),alpha = 0,size = 1,position="dodge", stat="identity" ) +
    scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#fb9a99","#66C2A5","#FC8D62", "#8dd3c7",'#a6cee3','#6a3d9a')) +
    scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#fb9a99","#66C2A5","#FC8D62", "#8dd3c7",'#a6cee3','#6a3d9a'))+
    geom_text(aes(x=Outcome, y=rel.freq, label=rel.freq), vjust=1, color="black", size=2.5)+
    # scale_fill_identity() +
    facet_wrap(~Source.number)+
    theme(legend.position="none",
          axis.text.x = element_text(angle = 15, vjust = 0.5, hjust=1, size = 7)) +
    xlab("")+ ylab("Relative frequency of phone calls (%)") + 
    labs(caption=paste0("\nFeroxus n = ",(sum(bymonth[bymonth$Source.number=='Feroxus',]$total)),"; IVR group 1 = ",(sum(bymonth[bymonth$Source.number=='IVR group 1',]$total)),"; IVR group 2 = ",(sum(bymonth[bymonth$Source.number=='IVR group 2',]$total)),"; Overall = ",(sum(bymonth[bymonth$Source.number=='Overall',]$total))),
         title = monthtoplot)
}

return(calloutcomestime(monthtoplot))

}
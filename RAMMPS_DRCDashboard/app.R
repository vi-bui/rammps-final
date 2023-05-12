# Libraries
# Load function to install list of packages

Sys.setenv(TZ ='GMT')

require(dplyr)
require(shinydashboard)
require(readr)
require(epitools)
require(ggpubr)
require(shiny)
require(knitr)
require(tidyselect)
require(gridExtra)
require(tidyr)
require(lubridate)
require(kableExtra)
require(arrow)

ggplot2::theme_set(theme_minimal())

data <- arrow::read_parquet("Data_Long_sml.parquet") %>%#readr::read_csv("Data_Long_sml.csv") %>%#
  as.data.frame() %>%
  dplyr::mutate(Date.Interview = as.Date(Date.Interview)) %>%
  dplyr::mutate(enumerator = ordered(enumerator, levels = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10",
                                                            'E11', 'E12', 'E13', 'E14', 'E15', 'E16', 'E17', 'E18', 'E19', 'E20'))) %>%
  dplyr::filter(!is.na(Outcome2)) %>%
  mutate(call_num_grp = ifelse(as.numeric(call_num) > 5, '5+',as.character(call_num)))
Consented <- arrow::read_parquet('Consented_sml.parquet') %>% #readr::read_csv("Consented_sml.csv") %>%#
  as.data.frame() %>%
  dplyr::mutate(Date.Interview = as.Date(Date.Interview)) %>%
  dplyr::mutate(enumerator = ordered(enumerator, levels = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10",
                                                            'E11', 'E12', 'E13', 'E14', 'E15', 'E16', 'E17', 'E18', 'E19', 'E20')))
dat.wide <- arrow::read_parquet("Data_Wide_sml.parquet") %>%#
  as.data.frame() %>%
  dplyr::mutate(enum = ordered(enum, levels = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10",
                                                'E11', 'E12', 'E13', 'E14', 'E15', 'E16', 'E17', 'E18', 'E19', 'E20')))
#Dividing into two provinces
NK <- Consented[which(Consented$Resp.Region==2),]
Kin <- Consented[which(Consented$Resp.Region==1),]
###########################################

# For OutcomebyOrder and Compbycall
outcomes <- dat.wide %>%
  mutate(call_num_grp = ifelse(call_num > 5, '5+', as.character(call_num))) %>%
  dplyr::group_by(call_num_grp, Outcome.FINAL) %>%
  dplyr::summarize(n = n()) %>%
  ungroup() %>%
  filter(Outcome.FINAL == 'COMP') %>%
  dplyr::mutate(totalcomp = sum(n),
                totalnums = nrow(dat.wide),
                perc = round(n/totalnums*100,2),
                perccompletedpercallnum=round(n/totalcomp*100,2)) %>%
  ungroup() %>%
  dplyr::mutate(perccum = cumsum(perc),
                country = 'DRC')
outcomes$Calls <- c(table(data$call_num_grp))
###########################################

##########################################
# === Table of progress to quota
# --- Create a table of Age vs sex by region and then proportions - block 1
quota.count <- table(Consented[Consented$Date.Interview <= '2021-12-31',]$Resp.Age.grp,
                     Consented[Consented$Date.Interview <= '2021-12-31',]$Resp.Sex,
                     Consented[Consented$Date.Interview <= '2021-12-31',]$Resp.Region)
quota.prop <- round(prop.table(table(Consented[Consented$Date.Interview <= '2021-12-31',]$Resp.Age.grp,
                                     Consented[Consented$Date.Interview <= '2021-12-31',]$Resp.Sex,
                                     Consented[Consented$Date.Interview <= '2021-12-31',]$Resp.Region))*100,1)

# --- Wrap this into a matrix for counts of completed interviews
quotascount <- (as.matrix(cbind(quota.count[,,1], quota.count[,,2])))
quotascount <- rbind(quotascount, colSums(quotascount))
quotascount <- cbind(quotascount, rowSums(quotascount))
rownames(quotascount) <- c("18-39", "40-64", "Total")
colnames(quotascount) <- c("Kinshasa: Women", "Kinshasa: Men", "Nord Kivu: Women", "Nord Kivu: Men", "Total")
quotascount.tab <-quotascount

colnames(quotascount.tab) <- c("Women", "Men", "Women", "Men", "Total")

# --- matrix3 = table of % of quota reached
quotasperc1 <- as.data.frame(quotascount) %>%
  dplyr::mutate(quotaKmen = c(1284,521,(521+1284)),
                quotaKwomen = c(1386,560,(1386+560)),
                quotaNKmen = c(1284,521,(521+1284)),
                quotaNKwomen = c(1386,560,(1386+560)),
                percKmen = round(`Kinshasa: Men`/quotaKmen*100,2),
                percKwomen = round(`Kinshasa: Women`/quotaKwomen*100,2),
                percNKmen = round(`Nord Kivu: Men`/quotaNKmen*100,2),
                percNKwomen = round(`Nord Kivu: Women`/quotaNKwomen*100,2)) %>%
  dplyr::select(c(percKwomen, percKmen, percNKwomen, percNKmen))
colnames(quotasperc1) <- c(c("Women", "Men", "Women", "Men"))

# #matrix for block 2 of quota %
# # --- Create a table of Age vs sex by region and then proportions
quota.count2 <- table(Consented[Consented$Date.Interview > '2021-12-31'& Consented$Date.Interview < '2022-06-01',]$Resp.Age.grp,
                      Consented[Consented$Date.Interview > '2021-12-31'& Consented$Date.Interview < '2022-06-01',]$Resp.Sex,
                      Consented[Consented$Date.Interview > '2021-12-31'& Consented$Date.Interview < '2022-06-01',]$Resp.Region)
quota.prop2 <- round(prop.table(table(Consented[Consented$Date.Interview > '2021-12-31'& Consented$Date.Interview < '2022-06-01',]$Resp.Age.grp,
                                      Consented[Consented$Date.Interview > '2021-12-31'& Consented$Date.Interview < '2022-06-01',]$Resp.Sex,
                                      Consented[Consented$Date.Interview > '2021-12-31'& Consented$Date.Interview < '2022-06-01',]$Resp.Region))*100,1)

# # --- Wrap this into a matrix for counts of completed interviews
quotascount2 <- (as.matrix(cbind(quota.count2[,,1], quota.count2[,,2])))
quotascount2 <- rbind(quotascount2, colSums(quotascount2))
quotascount2 <- cbind(quotascount2, rowSums(quotascount2))
rownames(quotascount2) <- c("18-39", "40-64", "Total")
colnames(quotascount2) <- c("Kinshasa: Women", "Kinshasa: Men", "Nord Kivu: Women", "Nord Kivu: Men", "Total")
quotascount.tab2 <-quotascount2

colnames(quotascount.tab2) <- c("Women", "Men", "Women", "Men", "Total")

# # --- matrix3 = table of % of quota reached
quotasperc2 <- as.data.frame(quotascount2) %>%
  dplyr::mutate(quotaKmen = c(1284,521,(521+1284)),
                quotaKwomen = c(1386,560,(1386+560)),
                quotaNKmen = c(1284,521,(521+1284)),
                quotaNKwomen = c(1386,560,(1386+560)),
                percKmen = round(`Kinshasa: Men`/quotaKmen*100,2),
                percKwomen = round(`Kinshasa: Women`/quotaKwomen*100,2),
                percNKmen = round(`Nord Kivu: Men`/quotaNKmen*100,2),
                percNKwomen = round(`Nord Kivu: Women`/quotaNKwomen*100,2)) %>%
  dplyr::select(c(percKwomen, percKmen, percNKwomen, percNKmen))
colnames(quotasperc2) <- c(c("Women", "Men", "Women", "Men"))

# block 3
quota.count3 <- table(Consented[Consented$Date.Interview > '2022-06-01',]$Resp.Age.grp,
                      Consented[Consented$Date.Interview > '2022-06-01',]$Resp.Sex,
                      Consented[Consented$Date.Interview > '2022-06-01',]$Resp.Region)
# # --- Wrap this into a matrix for counts of completed interviews
quotascount3 <- (as.matrix(cbind(quota.count3[,,1], quota.count3[,,2])))
quotascount3 <- rbind(quotascount3, colSums(quotascount3))
quotascount3 <- cbind(quotascount3, rowSums(quotascount3))
rownames(quotascount3) <- c("18-39", "40-64", "Total")
colnames(quotascount3) <- c("Kinshasa: Women", "Kinshasa: Men", "Nord Kivu: Women", "Nord Kivu: Men", "Total")
quotascount.tab3 <-quotascount3

colnames(quotascount.tab3) <- c("Women", "Men", "Women", "Men", "Total")

### Get list of months for drop down list
Consented$months <- ordered(Consented$month.interview, c('Aug-21', 'Sep-21','Oct-21','Nov-21',
                                                           'Dec-21','Jan-22', 'Feb-22','Mar-22',
                                                           'Apr-22','May-22','Jun-22','Jul-22',
                                                           'Aug-22', 'Sep-22','Oct-22','Nov-22',
                                                           'Dec-22','Jan-23','Feb-23','Mar-23',
                                                           'Apr-23','May-23','Jun-23'))
months <- Consented %>% dplyr::group_by(months) %>% dplyr::summarize()
list_mos <- unique(months$months) %>% as.character
##############################################################

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "united"),
  tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: '#1B9E77'}")),
  #Application title
  titlePanel(div(img(src="RAMMPS-FinalLogoHighRes.png", height = 70, width = 150),"RaMMPS DRC RESULTS")),#div(img(src="RAMMPS-FinalLogoHighRes.png", height = 70, width = 150),
  tabsetPanel(
    # tab 1
    tabPanel("CATI Interviews",
             fluidRow(
               column(1, 
                      selectInput('chooseenum', h5("Choose enumerator:"),
                                  choices = c('All',as.character(ordered(Consented$enumerator,
                                                                         levels = c('E1','E2','E3','E4','E5','E6','E7',
                                                                                    'E8','E9','E10','E11','E12','E13',
                                                                                    'E14','E15','E16','E17','E18','E19','E20'))) %>% sort()))
               ),
               column(6,
                      plotOutput("CATIdayandcum")
               ),
               column(2, radioButtons('choosequota', h5('Choose quota statistic:'),
                                      choices = c('Block 1 - Progress (n) to quota','Block 1 - Percent of quota',
                                                  'Block 2 - Progress (n) to quota','Block 2 - Percent of quota',
                                                  'Block 3 - Progress (n) to quota'),
                                      selected = 'Block 3 - Progress (n) to quota')),
               column(3,
                      conditionalPanel(
                        condition = "input.choosequota == 'Block 1 - Progress (n) to quota'", tableOutput('CATIquota1')
                      ),
                      conditionalPanel(
                        condition = "input.choosequota == 'Block 1 - Percent of quota'", tableOutput('percCATIquota1')
                      ),
                      conditionalPanel(
                        condition = "input.choosequota == 'Block 2 - Progress (n) to quota'", tableOutput('CATIquota2')
                      ),
                      conditionalPanel(
                        condition = "input.choosequota == 'Block 2 - Percent of quota'", tableOutput('percCATIquota2')
                      ),
                      conditionalPanel(
                        condition = "input.choosequota == 'Block 3 - Progress (n) to quota'", tableOutput('CATIquota3')
                      )
               )
             ),
             fluidRow(
               column(2,
                      sliderInput("slider", label = h5("Date Range:"), min = min(ymd(Consented$Date.Interview)),
                                  max = max(ymd(Consented$Date.Interview)), value = c(min(ymd(Consented$Date.Interview)), max(ymd(Consented$Date.Interview))))
               ),
               column(5,
                      plotOutput('Outcomebyorder')
               ),
               column(5,
                      plotOutput('Compbycall')
               )
             )),
    # tab 2
    tabPanel("Response Rates",
             fluidRow(
               column(1,
                      radioButtons("whichplot",h5("Choose outcome"),
                                   choices = c('CATI Outcomes','Call Outcomes'),
                                   selected = 'CATI Outcomes'),
               ),
               column(7,
                      # only show this panel if they choose CATI Outcomes
                      conditionalPanel(
                        condition = "input.whichplot =='CATI Outcomes'", plotOutput("catioutcomes",height = "600px")
                      ),
                      # only show this panel if they choose Call Outcomes
                      conditionalPanel(
                        condition = "input.whichplot =='Call Outcomes'", plotOutput("calloutcomes",height = "600px")
                      )
               ),
               column(4,
                      conditionalPanel(
                        condition = "input.whichplot =='CATI Outcomes'", htmlOutput('catioutcometext',height = "500px")
                      ),
                      conditionalPanel(
                        condition = "input.whichplot =='Call Outcomes'", htmlOutput("calloutcometext",height = "500px")
                      )
               )),
             fluidRow(
               column(2,
                      selectInput('choosemonth2',h5('Choose month to filter plot:'),
                                  choices = c('Overall', list_mos),
                                  selected = 'Overall') #'Aug-21', 'Sep-21','Oct-21','Nov-21','Dec-21','Jan-22', 'Feb-22','Mar-22','Apr-22','May-22','Jun-22','Jul-22','Aug-22'
               ),
               column(2,
                      selectInput('choosesample',h5('Choose sample group:'),
                                  choices = c('Overall','IVR group 1','IVR group 2','Feroxus'))),
               column(7,
                      tableOutput('RRtable')),
               column(1,
                      verbatimTextOutput('RRnotes'))
             )),
    # tab 3
    tabPanel("Interview Attributes",
             fluidRow(
               column(1,
                      selectInput('chooseenum2', h5("Choose enumerator:"),
                                  choices = c('All',as.character(ordered(Consented$enumerator,
                                                                         levels = c('E1','E2','E3','E4','E5','E6','E7',
                                                                                    'E8','E9','E10','E11','E12','E13',
                                                                                    'E14','E15','E16','E17','E18','E19','E20'))) %>% sort()))
               ),
               column(5,
                      plotOutput('phoneappduration',height = "400px")
               ),
               column(6,
                      plotOutput('moduletiming',height = "400px")
               )
             ),
             fluidRow(
               column(6,
                      plotOutput('avgintsperdayperenum', height = '400px')
               ),
               column(1,
                      radioButtons('camembert', h5('Choose plot:'),
                                   choices = c('Interview location','Phone ownership'),
                                   selected = 'Interview location')
               ),
               column(5,
                      conditionalPanel(
                        condition = "input.camembert == 'Interview location'", plotOutput('intloc',height = "400px")
                      ),
                      conditionalPanel(
                        condition = "input.camembert == 'Phone ownership'", plotOutput('phoneowner',height = "400px"))
               ),
             )
    ),
    # tab 4
    tabPanel('Enumerator Statistics',
             fluidRow(
               column(1,
                      radioButtons("whichplot2",h5("Choose outcome"),
                                   choices = c('Completed interviews','Call attempts'),
                                   selected = 'Completed interviews'),
               ),
               column(5,
                      # only show this panel if they choose CATI Outcomes
                      conditionalPanel(
                        condition = "input.whichplot2 =='Call attempts'", plotOutput("callsperint",height = "400px")
                      ),
                      # only show this panel if they choose Call Outcomes
                      conditionalPanel(
                        condition = "input.whichplot2 =='Completed interviews'", plotOutput("compperint",height = "400px")
                      )
               ),
               column(6,
                      plotOutput('catibyenum',height = "400px")
               )
             ),
             fluidRow(
               column(1,
                      selectInput('choosemonth',h5('Choose month to filter plots:'),
                                  choices = c('Overall',list_mos),
                                  selected = 'Overall')#'Aug-21', 'Sep-21','Oct-21','Nov-21','Dec-21','Jan-22', 'Feb-22','Mar-22','Apr-22','May-22','Jun-22','Jul-22','Aug-22' 2 closed parentheses here
               ),
               column(6,
                      plotOutput('timeonphone',height = "400px")
               ),
               column(5,
                      plotOutput('timeonphoneCATI',height = "400px"))
             )
    ),
    # tab 5
    tabPanel('Sociodemographic Characteristics',
             fluidRow(
               column(1,
                      selectInput("bymonth",h5("Choose month to filter age and sex distribution"),
                                  choices = c('Overall',list_mos),#'Aug-21', 'Sep-21','Oct-21','Nov-21','Dec-21','Jan-22', 'Feb-22','Mar-22','Apr-22','May-22','Jun-22','Jul-22','Aug-22',
                                  selected = 'Overall')
               ),
               column(6,
                      plotOutput('agesexdist',height = "800px")
               ),
               column(5,
                      div(style='height:800px; overflow-y: scroll',
                          tableOutput("sociodemotbl")
                      )
               ))),
    # tab 6
    tabPanel('Household Membership and Deaths/Parental Survival',
             fluidRow(
               column(4,
                      plotOutput('CDRandparentdeaths')
               ),
               column(4,
                      plotOutput('HHsize')
               ),
               column(4,
                      plotOutput('decparentbyprov'))
             ),
             fluidRow(
               column(5,
                      plotOutput('survparentsbyage')
               ),
               column(1,
                      selectInput('chooseparent', h5('Choose parents to filter:'),
                                  choices = c('Both','Mother','Father'))
               ),
               column(6,
                      plotOutput('agesofsurvparentsbyage'))
             )),
    # tab 7
    tabPanel('Sibling Survival',
             fluidRow(column(4,
                             tableOutput('numsibdeaths'),
             ),
             column(4,
                    plotOutput('sibsbyprov')
             ),
             column(4,
                    plotOutput('sibsbysex'))
             ),
             fluidRow(column(6,
                             plotOutput('decsibsbyrespage'),
             ),
             column(6,
                    plotOutput('distribbyrespage')))),
    # tab 8
    tabPanel('Deaths since 2019',
             fluidRow(column(6,
                             tableOutput('causeofdeath')
             ),
             column(6,
                    tableOutput('placeofdeath'),
             )),
             fluidRow(column(6,
                             tableOutput('placeofburial')
             ),
             column(6,
                    tableOutput('registereddeath')
             ))
    ))
)




server <- function(input, output, session) {
  #tab 1
  #Subset so that only the selected rows are in model.data
  Consented_enum <- reactive({
    if (input$chooseenum == "All") {
      Consented
    } else {
      Consented[Consented$enumerator == input$chooseenum,]
    }
  })

  output$CATIdayandcum <- renderPlot({
    Consented_enum <- Consented_enum() %>%
      dplyr::arrange(Date.Interview) %>%
      dplyr::filter(Date.Interview >= input$slider[1] & Date.Interview <= input$slider[2])
    Counts <- aggregate(data.frame(count = Consented_enum$Date.Interview),
                        list(value = Consented_enum$Date.Interview),
                        length)[,2]
    Cumulative <- cumsum(Counts)
    Date <- unique(Consented_enum$Date.Interview)
    dayandcum <- data.frame(Counts=Counts, Cumulative=Cumulative, Date= Date) %>%
      dplyr::arrange(Date)
    ggplot(dayandcum) +
      geom_bar(aes(x=Date, y=Counts), stat="identity", fill='#D95F02',color="black") +
      geom_text(aes(x=Date, y=Counts, label=Counts), vjust=1.6, color="black", size=3.5)+
      theme_minimal() +
      xlab("Date") + ylab("Completed Interviews") +
      geom_line(aes(x = Date, y=Cumulative/62), group =1,colour="#1B9E77", size = 1) +
      geom_text(data = dayandcum[which.max(dayandcum[,"Date"]),],aes(x = Date, y = Cumulative/62, label=Cumulative), size=4, vjust = 0.8)+
      scale_y_continuous(
        name = "Daily Completed interviews",
        sec.axis = sec_axis( trans=~.*62, name="Cumulative Completed interviews"))+
      scale_x_date(date_breaks = '2 days') +
      labs(title = paste0('Completed CATI interviews, by day and cumulative',input$enumerators))+
      theme(axis.title.y = element_text(color = '#D95F02', size=13),
            axis.title.y.right = element_text(color = "#1B9E77", size=13),
            axis.text.x = element_text(angle = 90)
      )
  })

  output$CATIquota1 <- function(){
    quotascount.tab1 <- quotascount.tab %>%
      kbl(caption = "Progress to quota - Block 1 (Sep-Dec 2021)") %>%
      kable_classic(full_width = F, html_font = "Arial")%>%
      kable_styling(latex_options = 'hold_position')
    add_header_above(quotascount.tab1, c(" " =1 ,"Kinshasa " = 2,"Nord Kivu " = 2 , " "=1))
  }

  output$percCATIquota1 <- function(){
    quotasperc1 <- quotasperc1 %>%
      kbl(caption = "Percent of quota collected - Block 1 (Sep-Dec 2021)") %>%
      kable_classic(full_width = F, html_font = "Arial") %>%
      kable_styling(latex_options = 'hold_position')
    add_header_above(quotasperc1, c(" " =1 ,"Kinshasa " = 2,"Nord Kivu " = 2 ))
  }

  output$CATIquota2 <- function(){
    quotascount.tab2 <- quotascount.tab2 %>%
      kbl(caption = "Progress to quota - Block 2 (Jan-Apr 2022)") %>%
      kable_classic(full_width = F, html_font = "Arial")%>%
      kable_styling(latex_options = 'hold_position')
    add_header_above(quotascount.tab2, c(" " =1 ,"Kinshasa " = 2,"Nord Kivu " = 2 , " "=1))
  }

  output$CATIquota3 <- function(){
    quotascount.tab3 <- quotascount.tab3 %>%
      kbl(caption = "Progress to quota - Block 3 (Jun-Jul 2022)") %>%
      kable_classic(full_width = F, html_font = "Arial")%>%
      kable_styling(latex_options = 'hold_position')
    add_header_above(quotascount.tab3, c(" " =1 ,"Kinshasa " = 2,"Nord Kivu " = 2 , " "=1))
  }

  output$percCATIquota2 <- function(){
    quotasperc2 %>%
      kbl(caption = "Percent of quota collected - Block 2 (Jan-Apr 2022)") %>%
      kable_classic(full_width = F, html_font = "Arial") %>%
      kable_styling(latex_options = 'hold_position') %>%
      add_header_above(c(" " =1 ,"Kinshasa " = 2,"Nord Kivu " = 2 ))
  }

  output$Outcomebyorder <- renderPlot({
    ggplot(data = subset(outcomes, Outcome.FINAL == 'COMP') ) +
      geom_line(aes(x = call_num_grp, y = perccompletedpercallnum), group=1, color = '#D95F02', size = 1.2) +
      geom_text(aes(x = call_num_grp, y = perccompletedpercallnum, label = paste0(perccompletedpercallnum,'%')), color = 'black',fontface = 'bold',vjust = 1.2) +
      xlab('Call attempt order') +
      ylab('% of completed CATIs per call num') +
      theme(legend.position = 'none') +
      coord_cartesian(ylim = c(0,70))
  })

  output$Compbycall <- renderPlot({
    ggplot(data = outcomes) +
      geom_line(aes(x = call_num_grp, y = perccum), group=1, color = '#D95F02', size = 0.8) +
      geom_bar(aes(y = log(Calls)*1.5, x = call_num_grp), stat = 'identity', fill = '#1B9E77', color = 'black') +
      geom_text(aes(x = call_num_grp, y = perccum, label = paste(perccum,'%')), fontface = 'bold', vjust = 1, color = 'black') +
      geom_text(aes(y = log(Calls)*1.5, x = call_num_grp, label = Calls), fontface = 'bold', vjust = 1, color = 'black') +
      xlab('Call attempt order') +
      scale_y_continuous(
        name = "Cumulative percentage of phone #s with completed CATI",
        sec.axis = sec_axis( trans=~exp(.)/1.5, name="Phone calls made (log scale)", breaks = c(200,1000,10000))) +
      theme(axis.title.y = element_text(color = '#D95F02', size=12),
            axis.title.y.right = element_text(color = "#1B9E77", size=12)) +
      coord_cartesian(ylim = c(0,32))
  })

  # tab 2
  # outcomesfunc <- dget('Outcomes_func.R')

  output$catioutcomes <- renderPlot({
    catitime <- dget('CATIOutcomesTime.R')
    if(input$choosemonth2 == 'Overall') {
      outcomesfunc(data, dat.wide, type = 'cati')
    }
    else {
      catitime(dat.wide, monthtoplot = input$choosemonth2)
    }
  })


  output$calloutcomes <- renderPlot({
    calltime <- dget('CallOutcomesTime.R')

    if(input$choosemonth2 == 'Overall') {
      outcomesfunc(data, dat.wide, type = 'call')
    }
    else {
      calltime(data, monthtoplot = input$choosemonth2)
    }
  })

  output$catioutcometext <- renderUI({HTML("COMP = Completed interview<br/>
            PART = Partially completed interview (no callback)<br/>
            REFU  = Refusal to participate<br/>
            NR = Functional number, but not reached after 5 attempts<br/>
            NNU = Phone number not in operation (not known on the network)<br/>
            INEL = Respondent is not eligible (living outside the Kinshasa or Nord Kivu or not in the eligible age range), and no deferral or referral was possible<br/>
            LANG = Respondent answered the phone, but spoke a language not spoken by any enumerators<br/>
            PEND = An interview that is currently pending and not reached 5 failed attempts<br/>
            NNA = The number is in operation but unable to be accessed because it is switched off or doesn't have network coverage<br/>
            REFER = Respondent is not eligible, referred other household member")})

  output$calloutcometext <- renderUI({HTML("COMP = Completed interview<br/>
            REFU  = Refusal to participate<br/>
            NNU =  Phone number not in operation (not known on the network) <br/>
            INEL = Respondent is not eligible due to residence or age, and no deferral or referral was possible<br/>
            LANG = Respondent answered the phone, but did not speak the same language as any enumerator<br/>
            INCO = Interview has begun, but has not been completed (callback required)<br/>
            INCOR = Phone call was answered, but not by the intended respondent<br/>
            NR = The call rang through, but no answer/busy<br/>
            NNA = The number is in operation but unable to be accessed because it is switched off or doesn't have network coverage<br/>
            REFER = Respondent is not eligible, referred other household member<br/>
            REASS = Respondent answered the phone, but spoke a language not spoken by any enumerators<br/>
            DEFER = Respondent is not eligible at this time (quota filled) and was deferred<br/>
            Other = Any other outcome")})

  output$RRtable <- function(){
    RRdata <- dget('ResponseRateFunc.R')
    print('rrfunc function called')
    RRtbl <- RRdata(data, dat.wide, Consented)
    print('rrfunc created table')
    stats <- RRtbl %>% dplyr::filter(Statistic!='')
    stats <- stats[,1]
    RRtbl1 <- RRtbl %>%
      dplyr::select(-Statistic) %>%
      dplyr::filter(if (input$choosesample == "Overall") {
        Sample == 'Overall'
      } else if (input$choosesample == 'IVR group 1') {
        Sample == 'IVR1'
      } else if (input$choosesample == 'IVR group 2') {
        Sample == 'IVR2'
      } else {
        Sample == 'Feroxus'
      }) %>%
      cbind(stats) %>%
      dplyr::rename(Statistic = stats) %>%
      dplyr::select(Statistic, everything())

    matrix.rates.tbl <- RRtbl1 %>%
      kbl() %>%
      kable_classic(position = "left",full_width = F, html_font = "Arial")
    matrix.rates.tbl
  }

  #tab 3
  Duration_enum <- reactive({
    if (input$chooseenum2 == "All") {
      Consented %>%
        dplyr::select(Total, Elig.Time, Arrangement.Time, Consent.Time,
                      Background.Time, C19.Time, HH.Time,
                      PS.Time, SSH.Time, DB.Time, Phone.duration, month.interview, Date.Interview) %>%
        drop_na(Elig.Time)
    } else {
      # Cons <- Consented[Consented$enumerator == input$chooseenum2,]
      Consented %>%
        dplyr::filter(enumerator == input$chooseenum2) %>%
        dplyr::select(Total, Elig.Time, Arrangement.Time, Consent.Time,
                      Background.Time, C19.Time, HH.Time,
                      PS.Time, SSH.Time, DB.Time, Phone.duration, month.interview, Date.Interview) %>%
        drop_na(Elig.Time)
    }
  })

  output$phoneappduration <- renderPlot({
    Duration_long <- Duration_enum() %>%
      mutate(`Year-Month` = format(as.Date(Date.Interview), "%Y-%m")) %>%
      pivot_longer(1:11, names_to = 'module', values_to = 'time') %>%
      dplyr::mutate(module = factor(module, levels = c('Total','Elig.Time','Arrangement.Time','Consent.Time','Background.Time','C19.Time','HH.Time','PS.Time','SSH.Time','DB.Time','Phone.duration')),
                    variable = ifelse(module %in% c('Phone.duration', 'Total'), '1', '2')) %>%
      dplyr::mutate(min = time/60)
    ###########################################################
    ggplot(data = subset(Duration_long, ((module == 'Total' | module == 'Phone.duration') & min < 100))) +
      geom_boxplot(aes(x = module, y = min, fill = `Year-Month`), alpha=0.8) +
      theme_minimal() + xlab('') +ylab('Time (min)') +
      scale_x_discrete(labels = c("Total time\nfor survey", 'Phone call')) +
      scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
      scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02","#A6761D","#66C2A5",
                                   "#666666","#FC8D62",'#540d37','#065535'))+
      labs(caption = 'Four observations with total time over 100 minutes were removed from visualization.',
           fill = 'Month of interview')
  })

  output$moduletiming <- renderPlot({
    # === Interview length
    Duration_enum() %>%
      pivot_longer(1:11, names_to = 'module', values_to = 'time') %>%
      dplyr::mutate(module = factor(module, levels = c('Total','Elig.Time','Arrangement.Time','Consent.Time','Background.Time','C19.Time','HH.Time','PS.Time','SSH.Time','DB.Time','Phone.duration')),
                    variable = ifelse(module %in% c('Phone.duration', 'Total'), '1', '2')) %>%
      dplyr::mutate(min = time/60) %>%
      dplyr::filter(time >=1 & time <1800 & module != 'Total' & module != 'Phone.duration') %>%
      # === Calculate average duration for individual modules
      ggplot() +
      geom_boxplot(aes(x = module, y = min), fill = '#D95F02') + theme_minimal() +  xlab('Module') +
      scale_x_discrete(labels = c('Eligibility','Arrange-\nments','Consent','Back-\nground','COVID19','Household','Parental\nSurvival', 'Sibling Survival\nHistory','Debrief')) +
      scale_y_continuous(name = "Time (min)")+
      labs(caption = 'Fifteen observations with time longer than 30 minutes per module were removed from the visualization.')
  })

  output$avgintsperdayperenum <- renderPlot({
    ## average completed interviews per day by enumerator
    jan <- 21
    feb <- 20
    mar <- 23
    jun <- 3
    jul <- 21
    aug22 <- data.frame(date=seq(as.Date('2022-08-01'), as.Date('2022-08-22'), by ='days')) %>%
      mutate(day = lubridate::wday(date)) %>%
      filter(day %in% c(2,3,4,5,6))
    aug22 <- nrow(aug22)
    apr <- 15
    dec <- 23#pas de jours feries
    nov <- 22-3
    oct <- 21
    sep <- 22
    aug <- 2
    Consented %>%
      dplyr::mutate(dayofweek = wday(Date.Interview),
                    weekday = ifelse(dayofweek %in% c(2,3,4,5,6), 1, 0)) %>%
      dplyr::group_by(month.interview) %>%
      dplyr::summarise(compint = n()) %>%
      dplyr::mutate(compintperenum = ifelse(month.interview == 'Jan-22', compint / jan / length(unique(data[data$month.interview=='Jan-22',]$enumerator)),
                                            ifelse(month.interview == 'Dec-21', compint/dec/length(unique(data[data$month.interview=='Dec-21',]$enumerator)),
                                                   ifelse(month.interview == 'Nov-21', compint/nov/length(unique(data[data$month.interview=='Nov-21',]$enumerator)),
                                                          ifelse(month.interview == 'Oct-21', compint/oct/length(unique(data[data$month.interview=='Oct-21',]$enumerator)),
                                                                 ifelse(month.interview == 'Sep-21', compint/sep/length(unique(data[data$month.interview=='Sep-21',]$enumerator)),
                                                                        ifelse(month.interview =='Aug-21', compint/aug/length(unique(data[data$month.interview=='Aug-21',]$enumerator)),
                                                                               ifelse(month.interview =='Aug-22', compint/aug22/length(unique(data[data$month.interview=='Aug-22',]$enumerator)),
                                                                                      ifelse(month.interview=='Feb-21',compint/feb/length(unique(data[data$month.interview=='Feb-21',]$enumerator)),
                                                                                             ifelse(month.interview =='Mar-22',compint/mar/length(unique(data[data$month.interview=='Mar-22',]$enumerator)),
                                                                                                    ifelse(month.interview =='Apr-22',compint/apr/length(unique(data[data$month.interview=='Apr-22',]$enumerator)),
                                                                                                           ifelse(month.interview == "May-22", compint/may/length(unique(data[data$month.interview=='May-22',]$enumerator)),
                                                                                                                  ifelse(month.interview == 'Jun-22', compint/jun/length(unique(data[data$month.interview=='Jun-22',]$enumerator)),
                                                                                                                         ifelse(month.interview == 'Jul-22', compint/jul/length(unique(data[data$month.interview=='Jul-22',]$enumerator)), NA))))))))))))),
                    month.interview = factor(month.interview, levels = c('Aug-21','Sep-21','Oct-21','Nov-21','Dec-21','Jan-22','Feb-22','Mar-22','Apr-22', 'May-22','Jun-22', 'Jul-22','Aug-22'))) %>%
      ggplot() +
      geom_line(group = 1, aes(x = month.interview, y = compintperenum), size = 2, color = '#D95F02') +
      geom_text(aes(x = month.interview, y = compintperenum, label = round(compintperenum,2), hjust = 1.4, fontface = 'bold')) +
      labs(y = 'Average completed interviews per enumerator per day',
           x = 'Month')
  })

  output$intloc <- renderPlot({
    # === Location from where call was taken
    title <- substitute(paste("Location from which respondents took the consented interview"))
    data.frame(Location = c("Home", "Workplace/University", 'Out and about',"Other", "Refuse"),
               Number = table(Consented$Location_call),
               Proportion = round(prop.table(table(Consented$Location_call))*100,1)) %>%
      dplyr::mutate(pos = cumsum(Proportion.Freq)- 0.5*Proportion.Freq) %>% as.data.frame() %>%

      ggplot() +
      geom_col(aes(x="", y=Proportion.Freq, fill=Location), width=1, color="white") +
      #geom_text(aes(x = '', y = pos,label = paste0(Proportion.Freq,'%'))) +
      coord_polar("y", start=0) +
      theme_void() +
      theme(legend.position="right") +
      scale_fill_brewer(palette="Dark2")+
      labs(title = "Location from which respondents took the\nconsented interview") +
      # geom_text(aes(y = ypos, label = Number.Freq), color = "white", size=4) +
      theme(text = element_text(family = 'Arial'))
  })

  output$phoneowner <- renderPlot({
    data.frame(prop = prop.table(table(Consented$OwnPhone))) %>%
      dplyr::mutate(Outcome = prop.Var1) %>%
      ggplot() +
      geom_bar(aes(x="", y=prop.Freq, fill=Outcome), stat="identity", width=1, color="white") +
      coord_polar("y", start=0) +theme_void() +
      theme(legend.position="right") +
      scale_fill_brewer(palette="Dark2")+
      labs(title = 'Phone ownership')
  })

  #### tab 4
  consbymonth <- reactive({
    if (input$choosemonth == "Overall") {
      Consented
    } else {
      Consented[Consented$month.interview %in% input$choosemonth,]
    }
  })
  databymonth <- reactive({
    if (input$choosemonth == "Overall") {
      data
    } else {
      data[data$month.interview == input$choosemonth,]
    }
  })

  output$compperint <- renderPlot({
    # --- Obtain vector of counts, cumulative tots and dates for number of interviews per day
    #Consentedfilt <- consbymonth()
    Counts <- aggregate(data.frame(count = consbymonth()$Date.Interview),
                        by= list(value = consbymonth()$Date.Interview,
                                 enumerator = consbymonth()$enumerator),length) %>%
      dplyr::group_by(enumerator) %>%
      dplyr::mutate(csum = cumsum(count)) %>% as.data.frame()
    Counts$Gr.Interviewer <- ifelse(Counts$enumerator=='E1'|Counts$enumerator=='E2'|Counts$enumerator=='E3'|
                                      Counts$enumerator=='E4'| Counts$enumerator=='E5'|Counts$enumerator=='E6'|
                                      Counts$enumerator=='E7'|Counts$enumerator=='E8',"Original", "New")
    ggplot(Counts) +
      geom_boxplot(aes(x = enumerator, y = count, fill = Gr.Interviewer)) +
      scale_y_continuous(name = 'Daily Completed interviews') +
      scale_x_discrete() +
      theme(axis.title.y = element_text(color = '#D95F02')) +
      theme_minimal() + xlab('Enumerator')+
      theme(legend.position="none") +
      scale_fill_manual(values= c("#1B9E77", "#D95F02")) +
      labs(title = 'Completed interviews per day per enumerator')
  })

  output$callsperint <- renderPlot({
    # === Call attempts per day per enumerator
    # --- create a counter and isolate the days over which we want to calculate this
    datafilt <- databymonth()
    print(table(datafilt$enumerator))
    datafilt$counter <- 1
    print(table(datafilt$Date.Interview))
    if (input$choosemonth == "Overall") {
      firstdate <- as.Date("2021-09-09"); lastdate <- as.Date("2022-08-22")#datafilt$Date.Interview[nrow(datafilt)])
    }
    else {
      firstdate <- lubridate::my(input$choosemonth)
      lastdate <- lubridate::ceiling_date(my(input$choosemonth),'month') - days(1)
    }
    print(paste('1st date',firstdate))
    print(paste0('last date',lastdate))
    print(input$choosemonth)
    days <- data.frame(date=seq(firstdate, lastdate, by ='days')) %>%
      dplyr::mutate(day = lubridate::wday(date)) %>%
      dplyr::filter(day %in% c(2,3,4,5,6))
    collection.time <- nrow(days)#as.numeric(lastdate- firstdate)

    print(paste('coll time: ',collection.time))

    datafilt$Gr.Interviewer <- if_else(datafilt$enumerator=='E1'|datafilt$enumerator=='E2'|datafilt$enumerator=='E3'|datafilt$enumerator=='E4'| datafilt$enumerator=='E5'|datafilt$enumerator=='E6'|datafilt$enumerator=='E7'|datafilt$enumerator=='E8',"Original", "New")
    print(table(datafilt$enumerator))

    datafilt2 <- datafilt %>%
      dplyr::group_by(enumerator, Date.Interview) %>%
      dplyr::summarize(Sum = sum(counter)/collection.time,
                       totalperday = sum(counter),
                       Gr.Interviewer = Gr.Interviewer)
    print(datafilt2)
    ggplot(data = datafilt2, aes(x=enumerator, y= totalperday, fill=Gr.Interviewer)) +
      geom_boxplot()+
      theme_minimal() +
      theme(legend.position="none") +
      ylab("Average calls per day") + xlab("")+
      scale_fill_manual(values= c("#1B9E77", "#D95F02")) +
      xlab('Enumerator') +
      labs(title = 'Call attempts per day per enumerator')

  })

  output$timeonphoneCATI <- renderPlot({
    #Consented <- consbymonth()
    # === Time on phone to complete CATI interview (consenting respondents)
    d1 <- subset(consbymonth(), Phone.duration<3000)
    d1$Gr.Interviewer <- if_else(d1$enumerator=='E1'|d1$enumerator=='E2'|d1$enumerator=='E3'|d1$enumerator=='E4'| d1$enumerator=='E5'|d1$enumerator=='E6'|d1$enumerator=='E7'|d1$enumerator=='E8',"Original", "New")

    ggplot(data = d1, aes(x = enumerator, y = Phone.duration/60, fill=Gr.Interviewer)) +
      geom_boxplot() +
      theme_minimal() +
      ylab("Time to complete consented interview (minutes)") +
      theme() +
      scale_fill_manual(values= c("#1B9E77", "#D95F02"))+
      theme(legend.position="none") +
      xlab('Enumerator') +
      labs(title = 'Time on phone to complete CATI interview (consenting respondents)')
  })

  output$catibyenum <- renderPlot({
    # === CATI Outcomes by Enumerator, excluding dysfunctional numbers
    dat.wide %>%
      dplyr::filter(Outcome.FINAL!= 'NNU') %>%
      dplyr::group_by(enum, Outcome.FINAL) %>%
      dplyr::summarize(n = n()) %>%
      dplyr::mutate(Gr.Interviewer = if_else(enum=='E1'|enum=='E2'|enum=='E3'|enum=='E4'| enum=='E5'|
                                               enum=='E6'|enum=='E7'|enum=='E8',"Original", "New") ) %>%
      ggplot() +
      geom_bar(aes(x = enum, y = n, fill = Outcome.FINAL), position = 'stack',stat = 'identity', color= 'black') +
      scale_x_discrete() +
      geom_text(aes(x = enum, y = n, group = Outcome.FINAL, label = n),position = position_stack(vjust = .5)) +
      theme(axis.title.y = element_text(color = '#D95F02')) +
      theme_minimal() + ylab('Call attempt outcomes per Enumerator') +
      scale_fill_brewer(palette = 'Dark2') +labs(fill = 'Outcome') +
      xlab('Enumerator') +
      labs(title = 'CATI Outcomes by Enumerator, excluding dysfunctional numbers')
  })

  output$timeonphone <- renderPlot({
    # === Daily time on CATI application and on the phone, per enumerator
    dattime <- databymonth() %>%
      #dplyr::filter(Date.Interview >= '2021-09-09') %>%
      dplyr::group_by(enumerator, Date.Interview) %>%
      dplyr::summarise(timeperday = sum(Phone.duration)/60/60,
                       CATItimeperday = sum(Total)/60/60) %>%
      pivot_longer(timeperday:CATItimeperday, names_to = "timeonwhat",values_to = 'hours')
    print(dattime)

    ggplot(dattime) + geom_boxplot(aes(x = enumerator, y = hours, fill = timeonwhat)) +
      theme_minimal() +
      xlab("") + ylab("Time per day (hours)") +
      theme(axis.text.x = element_text(angle = 0)) +
      scale_x_discrete() +
      scale_fill_brewer(palette = 'Dark2',labels = c('Time on CATI\napplication','Time on phone')) +
      labs(fill = '',
           #caption = 'This plot is filtered to September 9, 2021 and after.',
           title = 'Daily time on CATI application and on the phone, per enumerator')
  })

  # tab 5
  output$sociodemotbl <- function(){
    sociodemofunc <- dget('Sociodemotbl_func.R')
    #print('sociodemo func function called')
    sociodemofunc(Consented, Consented[Consented$Resp.Region==1,], Consented[Consented$Resp.Region==2,])
  }

  output$agesexdist <- renderPlot({
    agesexbymo <- dget('AgeSexScript.R')
    if (input$bymonth == "Overall") {
      agesexbymo(Consented, Consented[Consented$Resp.Region==2,], Consented[Consented$Resp.Region==1,],bymonth = 'Overall')
    }
    else {
      agesexbymo(Consented, Consented[Consented$Resp.Region==2,], Consented[Consented$Resp.Region==1,],bymonth = input$bymonth)
    }
  })

  # tab 6
  output$CDRandparentdeaths <- renderPlot({
    cdrfunc <- dget('CDRParentaldeaths_func.R')
    cdrfunc(Consented, Consented[Consented$Resp.Region==1,], Consented[Consented$Resp.Region==2,], tableyes = F)
  })

  output$HHsize <- renderPlot({
    ggplot(Consented, aes(x=HHsize)) +
      geom_histogram(aes(y=..density..), colour="black", fill="#D95F02",binwidth=1)+
      xlab("Household size")+ ylab("Density")+
      theme_minimal() +
      labs(title = 'Household size')
  })

  output$decparentbyprov <- renderPlot({
    # === Proportion of total sample, and regions with a mother and father deceassed
    matrix <- matrix(NA, 2, 3)
    matrix[,1] <- c(round((length(which(Consented$M.Dead==2)) / nrow(Consented))*100,1),
                    round((length(which(Consented$F.Dead==2)) / nrow(Consented))*100,1))
    matrix[,2] <- c(round((length(which(Consented[Consented$Resp.Region==1,]$M.Dead==2)) / nrow(Consented[Consented$Resp.Region==1,]))*100,1),
                    round((length(which(Consented[Consented$Resp.Region==1,]$F.Dead==2)) / nrow(Consented[Consented$Resp.Region==1,]))*100,1))
    matrix[,3] <- c(round((length(which(Consented[Consented$Resp.Region==2,]$M.Dead==2)) / nrow(Consented[Consented$Resp.Region==2,]))*100,1),
                    round((length(which(Consented[Consented$Resp.Region==2,]$F.Dead==2)) / nrow(Consented[Consented$Resp.Region==2,]))*100,1))
    colnames(matrix) <- c("Overall", "Kinshasa", "Nord Kivu")
    rownames(matrix) <- c("Mothers", "Fathers")

    ggplot(reshape2::melt(matrix), aes(x=Var2, y=value, group=Var1, fill=Var1)) +
      geom_bar(stat="identity", position="dodge", color= 'black') +#, alpha = 0.4
      xlab("Province") + ylab("Proportion with deceased parent") +
      scale_fill_brewer(palette = 'Dark2')+theme(legend.title = element_blank())+
      theme() + coord_flip() +
      labs(title = 'Proportion of respondents with deceased parent by province')
  })

  output$survparentsbyage <- renderPlot({
    cbind(Consented %>%
            dplyr::group_by(Resp.Age.pyr) %>%
            dplyr::summarize(Resp.Age.pyr_n = n()) %>%
            dplyr::select(-Resp.Age.pyr),
          Consented %>%
            dplyr::select(Resp.Age.pyr, Resp.Age, M.Dead) %>%
            dplyr::filter(M.Dead ==1) %>%# filtering to those with surviving mother
            dplyr::group_by(Resp.Age.pyr) %>%
            dplyr::summarize(n.M.Alive = n()) ,
          Consented %>%
            dplyr::select(Resp.Age.pyr, Resp.Age, F.Dead) %>%
            dplyr::rename(Resp.Age.pyr2 = Resp.Age.pyr) %>%
            dplyr::filter(F.Dead ==1) %>%# filtering to those with surviving father
            dplyr::group_by(Resp.Age.pyr2) %>%
            dplyr::summarize(n.F.Alive = n())
    ) %>%
      dplyr::select(-Resp.Age.pyr2) %>%
      dplyr::mutate(perc.M.alive = n.M.Alive / Resp.Age.pyr_n,
                    perc.F.alive = n.F.Alive / Resp.Age.pyr_n) %>%
      ggplot() +
      geom_line(aes(x = Resp.Age.pyr, y = perc.M.alive, color = 'Mother'), group = 1,size = 1) +
      geom_line(aes(x = Resp.Age.pyr, y = perc.F.alive, color = 'Father'), group = 1, size = 1) +
      theme_minimal() +
      scale_x_discrete(name ='Respondent Age category', labels = c("15-19", "20-29",'30-39','40-49','50-59','60-64')) +
      scale_y_continuous(name ='Proportion with alive parent') +
      scale_colour_manual("", breaks = c("Mother", "Father"),
                          values = c("#1B9E77", "#D95F02")) +
      labs(title = 'Proportion of surviving parents by age group of respondent')
  })

  output$agesofsurvparentsbyage <- renderPlot({
    Age.M <- Consented$Resp.Age; Age.F <- Consented$Resp.Age
    Father <- Consented$F.alive.age; Mother <- Consented$M.alive.age

    Age.M[which(is.na(Mother))] <- NA; Age.M<-Age.M[!is.na(Age.M)] ; Mother<-Mother[!is.na(Mother)]
    Age.F[which(is.na(Father))] <- NA; Age.F<-Age.F[!is.na(Age.F)] ; Father<-Father[!is.na(Father)]

    scatterages <- data.frame(Age = c(Age.M,Age.F),
                              Parent.Age = c(Mother, Father),
                              Parent = c(rep("Mother",length(Mother)), rep("Father", length(Father)))) %>%
      dplyr::filter(Parent.Age != -99)

    scatterages_re <- reactive({
      if (input$chooseparent == 'Both') {
        scatterages
      }
      else {subset(scatterages, Parent == input$chooseparent)}
    })
    ggplot(data= scatterages_re(),aes(x = Age, y = Parent.Age)) +
      geom_point(aes(color = Parent))+
      scale_color_brewer(palette = "Dark2")+
      xlab("Respondent Age") + ylab("Parent Age") + theme_minimal() +
      geom_abline(aes(intercept = 15, slope = 1)) +
      scale_y_continuous(limits = c(25,110), breaks = seq(25,110, by = 5))+
      labs(title = 'Ages of surviving parents by age of respondent') +
      scale_x_continuous(breaks = seq(15,70, by = 5))

  })

  # tab 7
  output$numsibdeaths <- function(){
    cdrfunc <- dget('CDRParentaldeaths_func.R')
    Sibling.parent <- cdrfunc(Consented, Consented[Consented$Resp.Region==1,], Consented[Consented$Resp.Region==2,], tableyes = T)
    Sibling.parent[c(1,2),c(1,2)] %>%
      kbl(caption = 'Total number of sibling deaths') %>% kable_classic(full_width = F, html_font = "Arial")
  }

  output$sibsbyprov <- renderPlot({
    # === Total number of brothers, sisters and total siblings by respondent's province
    Province.ssh <- data.frame(Bro.Sis = Consented$Brothers + Consented$Sisters,
                               Brother = Consented$Brothers,
                               Sister = Consented$Sisters,
                               Province = Consented$Resp.Region) %>%
      dplyr::group_by(Province) %>%
      dplyr::summarize(Siblings = sum(Bro.Sis),
                       Sisters = sum(Sister),
                       Brothers = sum(Brother)) %>% as.data.frame()
    Province.ssh <-     reshape2::melt(Province.ssh)[-c(1:2),]
    Province.ssh$Province <- rep(c("Kinshasa", "Nord Kivu"), 3)
    ggplot(data = Province.ssh, aes(x=variable, y=value, group=Province, fill = Province)) +
      geom_bar(stat="identity", position = "dodge", color='black') +
      theme_minimal() + xlab("Siblings") + ylab("Number of siblings reported by province") +
      theme(legend.position = "bottom") +
      coord_flip()+ scale_fill_brewer(palette = 'Dark2')
  })

  output$sibsbysex <- renderPlot({
    # === Total number of brothers, sisters and total siblings by repsondent's sex
    Sex.ssh <- data.frame(Bro.Sis = Consented$Brothers + Consented$Sisters,
                          Brother = Consented$Brothers,
                          Sister = Consented$Sisters,
                          Sex = Consented$Resp.Sex) %>% dplyr::group_by(Sex) %>%
      dplyr::summarize(Siblings = sum(Bro.Sis),
                       Sisters = sum(Sister),
                       Brothers = sum(Brother)) %>%
      as.data.frame() %>% reshape2::melt()
    #Sex.ssh <- melt(as.data.frame(Sex.ssh))

    ggplot(Sex.ssh, aes(x=variable, y=value, group=Sex, fill = Sex)) +
      geom_bar(stat="identity", position = "dodge", color= 'black') +
      theme_minimal() + xlab("Siblings") + ylab("Number of siblings reported by sex of respondent") +
      theme(legend.position = "bottom") +
      coord_flip() + scale_fill_brewer(palette = 'Dark2')
  })

  output$decsibsbyrespage <- renderPlot({
    #Sis.D <-
    sis.bro.D <- rbind(
      data.frame(Dead = Consented$Sis1.Dead,
                 Number = Consented$Sisters,
                 Age = Consented$agegp, Sibling="Sister")%>%
        dplyr::group_by(Age) %>%
        dplyr::summarise(D=sum(Dead, na.rm=T),
                         No = sum(Number, na.rm=T)) %>%
        dplyr::mutate(Prop = round((D/No)*100,2))%>%
        dplyr::mutate(Sibling = "Sister"),
      #Bro.D <-
      data.frame(Dead = Consented$Bro1.Dead,
                 Number = Consented$Brothers,
                 Age = Consented$agegp, Sibling="Brother") %>%
        dplyr::group_by(Age) %>%
        dplyr::summarise(D=sum(Dead, na.rm=T),
                         No = sum(Number, na.rm=T)) %>%
        dplyr::mutate(Prop = round((D/No)*100,2))%>%
        dplyr::mutate(Sibling = "Brother")
    )

    #sis.bro.D <- rbind(Bro.D, Sis.D)

    ggplot() +
      geom_line(data = subset(sis.bro.D, Sibling=="Sister"), aes(x = Age, y = Prop), group = 1,size = 1, color="#1B9E77") +
      geom_line(data = subset(sis.bro.D, Sibling=="Brother"), aes(x = Age, y = Prop), group = 1,size = 1, color="#D95F02") +
      theme_minimal() +
      scale_x_discrete(name ='Respondent Age category', labels = c("15-29",'30-39','40-49','50-59','60-64')) +
      scale_y_continuous(name ='Proportion with deceased sibling') +
      scale_colour_manual("", breaks = c("Mother", "Father"),
                          values = c("#1B9E77", "#D95F02"))+
      labs(title = 'Proportion of deceased siblings by respondent age')
  })

  output$distribbyrespage <- renderPlot({
    Consented$agegp <- cut(Consented$Resp.Age,
                           breaks=c(17, 30, 40, 50, 60, 70),
                           labels=c("17-29","30-39", "40-49", "50-59", "60+"))
    #Bro <-
    rbind(data.frame(Number = subset(Consented$Brothers, Consented$Brothers<30),Age = subset(Consented$agegp, Consented$Brothers<30), Sibling="Brother"),
          #Sis <-
          data.frame(Number = subset(Consented$Sisters,Consented$Sisters<30), Age = subset(Consented$agegp, Consented$Sisters<30), Sibling="Sister")) %>%
      #Sibling <- rbind(Bro, Sis)

      ggplot() +
      geom_boxplot(aes(x = Age, y = Number, fill=Sibling)) +
      theme_minimal() +
      xlab('Age') +
      ylab('Siblings') +
      scale_x_discrete(labels = c("17-29","30-39", "40-49", "50-59", "60+"))+
      scale_fill_brewer(palette = 'Dark2')+
      labs(title = 'Distribution of siblings by age of respondent')

  })

  #tab 8
  deathfunc <- dget('Deaths_func.R')
  listtables <- deathfunc(Consented)
  matrix.D.set.df <- listtables[[1]]
  matrixtots <- listtables[[2]]
  matrix2019.df <- listtables[[3]]
  matrix.LA.df <- listtables[[4]]
  matrix.Burial.df <- listtables[[5]]

  output$causeofdeath <- function(){
    matrix2019.df <- cbind(matrixtots[,1], matrix2019.df[,1:3],
                           matrixtots[,2], matrix2019.df[,4:6],
                           matrixtots[,3], matrix2019.df[,7:9])
    colnames(matrix2019.df) <- rep(c("N", "%Maternal", "%External", "%COVID"),3)

    matrix2019.df <- matrix2019.df %>%
      kbl() %>%
      kable_classic(full_width = F, html_font = "Arial")%>%
      column_spec (c(1,5,9),border_left = F, border_right = T) %>%
      footnote(general = "The presumed causes of death are those reported by the respondent. Maternal deaths are defined as deaths that occured whilst the deceased was pregnant, during childbirth, or 2 months post-pregnancy. COVID deaths are defined as those that are declared to be very likely or somewhat likely COVID related. External deaths are defined as those that were caused by an accident or by an act of violence")
    add_header_above(matrix2019.df, c(" " = 1,"2019 " = 4, "2020" = 4, "2021" = 4))
  }

  output$placeofdeath <- function(){
    matrix.D.set.df <- cbind(matrixtots[,1], matrix.D.set.df[,1:4],
                             matrixtots[,2], matrix.D.set.df[,5:8],
                             matrixtots[,3], matrix.D.set.df[,9:12])
    colnames(matrix.D.set.df) <- rep(c("N", "%Health Facility", "Home", "%Other", "%DK"),3)

    matrix.D.set.df <- matrix.D.set.df %>%
      kbl() %>%
      kable_classic(full_width = F, html_font = "Arial")%>%
      column_spec (c(1,6,11),border_left = F, border_right = T)

    add_header_above(matrix.D.set.df, c("  " = 1, "2019 " = 5, "2020" = 5, "2021" = 5))
  }

  output$placeofburial <- function(){
    matrix.Burial.df <- cbind(matrixtots[,1], matrix.Burial.df[,1:4],
                              matrixtots[,2], matrix.Burial.df[,5:8],
                              matrixtots[,3], matrix.Burial.df[,9:12])
    colnames(matrix.Burial.df) <- rep(c("N", "%Cemetery", "%Family Plot", "%Not buried", "%DK"),3)

    matrix.Burial.df <- matrix.Burial.df %>%
      kbl() %>%
      kable_classic(full_width = F, html_font = "Arial")%>%
      column_spec (c(1,6,11),border_left = F, border_right = T)
    add_header_above(matrix.Burial.df, c("  " = 1, "2019 " = 5, "2020" = 5, "2021" = 5))
  }

  output$registereddeath <- function(){
    matrix.LA.df <- cbind(matrixtots[,1], matrix.LA.df[,1:3],
                          matrixtots[,2], matrix.LA.df[,4:6],
                          matrixtots[,3], matrix.LA.df[,7:9])
    colnames(matrix.LA.df) <- rep(c("N", "%Yes", "%No","%DK"),3)

    matrix.LA.df <- matrix.LA.df %>%
      kbl() %>%
      kable_classic(full_width = F, html_font = "Arial")%>%
      column_spec (c(1,5,9),border_left = F, border_right = T)

    add_header_above(matrix.LA.df, c(" " = 1,"2019 " = 4, "2020" = 4, "2021" = 4))
  }
}


# Run the application 
shinyApp(ui = ui, server = server)

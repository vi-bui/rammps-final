# MALAWI APP

# Libraries
# Load function to install list of packages
#ldpkg <- dget("ldpkg.R")


# Load/install packages 
pkgs <- c( 'plyr','dplyr','lubridate','magrittr', 'readr','shiny',
          'kableExtra','knitr','bslib','shinydashboard','date','tidyselect','httr',#'jsonlite','plyr',
          'gridExtra','reshape','epitools','ggpubr','tidyr', 'janitor', 'haven')#'magick','DescTools', 'extrafont'
# ldpkg(pkgs)        library(DescTools)
lapply(pkgs, require, character.only = TRUE)

require(epitools)
require(ggpubr)
require(magick)
require(lubridate)
require(reshape)
require(date)
require(kableExtra)
# require(extrafont)
require(shinydashboard)
require(dplyr)
require(janitor)
require(haven)
# loadfonts()

theme_set(theme_minimal())

#pull_data <- dget('pull_data_func.R')
#mwdata_raw <- pull_data('malawirammps', 'ipormw', 'kelly.mccain@lshtm.ac.uk', 'rammpS!CTOsurvey')
#mwdata_raw <- read.csv('MalawiRAMMPS_WIDE.csv')

#getmwdata <- dget('Malawi_CleaningScript.R')
#listdfsMW<- getmwdata(mwdata_raw)

# Consentedmw <- listdfsMW[[1]] # Consented
# data_clean3 <- listdfsMW[[2]] # data_essential3
# dat.widemw <- listdfsMW[[3]] #dat.widemw
Consentedmw <- read.csv('ConsentedMW.csv')#,show_col_types = FALSE) %>% as.data.frame()
data_clean3 <- read.csv('DatLongMW_sml.csv') %>% #,show_col_types = FALSE)%>% as.data.frame()
  mutate(call_num_grp = ifelse(call_num > 5, '5+',as.character(call_num)))
dat.widemw <- read.csv('DatWide.csv')#,show_col_types = FALSE)%>% as.data.frame()

Northern <- Consentedmw[which(Consentedmw$Resp.Region=='Northern'),]
Central <- Consentedmw[which(Consentedmw$Resp.Region=='Central'),]
Southern <- Consentedmw[which(Consentedmw$Resp.Region=='Southern'),]

# === Table of progress to quota
# --- Create a table of Age vs sex by region and then proportions
block1 <- Consentedmw %>%
  dplyr::filter(Date.Interview < "2022-05-15")
quota.count1 <- table(block1$Resp.Age.grp, 
                      block1$Resp.Sex,
                      block1$Resp.Region,
                      block1$RuralUrban) 
quota.prop1 <- round(prop.table(table(block1$Resp.Age.grp, 
                                      block1$Resp.Sex,
                                      block1$Resp.Region,
                                      block1$RuralUrban
))*100,1)
block2 <- Consentedmw %>%
  dplyr::filter(Date.Interview >= "2022-05-25" & Date.Interview < '2022-09-19')
quota.count2 <- table(block2$Resp.Age.grp,
                      block2$Resp.Sex,
                      block2$Resp.Region,
                      block2$RuralUrban)
quota.prop2 <- round(prop.table(table(block2$Resp.Age.grp,
                                      block2$Resp.Sex,
                                      block2$Resp.Region,
                                      block2$RuralUrban
))*100,1)
block3 <- Consentedmw %>%
  dplyr::filter(Date.Interview >= '2022-09-19')
quota.count3 <- table(block3$Resp.Age.grp,
                      block3$Resp.Sex,
                      block3$Resp.Region,
                      block3$RuralUrban)
quota.prop3 <- round(prop.table(table(block3$Resp.Age.grp,
                                      block3$Resp.Sex,
                                      block3$Resp.Region,
                                      block3$RuralUrban
))*100,1)

# --- Wrap this into a matrix for counts of completed interviews


# Quotas
quotascount1 <- (as.matrix(cbind(quota.count1[,,2,2], quota.count1[,,2,1],quota.count1[,,1,2],quota.count1[,,1,1], quota.count1[,,3,2],quota.count1[,,3,1])))
quotascount1 <- rbind(quotascount1, colSums(quotascount1))
quotascount1 <- cbind(quotascount1, rowSums(quotascount1)) 
#quotascount <- quotascount[,-c(3,4,7,8,11,12)] 
rownames(quotascount1) <- c("18-49", "50-64", "Total")
colnames(quotascount1) <- c("Northern Urban Women", "Northern Urban Men", "Northern Rural Women", "Northern Rural Men",
                            "Central Urban Women", "Central Urban Men","Central Rural Women", "Central Rural Men",
                            "Southern Urban Women", "Southern Urban Men","Southern Rural Women", "Southern Rural Men", "Total")
quotascount.tab1 <-quotascount1

colnames(quotascount.tab1) <- c("Urban Women", "Urban Men", "Rural Women", "Rural Men","Urban Women", "Urban Men", "Rural Women", "Rural Men",
                                "Urban Women", "Urban Men", "Rural Women", "Rural Men","Total")
quotascount2 <- (as.matrix(cbind(quota.count2[,,2,2], quota.count2[,,2,1],quota.count2[,,1,2],quota.count2[,,1,1], quota.count2[,,3,2],quota.count2[,,3,1])))
quotascount2 <- rbind(quotascount2, colSums(quotascount2))
quotascount2 <- cbind(quotascount2, rowSums(quotascount2))
#quotascount <- quotascount[,-c(3,4,7,8,11,12)]
rownames(quotascount2) <- c("18-49", "50-64", "Total")
colnames(quotascount2) <- c("Northern Urban Women", "Northern Urban Men", "Northern Rural Women", "Northern Rural Men",
                            "Central Urban Women", "Central Urban Men","Central Rural Women", "Central Rural Men",
                            "Southern Urban Women", "Southern Urban Men","Southern Rural Women", "Southern Rural Men", "Total")
quotascount.tab2 <-quotascount2

colnames(quotascount.tab2) <- c("Urban Women", "Urban Men", "Rural Women", "Rural Men","Urban Women", "Urban Men", "Rural Women", "Rural Men",
                                "Urban Women", "Urban Men", "Rural Women", "Rural Men","Total")
quotascount3 <- (as.matrix(cbind(quota.count3[,,2,2], quota.count3[,,2,1],quota.count3[,,1,2],quota.count3[,,1,1], quota.count3[,,3,2],quota.count3[,,3,1])))
quotascount3 <- rbind(quotascount3, colSums(quotascount3))
quotascount3 <- cbind(quotascount3, rowSums(quotascount3))
#quotascount <- quotascount[,-c(3,4,7,8,11,13)]
rownames(quotascount3) <- c("18-49", "50-64", "Total")
colnames(quotascount3) <- c("Northern Urban Women", "Northern Urban Men", "Northern Rural Women", "Northern Rural Men",
                            "Central Urban Women", "Central Urban Men","Central Rural Women", "Central Rural Men",
                            "Southern Urban Women", "Southern Urban Men","Southern Rural Women", "Southern Rural Men", "Total")
quotascount.tab3 <-quotascount3

colnames(quotascount.tab3) <- c("Urban Women", "Urban Men", "Rural Women", "Rural Men","Urban Women", "Urban Men", "Rural Women", "Rural Men",
                                "Urban Women", "Urban Men", "Rural Women", "Rural Men","Total")
# --- matrix3 = table of % of quota reached
quotasperc1 <- as.data.frame(quotascount1) %>%
  dplyr::mutate(quotaNmenU = c(33,6,(33+6)),
                quotaNmenR = c(161,31,(161+31)),
                quotaNwomenU = c(66,6,(66+6)),
                quotaNwomenR = c(322,31,(322+31)),
                quotaCmenU = c(111,18,(111+18)),
                quotaCmenR = c(542,90,(542+90)),
                quotaCwomenU =c(222,18,(222+18)),
                quotaCwomenR = c(1083,90,(1083+90)),
                quotaSmenU = c(111,18,(111+18)),
                quotaSmenR = c(540,90,(540+90)),
                quotaSwomenU =c(222,18,(222+18)),
                quotaSwomenR = c(1081,90,(1081+90)),
                percNmenU = round(`Northern Urban Men`/quotaNmenU*100,2), 
                percNmenR = round(`Northern Rural Men`/quotaNmenR*100,2), 
                percNwomenU = round(`Northern Urban Women`/quotaNwomenU*100,2), 
                percNwomenR = round(`Northern Rural Women`/quotaNwomenR*100,2),
                percCmenU = round(`Central Urban Men`/quotaCmenU*100,2), 
                percCmenR = round(`Central Rural Men`/quotaCmenR*100,2), 
                percCwomenU = round(`Central Urban Women`/quotaCwomenU*100,2), 
                percCwomenR = round(`Central Rural Women`/quotaCwomenR*100,2),
                percSmenU = round(`Southern Urban Men`/quotaSmenU*100,2), 
                percSmenR = round(`Southern Rural Men`/quotaSmenR*100,2), 
                percSwomenU = round(`Southern Urban Women`/quotaSwomenU*100,2), 
                percSwomenR = round(`Southern Rural Women`/quotaSwomenR*100,2)) %>%
  dplyr::select(c(percNwomenU, percNmenU, percNwomenR, percNmenR, 
                  percCwomenU, percCmenU, percCwomenR, percCmenR,
                  percSwomenU, percSmenU, percSwomenR, percSmenR))
colnames(quotasperc1) <- c("Urban Women", "Urban Men", "Rural Women", "Rural Men","Urban Women", "Urban Men", "Rural Women", "Rural Men",
                           "Urban Women", "Urban Men", "Rural Women", "Rural Men")
quotasperc1 <- as.data.frame(quotasperc1)

quotasperc2 <- as.data.frame(quotascount2) %>%
  dplyr::mutate(quotaNmenU = c(33,6,(33+6)),
                quotaNmenR = c(161,31,(161+31)),
                quotaNwomenU = c(66,6,(66+6)),
                quotaNwomenR = c(322,31,(322+31)),
                quotaCmenU = c(111,18,(111+18)),
                quotaCmenR = c(542,90,(542+90)),
                quotaCwomenU =c(222,18,(222+18)),
                quotaCwomenR = c(1083,90,(1083+90)),
                quotaSmenU = c(111,18,(111+18)),
                quotaSmenR = c(540,90,(540+90)),
                quotaSwomenU =c(222,18,(222+18)),
                quotaSwomenR = c(1081,90,(1081+90)),
                percNmenU = round(`Northern Urban Men`/quotaNmenU*100,2),
                percNmenR = round(`Northern Rural Men`/quotaNmenR*100,2),
                percNwomenU = round(`Northern Urban Women`/quotaNwomenU*100,2),
                percNwomenR = round(`Northern Rural Women`/quotaNwomenR*100,2),
                percCmenU = round(`Central Urban Men`/quotaCmenU*100,2),
                percCmenR = round(`Central Rural Men`/quotaCmenR*100,2),
                percCwomenU = round(`Central Urban Women`/quotaCwomenU*100,2),
                percCwomenR = round(`Central Rural Women`/quotaCwomenR*100,2),
                percSmenU = round(`Southern Urban Men`/quotaSmenU*100,2),
                percSmenR = round(`Southern Rural Men`/quotaSmenR*100,2),
                percSwomenU = round(`Southern Urban Women`/quotaSwomenU*100,2),
                percSwomenR = round(`Southern Rural Women`/quotaSwomenR*100,2)) %>%
  dplyr::select(c(percNwomenU, percNmenU, percNwomenR, percNmenR,
                  percCwomenU, percCmenU, percCwomenR, percCmenR,
                  percSwomenU, percSmenU, percSwomenR, percSmenR))
colnames(quotasperc2) <- c("Urban Women", "Urban Men", "Rural Women", "Rural Men","Urban Women", "Urban Men", "Rural Women", "Rural Men",
                           "Urban Women", "Urban Men", "Rural Women", "Rural Men")
quotasperc2 <- as.data.frame(quotasperc2)

quotasperc3 <- as.data.frame(quotascount3) %>%
  dplyr::mutate(quotaNmenU = c(33,6,(33+6)),
                quotaNmenR = c(161,31,(161+31)),
                quotaNwomenU = c(66,6,(66+6)),
                quotaNwomenR = c(322,31,(322+31)),
                quotaCmenU = c(111,18,(111+18)),
                quotaCmenR = c(542,90,(542+90)),
                quotaCwomenU =c(222,18,(222+18)),
                quotaCwomenR = c(1083,90,(1083+90)),
                quotaSmenU = c(111,18,(111+18)),
                quotaSmenR = c(540,90,(540+90)),
                quotaSwomenU =c(222,18,(222+18)),
                quotaSwomenR = c(1081,90,(1081+90)),
                percNmenU = round(`Northern Urban Men`/quotaNmenU*100,2),
                percNmenR = round(`Northern Rural Men`/quotaNmenR*100,2),
                percNwomenU = round(`Northern Urban Women`/quotaNwomenU*100,2),
                percNwomenR = round(`Northern Rural Women`/quotaNwomenR*100,2),
                percCmenU = round(`Central Urban Men`/quotaCmenU*100,2),
                percCmenR = round(`Central Rural Men`/quotaCmenR*100,2),
                percCwomenU = round(`Central Urban Women`/quotaCwomenU*100,2),
                percCwomenR = round(`Central Rural Women`/quotaCwomenR*100,2),
                percSmenU = round(`Southern Urban Men`/quotaSmenU*100,2),
                percSmenR = round(`Southern Rural Men`/quotaSmenR*100,2),
                percSwomenU = round(`Southern Urban Women`/quotaSwomenU*100,2),
                percSwomenR = round(`Southern Rural Women`/quotaSwomenR*100,2)) %>%
  dplyr::select(c(percNwomenU, percNmenU, percNwomenR, percNmenR,
                  percCwomenU, percCmenU, percCwomenR, percCmenR,
                  percSwomenU, percSmenU, percSwomenR, percSmenR))
colnames(quotasperc3) <- c("Urban Women", "Urban Men", "Rural Women", "Rural Men","Urban Women", "Urban Men", "Rural Women", "Rural Men",
                           "Urban Women", "Urban Men", "Rural Women", "Rural Men")
quotasperc3 <- as.data.frame(quotasperc3)
##############################################################
# For OutcomebyOrder and Compbycall
outcomes <- dat.widemw %>%
  mutate(call_num_grp = ifelse(call_num > 5, '5+',as.character(call_num))) %>%
  dplyr::group_by(call_num_grp, Outcome.FINAL) %>%
  dplyr::summarize(n = n()) %>%
  dplyr::ungroup() %>%
  dplyr::filter(Outcome.FINAL == 'COMP') %>%
  dplyr::mutate(totalcomp = sum(n),
                totalnums = nrow(dat.widemw),
                perc = round(n/totalnums*100,2),
                perccompletedpercallnum=round(n/totalcomp*100,2)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(perccum = cumsum(perc))
outcomes$Calls <- c(table(data_clean3$call_num_grp))

### Get list of months for drop down list
Consentedmw$months <- ordered(Consentedmw$month.interview, c('Aug-21', 'Sep-21','Oct-21','Nov-21',
                                                         'Dec-21','Jan-22', 'Feb-22','Mar-22',
                                                         'Apr-22','May-22','Jun-22','Jul-22',
                                                         'Aug-22', 'Sep-22','Oct-22','Nov-22',
                                                         'Dec-22','Jan-23','Feb-23','Mar-23',
                                                         'Apr-23','May-23','Jun-23'))
months <- Consentedmw %>% dplyr::group_by(months) %>% dplyr::summarize()
list_mos <- unique(months$months) %>% as.character
############################################################################################################################

library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "united"),
  tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: '#1B9E77'}")),
  # Application title
  titlePanel(div(img(src="RAMMPS-FinalLogoHighRes.png", height = 70, width = 150),"MALAWI RESULTS")),
  tabsetPanel(
    # tab 1
    tabPanel("CATI Interviews", 
             fluidRow(
               column(1,
                      selectInput('chooseenum', h5("Choose enumerator:"),
                                  choices = c('All',as.character(ordered(Consentedmw$enumerator, 
                                                                         levels = c('E1','E2','E3','E4','E5','E6','E7',
                                                                                    'E8','E9','E10','E11','E12','E13',
                                                                                    'E14','E15', 'E16','E17','E18','E19','E20'))) %>% sort()))
               ),
               column(5,
                      plotOutput("CATIdayandcum")
               ),
               column(2, radioButtons('choosequota', h5('Choose quota statistic:'),
                                      choices = c('T1: Progress (n) to quota','T1: Percent of quota','T2: Progress (n) to quota','T2: Percent of quota',
                                                  'T3: Progress (n) to quota','T3: Percent of quota'),
                                      selected = 'T3: Progress (n) to quota')),
               column(1, radioButtons('chooseregion',h5('Choose region to filter table:'),
                                      choices = c('Northern', 'Central','Southern'),
                                      selected = 'Northern')),
               column(3,
                      conditionalPanel(
                        condition = "input.choosequota == 'T1: Progress (n) to quota'", tableOutput('CATIquota1')
                      ),
                      conditionalPanel(
                        condition = "input.choosequota == 'T1: Percent of quota'", tableOutput('percCATIquota1')
                      ),
                      conditionalPanel(
                        condition = "input.choosequota == 'T2: Progress (n) to quota'", tableOutput('CATIquota2')
                      ),
                      conditionalPanel(
                        condition = "input.choosequota == 'T2: Percent of quota'", tableOutput('percCATIquota2')
                      ),
                      conditionalPanel(
                        condition = "input.choosequota == 'T3: Progress (n) to quota'", tableOutput('CATIquota3')
                      ),
                      conditionalPanel(
                        condition = "input.choosequota == 'T3: Percent of quota'", tableOutput('percCATIquota3')
                      )
               )
             ),
             fluidRow(
               column(2, 
                      sliderInput("slider", label = h5("Date Range:"), min = min(ymd(Consentedmw$Date.Interview)), 
                                  max = max(ymd(Consentedmw$Date.Interview)), value = c(min(ymd(Consentedmw$Date.Interview)), max(ymd(Consentedmw$Date.Interview))))
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
                                   selected = 'CATI Outcomes')
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
                                  choices = c('Overall',list_mos))#'Jan-22', 'Feb-22','Mar-22','Apr-22','May-22','Jun-22','Jul-22','Aug-22','Sep-22','Oct-22','Nov-22','Dec-22'
               ),
               column(7,
                      tableOutput('RRtable')),
               column(3, 
                      verbatimTextOutput('RRnotes'))
             )),
    # tab 3
    tabPanel("Interview Attributes",
             fluidRow(
               column(1,
                      selectInput('chooseenum2', h5("Choose enumerator:"),
                                  choices = c('All',as.character(ordered(Consentedmw$enumerator,
                                                                         levels = c('E1','E2','E3','E4','E5','E6','E7',
                                                                                    'E8','E9','E10','E11','E12','E13',
                                                                                    'E14','E15'))) %>% sort()))
               ),
               column(5,
                      plotOutput('phoneappduration',height = "400px")
               ),
               column(6,
                      plotOutput('avgintsperdayperenum',height = "400px")
               )
             ),
             fluidRow(
               column(6,
                      plotOutput('intloc', height = '400px')
               ),
               # column(1,
               #        radioButtons('camembert', h5('Choose plot:'),
               #                     choices = c('Interview location','Phone ownership'),
               #                     selected = 'Interview location')
               # ),
               # column(5,
               #        conditionalPanel(
               #            condition = "input.camembert == 'Interview location'", plotOutput('intloc',height = "400px")
               #        ),
               #        conditionalPanel(
               #            condition = "input.camembert == 'Phone ownership'", plotOutput('phoneowner',height = "400px"))
               # ),
               column(6,
                      plotOutput('phoneowner',height ='400px'))
             )
    ),
    # tab 4
    tabPanel('Enumerator Statistics',
             fluidRow(
               column(1,
                      radioButtons("whichplot2",h5("Choose outcome"),
                                   choices = c('Completed interviews','Completed interviews by respondent characteristic','Call attempts'),
                                   selected = 'Completed interviews')
               ),
               column(5,
                      # only show this panel if they choose CATI Outcomes
                      conditionalPanel(
                        condition = "input.whichplot2 =='Call attempts'", plotOutput("callsperint",height = "400px")
                      ),
                      # only show this panel if they choose Call Outcomes
                      conditionalPanel(
                        condition = "input.whichplot2 =='Completed interviews'", plotOutput("compperint",height = "400px")
                      ),
                      conditionalPanel(
                        condition = "input.whichplot2 =='Completed interviews by respondent characteristic'", plotOutput("compperintchar",height = "400px")
                      )
               ),
               column(6,
                      plotOutput('catibyenum',height = "400px")
               )
             ),
             fluidRow(
               column(1,
                      selectInput('choosemonth',h5('Choose month to filter plots:'),
                                  choices = c('Overall',list_mos))#'Jan-22', 'Feb-22','Mar-22','Apr-22','May-22','Jun-22','Jul-22','Aug-22','Sep-22','Oct-22','Nov-22','Dec-22'
               ),
               column(5,
                      plotOutput('timeonphone',height = "400px")
               ),
               column(6,
                      plotOutput('timeonphoneCATI',height = "400px"))
             )
    ),
    # tab 5
    tabPanel('Sociodemographic Characteristics',
             fluidRow(
               column(1,
                      selectInput("bymonth",h5("Choose month to filter age and sex distribution"),
                                  choices = c('Overall',list_mos),#'Jan-22', 'Feb-22','Mar-22','Apr-22','May-22','Jun-22','Jul-22','Aug-22','Sep-22','Oct-22','Nov-22','Dec-22'
                                  selected = 'Overall')
               ),
               column(5,
                      plotOutput('agesexdist',height = "800px")
               ),
               column(6,
                      div(style='height:800px; overflow-y: scroll',
                          tableOutput("sociodemotbl")
                      )
               )))#,
    # # tab 6
    # tabPanel('Household Membership and Deaths/Parental Survival',
    #          fluidRow(
    #              column(4,
    #                     plotOutput('CDRandparentdeaths')
    #              ),
    #              column(4,
    #                     plotOutput('HHsize')
    #              ),
    #              column(4,
    #                     plotOutput('decparentbyprov'))
    #          ),
    #          fluidRow(
    #              column(5,
    #                     plotOutput('survparentsbyage')
    #              ),
    #              column(1,
    #                     selectInput('chooseparent', h5('Choose parents to filter:'),
    #                                 choices = c('Both','Mother','Father'))
    #              ),
    #              column(6, 
    #                     plotOutput('agesofsurvparentsbyage'))
    #          )),
    # # tab 7
    # tabPanel('Sibling Survival',
    #          fluidRow(column(4,
    #                          tableOutput('numsibdeaths')
    #          ),
    #          column(4,
    #                 plotOutput('sibsbyprov')
    #          ),
    #          column(4, 
    #                 plotOutput('sibsbysex'))
    #          ),
    #          fluidRow(column(6,
    #                          plotOutput('decsibsbyrespage')
    #          ),
    #          column(6, 
    #                 plotOutput('distribbyrespage')))),
    # # tab 8
    # tabPanel('Deaths since 2019',
    #          fluidRow(column(6,
    #                          tableOutput('causeofdeath')
    #          ),
    #          column(6,
    #                 tableOutput('placeofdeath')
    #          )),
    #          fluidRow(column(6,
    #                          tableOutput('placeofburial')
    #          ),
    #          column(6,
    #                 tableOutput('registereddeath')
    #          ))
    # tab 9
    ,
    tabPanel('Truncated pregnancy history',
             fluidRow(column(6, 
                             tableOutput('tphbirths')
                             ),
                      column(6, 
                             tableOutput('tphdeaths')))),
    # tab 10s
    tabPanel('Full pregnancy history',
             fluidRow(column(6, 
                             tableOutput('fphbirths'),
                             ),
                      column(6, tableOutput('fphdeaths'))))
  )
  
)


server <- function(input, output, session) {
  # Subset so that only the selected rows are in model.data
  global_Cons <- Consentedmw
  Consented_enum <- reactive({
    # subset(Consented, enumerator %in% input$enumerators)
    if (input$chooseenum == "All") {
      global_Cons
    } else {
      Consentedmw[Consentedmw$enumerator == input$chooseenum,]
    }
  })
  
  #tab 1
  output$CATIdayandcum <- renderPlot({
    Consented_enum <- Consented_enum() %>%
      arrange(Date.Interview) %>%
      dplyr::filter(Date.Interview >= input$slider[1] & Date.Interview <= input$slider[2])
    Counts <- aggregate(data.frame(count = Consented_enum$Date.Interview),
                        list(value = Consented_enum$Date.Interview),
                        length)[,2]
    
    Cumulative <- cumsum(Counts)
    Date <- unique(as.Date(Consented_enum$Date.Interview))
    dayandcum <- data.frame(Counts=Counts, Cumulative=Cumulative, Date= Date) %>%
      #mutate(Date = ymd(Date)) %>%
      arrange(Date)
    ggplot(data = dayandcum) +
      geom_bar(aes(x=Date, y=Counts), stat="identity", fill='#D95F02',color="black") +
      geom_text(aes(x=Date, y=Counts, label=Counts), vjust=-1, color="black", size=3.5)+
      theme_minimal() +
      xlab("Date") + ylab("Completed Interviews") +
      geom_line(aes(x = Date, y=Cumulative/65), group =1, colour="#1B9E77", size = 1) +
      geom_text(data = dayandcum[which.max(dayandcum[,"Date"]),],aes(x = Date, y = Cumulative/65, label=Cumulative), size=4, vjust = -0.8)+
      scale_y_continuous(
        name = "Daily Completed interviews",
        sec.axis = sec_axis(trans=~.*65, name="Cumulative Completed interviews"))+
      scale_x_date(date_breaks = '1 week') +
      labs(title = paste0('Completed CATI interviews, by day and cumulative',input$enumerators))+
      theme(axis.title.y = element_text(color = '#D95F02', size=13),
            axis.title.y.right = element_text(color = "#1B9E77", size=13),
            axis.text.x = element_text(angle = 90)
      )
  })
  
  quotas_region1 <- reactive({
    # subset(Consented, enumerator %in% input$enumerators)
    if (input$chooseregion == 'Northern') {
      quotascount.tab1[,c(1:4,13)]
    } else if (input$chooseregion == 'Central') {
      quotascount.tab1[,c(5:8,13)]
    } else if (input$chooseregion == 'Southern') {
      quotascount.tab1[,c(9:13)]
    }
  })
  
  quotas_region2 <- reactive({
    # subset(Consented, enumerator %in% input$enumerators)
    if (input$chooseregion == 'Northern') {
      quotascount.tab2[,c(1:4,13)]
    } else if (input$chooseregion == 'Central') {
      quotascount.tab2[,c(5:8,13)]
    } else if (input$chooseregion == 'Southern') {
      quotascount.tab2[,c(9:13)]
    }
  })
  
  quotas_region3 <- reactive({
    # subset(Consented, enumerator %in% input$enumerators)
    if (input$chooseregion == 'Northern') {
      quotascount.tab3[,c(1:4,13)]
    } else if (input$chooseregion == 'Central') {
      quotascount.tab3[,c(5:8,13)]
    } else if (input$chooseregion == 'Southern') {
      quotascount.tab3[,c(9:13)]
    }
  })
  
  output$CATIquota1 <- function(){
    quotascount.tab1 <- quotas_region1() %>%
      kbl(caption = "Progress to quota - Block 1 (Dec'21-May 25 '22)") %>%
      kable_classic(full_width = F, html_font = "Arial")%>%
      kable_styling(latex_options = 'hold_position')
    quotascount.tab1
    #add_header_above(quotascount.tab1, c(" " =1 ,"Northern " = 4,"Central " = 4 ,'Southern ' = 4, " "=1))
  }
  
  output$CATIquota2 <- function(){
    quotascount.tab2 <- quotas_region2() %>%
      kbl(caption = "Progress to quota - Block 2 (May 25 '22-Sep 13'22)") %>%
      kable_classic(full_width = F, html_font = "Arial")%>%
      kable_styling(latex_options = 'hold_position')
    quotascount.tab2
    #add_header_above(quotascount.tab1, c(" " =1 ,"Northern " = 4,"Central " = 4 ,'Southern ' = 4, " "=1))
  }
  
  output$CATIquota3 <- function(){
    quotascount.tab3 <- quotas_region3() %>%
      kbl(caption = "Progress to quota - Block 3 (Sep 14 '22 - present)") %>%
      kable_classic(full_width = F, html_font = "Arial")%>%
      kable_styling(latex_options = 'hold_position')
    quotascount.tab3
    #add_header_above(quotascount.tab1, c(" " =1 ,"Northern " = 4,"Central " = 4 ,'Southern ' = 4, " "=1))
  }
  
  quotaperc_region1 <- reactive({
    # subset(Consented, enumerator %in% input$enumerators)
    if (input$chooseregion == 'Northern') {
      quotasperc1[,c(1:4)]
    } else if (input$chooseregion == 'Central') {
      quotasperc1[,c(5:8)]
    } else if (input$chooseregion == 'Southern') {
      quotasperc1[,c(9:12)]
    }
  })
  
  quotaperc_region2 <- reactive({
    # subset(Consented, enumerator %in% input$enumerators)
    if (input$chooseregion == 'Northern') {
      quotasperc2[,c(1:4)]
    } else if (input$chooseregion == 'Central') {
      quotasperc2[,c(5:8)]
    } else if (input$chooseregion == 'Southern') {
      quotasperc2[,c(9:12)]
    }
  })
  
  quotaperc_region3 <- reactive({
    # subset(Consented, enumerator %in% input$enumerators)
    if (input$chooseregion == 'Northern') {
      quotasperc3[,c(1:4)]
    } else if (input$chooseregion == 'Central') {
      quotasperc3[,c(5:8)]
    } else if (input$chooseregion == 'Southern') {
      quotasperc3[,c(9:12)]
    }
  })

  output$percCATIquota1 <- function(){
    quotasperc1 <- quotaperc_region1() %>%
      kbl(caption = "Percent of quota collected - Block 1 (Dec'21-May 25 '22)") %>%
      kable_classic(full_width = F, html_font = "Arial") %>%
      kable_styling(latex_options = 'hold_position')
    quotasperc1
    #add_header_above(quotasperc12, c(c(" " =1 ,"Northern " = 4,"Central " = 4 ,'Southern ' = 4)))
  }
  
  output$percCATIquota2 <- function(){
    quotasperc2 <- quotaperc_region2() %>%
      kbl(caption = "Percent of quota collected - Block 2 (May 25 '22-Sep 13'22)") %>%
      kable_classic(full_width = F, html_font = "Arial") %>%
      kable_styling(latex_options = 'hold_position')
    quotasperc2
    #add_header_above(quotasperc12, c(c(" " =1 ,"Northern " = 4,"Central " = 4 ,'Southern ' = 4)))
  }
  
  output$percCATIquota3 <- function(){
    quotasperc3 <- quotaperc_region3() %>%
      kbl(caption = "Percent of quota collected - Block 3 (Sep 14 '22 - present)") %>%
      kable_classic(full_width = F, html_font = "Arial") %>%
      kable_styling(latex_options = 'hold_position')
    quotasperc3
    #add_header_above(quotasperc12, c(c(" " =1 ,"Northern " = 4,"Central " = 4 ,'Southern ' = 4)))
  }
  
  output$Outcomebyorder <- renderPlot({
    # outcomes <- outcomes %>%
    #     filter(Date.Interview > input$slider[1] & Date.Interview < input$slider[2])
    ggplot(data = subset(outcomes, Outcome.FINAL == 'COMP') ) +
      geom_line(aes(x = call_num_grp, y = perccompletedpercallnum), group=1, color = '#D95F02', size = 1.2) +
      geom_text(aes(x = call_num_grp, y = perccompletedpercallnum, label = paste0(perccompletedpercallnum,'%')), color = 'black',fontface = 'bold',vjust = 1.2) +
      xlab('Call attempt order') +
      ylab('% of completed CATIs per call num') +
      theme(legend.position = 'none') + 
      coord_cartesian(ylim = c(0,60))
  })
  
  output$Compbycall <- renderPlot({
    # outcomes <- outcomes %>%
    #     filter(Date.Interview > input$slider[1] & Date.Interview < input$slider[2])
    ggplot(data = outcomes) + 
      geom_line(aes(x = call_num_grp, y = perccum), group=1, color = '#D95F02', size = 0.8) + 
      geom_bar(aes(y = log(Calls), x = call_num_grp), stat = 'identity', fill = '#1B9E77', color = 'black') +
      geom_text(aes(x = call_num_grp, y = perccum, label = paste(perccum,'%')), fontface = 'bold', vjust = 1, color = 'black') +
      geom_text(aes(y = log(Calls), x = call_num_grp, label = Calls), fontface = 'bold', vjust = 1, color = 'black') +
      xlab('Call attempt order') + 
      scale_y_continuous(
        name = "Cumulative percentage of phone #s with completed CATI",
        sec.axis = sec_axis( trans=~exp(.), name="Phone calls made (log scale)", breaks = c(200,1000,10000))) +
      theme(axis.title.y = element_text(color = '#D95F02', size=12),
            axis.title.y.right = element_text(color = "#1B9E77", size=12)) +
      coord_cartesian(ylim = c(0,25))
  })
  
  # tab 2
  output$catioutcomes <- renderPlot({
    catitime <- dget('CATIOutcomesTimeMW.R')
    outcomesfunc <- dget('Outcomes_funcMW.R')
    if(input$choosemonth2 == 'Overall') {
      outcomesfunc(data_clean3, dat.widemw, type = 'cati')
    }
    else {
      catitime(dat.widemw, monthtoplot = input$choosemonth2)
    }
  })
  
  
  output$calloutcomes <- renderPlot({
    calltime <- dget('CallOutcomesTimeMW.R')
    outcomesfunc <- dget('Outcomes_funcMW.R')
    if(input$choosemonth2 == 'Overall') {
      outcomesfunc(data_clean3, dat.widemw, type = 'call')
    }
    else {
      calltime(data_clean3, monthtoplot = input$choosemonth2)
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
            INCOR = Phone was answered, but not by the respondent<br/>
            NR = The call rang through, but no answer/busy<br/>
            NNA = The number is in operation but unable to be accessed because it is switched off or doesn't have network coverage<br/>
            REFER = Respondent is not eligible, referred other household member<br/>
            REASS = Respondent answered the phone, but spoke a language not spoken by any enumerators<br/>
            DEFER = Respondent is not eligible at this time (quota filled) and was deferred<br/>
            Other = Any other outcome")})
  
  output$RRtable <- function(){
    RRdata <- dget('ResponseRateFuncMW.R')
    print('rrfunc function called')
    RRtbl <- RRdata(data_clean3, dat.widemw, Consentedmw)
    print('rrfunc created table')
    stats <- RRtbl %>% dplyr::filter(Statistic!='')
    stats <- stats[,1]
    RRtbl1 <- RRtbl %>%
      dplyr::select(-Statistic) %>%
      cbind(stats) %>%
      dplyr::rename(Statistic = stats) %>%
      dplyr::select(Statistic, everything())
    
    matrix.rates.tbl <- RRtbl1 %>%
      kbl() %>%
      kable_classic(position = "left",full_width = F, html_font = "Arial")
    matrix.rates.tbl
  } 
  
  # tab 3
  Duration_enum <- reactive({
    if (input$chooseenum2 == "All") {
      global_Cons %>%
        dplyr::mutate(duration = as.integer(duration),
                      `Year-Month` = format(as.Date(Date.Interview), "%Y-%m"),
                      enumerator = ordered(enumerator, levels = c("E1", "E2", "E3", "E4", "E5", "E6",
                                                                  "E7", "E8", "E9", "E10", 'E11', 'E12',
                                                                  'E13', 'E14', 'E15','E16','E17','E18',
                                                                  'E19','E20','E21','E22','E23','E24','E25','E26'))) %>%
        dplyr::ungroup()
    } else {
      Cons <- Consentedmw[Consentedmw$enumerator == input$chooseenum2,]
      Cons %>%
        dplyr::mutate(duration = as.integer(duration),
                      `Year-Month` = format(as.Date(Date.Interview), "%Y-%m"),
                      enumerator = ordered(enumerator, levels = c("E1", "E2", "E3", "E4", "E5", "E6",
                                                                  "E7", "E8", "E9", "E10", 'E11', 'E12',
                                                                  'E13', 'E14', 'E15','E16','E17','E18',
                                                                  'E19','E20','E21','E22','E23','E24','E25','E26'))) %>%
        dplyr::ungroup()
    }
  })
  
  output$phoneappduration <- renderPlot({
    Duration_long <- Duration_enum() %>%
      dplyr::mutate(min = duration/60)
    
    ggplot(data = Duration_long)+#subset(Duration_long, enumerator !='E15')) +
      geom_boxplot(aes(x = enumerator, y = min, fill = as.factor(F.ReproAge))) +
      theme_minimal() + xlab('') +ylab('Time (min)') +
      scale_fill_manual(values = c("#D95F02","#7570B3"))+
      labs(fill = "Respondent characteristics",title = 'Distribution of duration in the application per interview by enumerator')
  })
  
  output$avgintsperdayperenum <- renderPlot({
    jan <- 21 
    feb <- 20
    mar <- 23
    apr <- 21
    may <- 13
    jun <- 22
    jul <- 21
    aug <- 23
    sep <- 20
    oct22 <- 21
    nov22 <- 22
    dec22 <- 20 # minus 2 days for holidays
    jan23 <- 22
    feb23 <- 20
    mar23 <- 23
    apr23 <- 20
    may23 <- 23
    jun23 <- 22
    
    plot <- Consentedmw %>% 
      dplyr::filter(!is.na(enumerator)) %>%
      dplyr::group_by(month.interview) %>%
      dplyr::summarise(compint = n()) %>%
      dplyr::mutate(compintperenum = ifelse(month.interview == 'Jan-22', compint / jan / length(unique(data_clean3[data_clean3$month.interview=='Jan-22',]$enumerator)),
                                            ifelse(month.interview=='Feb-22',compint/feb/length(unique(data_clean3[data_clean3$month.interview=='Feb-22',]$enumerator)),
                                                   ifelse(month.interview =='Mar-22',compint / mar / length(unique(data_clean3[data_clean3$month.interview=='Mar-22',]$enumerator)),
                                                          ifelse(month.interview == 'Apr-22', compint / apr / length(unique(data_clean3[data_clean3$month.interview=='Apr-22',]$enumerator)),
                                                                 ifelse(month.interview == 'May-22', compint / may / length(unique(data_clean3[data_clean3$month.interview=='May-22',]$enumerator)),
                                                                        ifelse(month.interview == 'Jun-22', compint / jun / length(unique(data_clean3[data_clean3$month.interview=='Jun-22',]$enumerator)),
                                                                               ifelse(month.interview == 'Jul-22', compint/jul / length(unique(data_clean3[data_clean3$month.interview=='Jul-22',]$enumerator)),
                                                                                      ifelse(month.interview == 'Aug-22', compint/aug / length(unique(data_clean3[data_clean3$month.interview=='Aug-22',]$enumerator)), 
                                                                                             ifelse(month.interview == 'Sep-22', compint/sep / length(unique(data_clean3[data_clean3$month.interview=='Sep-22',]$enumerator)), 
                                                                                                    if_else(month.interview == 'Oct-22' & Sys.Date() >= '2022-10-01', compint/oct22/length(unique(data_clean3[data_clean3$month.interview =='Oct-22',]$enumerator)),
                                                                                                            if_else(month.interview == 'Nov-22' & Sys.Date() >= '2022-11-01', compint/nov22/length(unique(data_clean3[data_clean3$month.interview =='Nov-22',]$enumerator)),
                                                                                                                    if_else(month.interview == 'Dec-22' & Sys.Date() >= '2022-12-01', compint/dec22/length(unique(data_clean3[data_clean3$month.interview =='Dec-22',]$enumerator)),
                                                                                                                            if_else(month.interview == 'Jan-23' & Sys.Date() >= '2023-01-01', compint/jan23/length(unique(data_clean3[data_clean3$month.interview =='Jan-23',]$enumerator)),
                                                                                                                                    if_else(month.interview == 'Feb-23' & Sys.Date() >= '2023-02-01', compint/feb23/length(unique(data_clean3[data_clean3$month.interview =='Feb-23',]$enumerator)),
                                                                                                                                            if_else(month.interview == 'Mar-23' & Sys.Date() >= '2023-03-01', compint/mar23/length(unique(data_clean3[data_clean3$month.interview =='Mar-23',]$enumerator)),
                                                                                                                                                    if_else(month.interview == 'Apr-23' & Sys.Date() >= '2023-04-01', compint/apr23/length(unique(data_clean3[data_clean3$month.interview =='Apr-23',]$enumerator)),
                                                                                                                                                            if_else(month.interview == 'May-23' & Sys.Date() >= '2023-05-01', compint/may23/length(unique(data_clean3[data_clean3$month.interview =='May-23',]$enumerator)),
                                                                                                                                                                    if_else(month.interview == 'Jun-23' & Sys.Date() >= '2023-06-01', compint/mar23/length(unique(data_clean3[data_clean3$month.interview =='Jun-23',]$enumerator)),
                                                                                                                                    0)))))))))))))))))),
                    month.interview = factor(month.interview, levels = c('Jan-22','Feb-22','Mar-22','Apr-22','May-22','Jun-22','Jul-22','Aug-22','Sep-22', 'Oct-22','Nov-22','Dec-22','Jan-23','Feb-23','Mar-23','Apr-23','May-23','Jun-23'))) 
    ggplot(data = plot) +
      geom_line(group = 1, aes(x = month.interview, y = compintperenum), size = 2, color = '#D95F02') +
      geom_text(aes(x = month.interview, y = compintperenum, label = round(compintperenum,2), hjust = 1.4, fontface = 'bold')) +
      labs(y = 'Average completed interviews per enumerator per day',
           x = 'Month')
  })
  
  output$intloc <- renderPlot({
    #=== Location from where call was taken
    Location <- data.frame(Location = c('HOME','WORKPLACE/ SCHOOL','OTHER LOCATION','ON THE ROAD','MARKET', 'REFUSE'),
                           Number = table(Consentedmw$i1_1),
                           Proportion = round(prop.table(table(Consentedmw$i1_1))*100,1))
    
    title <- substitute(paste("Location from which respondents took the\nconsented interview, n=",sum(Location$Number),sep=""), list(n = sum(Location$Number)))
    
    ggplot(Location, aes(x="", y=Proportion.Freq, fill=Location)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) +
      theme_void() +
      theme(legend.position="right") +
      scale_fill_brewer(palette="Dark2")+
      # labs(caption = paste0("n=",n)) +
      # geom_text(aes(y = ypos, label = Number.Freq), color = "white", size=4) +
      theme(text = element_text(family = 'Arial'))
  })
  
  output$phoneowner <- renderPlot({
    Consentedmw <- Consentedmw %>% dplyr::mutate(b4_1 = ifelse(b4_1 == 1,'Owns phone',
                                                               ifelse(b4_1 == 2,'Belongs to someone else',NA)))
    pie.data <- data.frame(prop = prop.table(table(Consentedmw$b4_1)))
    pie.data%>%
      dplyr::mutate(Outcome = prop.Var1)%>%
      ggplot(aes(x="", y=prop.Freq, fill=Outcome)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) +theme_void() +
      theme(legend.position="right") +
      scale_fill_brewer(palette="Dark2")
  })
  
  #tab 4
  consbymonth <- reactive({
    if (input$choosemonth == "Overall") {
      Consentedmw
    } else {
      Consentedmw[Consentedmw$month.interview %in% input$choosemonth,]
    }
  })
  databymonth <- reactive({
    if (input$choosemonth == "Overall") {
      data_clean3
    } else {
      data_clean3[data_clean3$month.interview %in% input$choosemonth,]
    }
  })
  
  output$callsperint <- renderPlot({
    dat <- databymonth()
    dat$counter <- 1
    firstdate <- as.Date("2022-01-24"); lastdate <- as.Date(dat$Date.Interview[nrow(dat)])
    collection.time <- as.numeric(lastdate- firstdate)
    
    d1 <- dat %>%
      dplyr::group_by(enumerator, Date.Interview) %>%
      dplyr::summarize(Sum = sum(counter)/collection.time,
                       totalperday = sum(counter))
    
    d1$enumerator <- ordered(d1$enumerator, levels = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", 'E11',
                                                       'E12', 'E13', 'E14','E15','E16','E17','E18','E19','E20','E21','E22','E23','E24','E25','E26'))
    
    ggplot(data=d1, aes(x=enumerator, y= totalperday, fill="#1B9E77")) +
      geom_boxplot()+theme_minimal() +
      theme(legend.position="none") +
      ylab("Average calls per day") + xlab("")+
      scale_fill_manual(values= c("#1B9E77"))
  })
  
  output$compperint <- renderPlot({
    conss <- consbymonth()
    Counts <- aggregate(data.frame(count = conss$Date.Interview),
                        by= list(value = conss$Date.Interview,
                                 enumerator = conss$enumerator),length) %>%
      dplyr::group_by(enumerator) %>%
      dplyr::mutate(csum = cumsum(count)) %>% as.data.frame() 
    
    Counts$enumerator <- ordered(Counts$enumerator, levels = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", 'E11', 'E12', 'E13', 'E14', 'E15','E16','E17','E18','E19','E20','E21','E22','E23','E24','E25','E26'))
    
    ggplot(data = Counts) +
      geom_boxplot(aes(x = enumerator, y = count, fill = "#D95F02")) + 
      scale_y_continuous(name = 'Daily Completed interviews') +
      scale_x_discrete() +
      theme(axis.title.y = element_text(color = '#D95F02')) + 
      theme_minimal() + xlab('Enumerator')+
      theme(legend.position="none") + 
      labs(fill = "Respondent characteristics")+
      scale_fill_manual(values = c("#D95F02","#7570B3"))
  })
  
  output$compperintchar <- renderPlot({
    conss <- consbymonth()
    Counts <- aggregate(data.frame(count = conss$Date.Interview),
                        by= list(value = conss$Date.Interview,
                                 enumerator = conss$enumerator,
                                 woman=conss$F.ReproAge),length) %>%
      dplyr::group_by(enumerator,woman) %>%
      dplyr::mutate(csum = cumsum(count)) %>% as.data.frame() 
    
    Counts$enumerator <- ordered(Counts$enumerator, levels = c("E1", "E2", "E3", "E4", "E5", "E6", "E7",
                                                               "E8", "E9", "E10", 'E11', 'E12', 'E13', 'E14', 
                                                               'E15','E16','E17','E18','E19','E20','E21','E22','E23','E24','E25'))
    
    ggplot(data = Counts) +
      geom_boxplot(aes(x = enumerator, y = count, fill = woman)) + 
      scale_y_continuous(name = 'Daily Completed interviews') +
      scale_x_discrete() +
      theme(axis.title.y = element_text(color = '#D95F02')) + 
      theme_minimal() + xlab('Enumerator')+
      # theme(legend.position="none") + 
      labs(fill = "Respondent characteristics")+
      scale_fill_manual(values = c("#D95F02","#7570B3"))
  })
  
  output$catibyenum <- renderPlot({
    # === CATI Outcomes by Enumerator, excluding dysfunctional numbers  
    d1 <- dat.widemw %>%
      dplyr::filter(Outcome.FINAL!= 'NNU') %>%
      dplyr::group_by(enum, Outcome.FINAL) %>%
      dplyr::summarize(n = n()) %>% 
      dplyr::filter(!is.na(enum))
    
    d1$enum <- ordered(d1$enum, levels = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9",
                                           "E10",'E11', 'E12', 'E13', 'E14', 'E15','E16','E17',
                                           'E18','E19','E20','E21','E22','E23','E24','E25','E26'))
    ggplot(d1) +
      geom_bar(aes(x = enum, y = n, fill = Outcome.FINAL), position = 'stack',stat = 'identity', color= 'black') +
      scale_x_discrete() +
      geom_text(aes(x = enum, y = n, group = Outcome.FINAL, label = n),position = position_stack(vjust = .5)) +
      theme(axis.title.y = element_text(color = '#D95F02')) + 
      theme_minimal() + ylab('Call attempt outcomes per Enumerator') +
      scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666","#fb9a99","#a6cee3")) +labs(fill = 'Outcome')
  })
  
  output$timeonphone <- renderPlot({
    dat <- databymonth()
    # === Daily time on CATI application and on the phone, per enumerator
    d1 <- dat %>%
      dplyr::filter(Date.Interview >= '2021-09-09') %>%
      dplyr::mutate(Phone.duration = as.numeric(Phone.duration)) %>%
      dplyr::group_by(enumerator, Date.Interview) %>%
      dplyr::summarise(timeperday = sum(Phone.duration)/60/60) %>%
      pivot_longer(timeperday, names_to = "timeonwhat",values_to = 'hours')
    
    d1$enumerator <- ordered(d1$enumerator, levels = c("E1", "E2", "E3", "E4", "E5", "E6",
                                                       "E7", "E8", "E9", "E10", 'E11', 'E12',
                                                       'E13', 'E14', 'E15','E16','E17','E18','E19',
                                                       'E20','E21','E22','E23','E24','E25','E26'))
    
    ggplot(d1) + geom_boxplot(aes(x = enumerator, y = hours, fill = "#1B9E77")) +
      theme_minimal() +
      xlab("") + ylab("Time on phone per day (hours)") +
      theme(axis.text.x = element_text(angle = 0)) +
      scale_x_discrete() +
      scale_fill_manual(values = c("#1B9E77")) +
      theme(legend.position="none")
  })
  
  output$timeonphoneCATI <- renderPlot({
    # === Time on phone to complete CATI interview (consenting respondents)  
    conss <- consbymonth()
    d1 <- subset(conss, Phone.duration<3000) %>%
      dplyr::mutate(Phone.duration = as.numeric(Phone.duration))
    d1$enumerator <- ordered(d1$enumerator, levels = c("E1", "E2", "E3", "E4", "E5", "E6",
                                                       "E7", "E8", "E9", "E10", 'E11', 'E12', 
                                                       'E13', 'E14', 'E15','E16','E17','E18','E19',
                                                       'E20','E21','E22','E23','E24','E25','E26'))
    
    ggplot(data = subset(d1, Phone.duration >0), aes(x = enumerator, y = Phone.duration/60, fill = F.ReproAge)) +
      geom_boxplot() +
      theme_minimal() +
      ylab("Time to complete consented interview (minutes)") +
      theme() +
      labs(fill = "Respondent characteristics")+
      scale_fill_manual(values = c("#D95F02","#7570B3"))
  })
  
  # tab 5
  output$sociodemotbl <- function(){
    sociodemofunc <- dget('Sociodemotbl_funcMW.R')
    #print('sociodemo func function called')
    sociodemofunc(Consentedmw, Northern, Central, Southern)
  }
  
  output$agesexdist <- renderPlot({
    agesexbymo <- dget('AgeSexScriptMW.R')
    if (input$bymonth == "Overall") {
      agesexbymo(Consentedmw, Northern, Central, Southern,bymonth = 'Overall')
    }
    else {
      agesexbymo(Consentedmw, Northern, Central, Southern,bymonth = input$bymonth)
    }
  })
  
  #tab 9
  
# source('MW_Preg_hist_TPH.R')
  # library(haven)
# Live.births <- mwpregfunc(Consentedmw)
  Live.births <- read.csv(paste0('TPH_MW.csv'))

  output$tphbirths <- function(){
    tbl <- table(Live.births$pregbaby, Live.births$Born.A.D) %>%
      as.data.frame.matrix()
    tbl$`Pregnancy-Baby` <- rownames(tbl)
    rownames(tbl) <- NULL

    tbl %>%
      select(c(`Pregnancy-Baby`, everything())) %>%
      adorn_totals() %>%
      kbl(caption = "Survival status at birth - TPH") %>%
      kable_classic(full_width = F, html_font = "Arial") %>%
      footnote(general = "In T1, information on only the most recent pregnancy (1) was collected.")
    }

  output$tphdeaths <- function(){
    tbl <- table(Live.births[Live.births$Born.A.D=='Alive',]$pregbaby, Live.births[Live.births$Born.A.D=='Alive',]$Now.A.D) %>%
      as.data.frame.matrix()
    tbl$`Pregnancy-Baby` <- rownames(tbl)
    rownames(tbl) <- NULL

    tbl %>%
      select(c(`Pregnancy-Baby`, everything())) %>%
      adorn_totals() %>%
      kbl(caption = "Survival status of live-born children at time of questionnaire - TPH") %>%
      kable_classic(full_width = F, html_font = "Arial") %>%
      footnote(general = "In T1, information on only the most recent pregnancy (1) was collected. Additionally, in T1, respondents were only asked if their child was still alive if they were born within the last 7 years. In T2, this question was asked for every child born alive.")
  }

  # tab 10
  # source('MW_Preg_hist_FPH.R')
  # library(haven)
  # Live.births_fph <- mwpregfunc_fph(Consentedmw)
  Live.births_fph <- read.csv(paste0('FPH_MW.csv'))

  output$fphbirths <- function(){
    tbl <- table(Live.births_fph$pregbaby, Live.births_fph$Born.A.D) %>%
      as.data.frame.matrix()
    tbl$`Pregnancy-Baby` <- rownames(tbl)
    rownames(tbl) <- NULL

    tbl_ <- tbl %>%
      select(c(`Pregnancy-Baby`, everything())) %>%
      mutate(across(as.numeric())) %>%
      adorn_totals() %>%
      kbl(caption = "Survival status at birth - FPH") %>%
      kable_classic(full_width = F, html_font = "Arial") %>%
      footnote(general = "The full pregnancy history module was introduced in T2 which began on 26 May, 2022. In T2, 50% of the female respondents of childbearing age were assigned to the FPH module.")
    tbl_
  }

  output$fphdeaths <- function(){
    tbl <- table(Live.births_fph[Live.births_fph$Born.A.D=='Alive',]$pregbaby, Live.births_fph[Live.births_fph$Born.A.D=='Alive',]$Now.A.D) %>%
      as.data.frame.matrix()
    tbl$`Pregnancy-Baby` <- rownames(tbl)
    rownames(tbl) <- NULL

    tbl_ <- tbl %>%
      select(c(`Pregnancy-Baby`, everything())) %>%
      mutate(across(as.numeric())) %>%
      adorn_totals() %>%
      kbl(caption = "Survival status of live-born children at time of questionnaire - FPH") %>%
      kable_classic(full_width = F, html_font = "Arial") %>%
      footnote(general = "The full pregnancy history module was introduced in T2 which began on 26 May, 2022. In T2, 50% of the female respondents of childbearing age were assigned to the FPH module./n This table includes all reported deaths, including those who died over 5 years of age.")
    tbl_
}
}

# Run the application 
shinyApp(ui = ui, server = server)

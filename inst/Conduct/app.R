#library("dichromat")
#library("DT")
#library("plyr")
#library("scales")

#library("ggplot2")
library("knitr")
library("shiny")
library("shinyBS")

library("rhandsontable")
library("data.table")

#source("../mainDoseEscalation.R")
source("mainDoseEscalation.R")

ui <- fluidPage(
  
  img(src="MRC.png", height="100", align="right"),
  titlePanel(title="Evaluating a Model-Based Dose-Escalation Study"),
  h5("Philip Pallmann & Fang Wan"),
  h6("Department of Mathematics & Statistics, Lancaster University, UK"),
  
  fluidRow(
    
    column(3,
           
           wellPanel(
             tags$h3("1. Upload design file"),
             p("You can obtain the design file from the design app."),
             fileInput(inputId="designfile", label="",
                       accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
           ),
           
           wellPanel(
             tags$h3("2. Upload data"),
             checkboxInput("howtoenter", "Enter data manually into a spreadsheet"),
             bsTooltip("howtoenter", "Tick here if you want to enter the data manually rather than upload a CSV file.",
                       placement="right", trigger="hover", options=list(container="body")),
             conditionalPanel(
               condition="input.howtoenter == false",
               p("The dataset must be a CSV file that has (at least) three columns: one for
               the cohort, one for the dose, and one for the response (0: no toxicity; 1: toxicity)."),
               fileInput(inputId="file", label="",
                         accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
               checkboxInput(inputId="head", label="Column headlines in the first row?", value=TRUE),
               radioButtons(inputId="sep", label="Column separator", choices=c(Comma=",", Semicolon=";", Tab="\t"),
                            selected=",", inline=TRUE),
               radioButtons(inputId="dec", label="Decimal separator", choices=c(Comma=",", Point="."),
                            selected=".", inline=TRUE),
               bsTooltip("sep", "What symbol is used to separate columns in your CSV file? Double-check this if your data table looks messed up.",
                         placement="right", trigger="hover", options=list(container="body")),
               bsTooltip("dec", "What symbol is used to separate decimals in your CSV file? Double-check this if your data table looks messed up.",
                         placement="right", trigger="hover", options=list(container="body")),
               hr(),
               p("Once the dataset has been uploaded, a", strong("cohort,"), "a", strong("dose,"), "and a",
                 strong("response"), "variable must be specified."),
               uiOutput(outputId="cohort"),
               uiOutput(outputId="dose"),
               uiOutput(outputId="outcome"),
               bsTooltip("cohort", "Which column in your dataset specifies the cohort?",
                         placement="right", trigger="hover", options=list(container="body")),
               bsTooltip("dose", "Which column in your dataset specifies the dose?",
                         placement="right", trigger="hover", options=list(container="body")),
               bsTooltip("outcome", "Which column in your dataset specifies the response (toxicity / no toxicity)?",
                         placement="right", trigger="hover", options=list(container="body"))
             ),
             conditionalPanel(
               condition="input.howtoenter == true",
               rHandsontableOutput("hot"),
               bsTooltip("hot", "Enter cohorts, doses, and toxicity events here.",
                         placement="right", trigger="hover", options=list(container="body")),
               p(""),
               p("Click on a cell to manipulate its entry."),
               p("Right-click anywhere on the table to add or remove rows.")
             )
           )
    ),
    
    column(6,
           
           tabsetPanel(
             tabPanel("Design",
                      hr(),
                      bsAlert("alertDesign1"),
                      textOutput("DesignText1"),
                      tableOutput("DesignTable1"),
                      hr(),
                      textOutput("DesignText2"),
                      tableOutput("DesignTable2")),
             tabPanel("Dataset",
                      hr(),
                      bsAlert("alertDataset1"),
                      dataTableOutput("DatasetTable"),
                      hr(),
                      textOutput("DatasetText"),
                      plotOutput("DatasetPlot", width=800, height=400)),
             tabPanel("Recommendation",
                      hr(),
                      uiOutput("RecommendationText1"),
                      hr(),
                      textOutput("RecommendationText2"),
                      p(""),
                      tableOutput("RecommendationTable"),
                      textOutput("RecommendationText3"),
                      plotOutput("RecommendationPlot", width=800, height=600),
                      uiOutput("RecommendationTickbox")),
             tabPanel("Download",
                      hr(),
                      h4("Report"),
                      p("Download a pdf report summarising the design, study data, and dose recommendation."),
                      downloadButton("report")),
             tabPanel("References",
                      hr(),
                      p("This is an implementation of Bayesian dose escalation as proposed in:"),
                      p("Zhou Y, Whitehead J (2003)",
                        HTML("<a href='http://journals.sagepub.com/doi/abs/10.1177/009286150303700108'>Practical implementation of Bayesian dose-escalation procedures.</a>"), 
                        "Drug Information Journal, 37(1), 45-59"),
                      hr(),
                      p("The development of this app was supported by a project grant from the",
                        HTML("<a href='https://www.methodologyhubs.mrc.ac.uk/'>MRC Network of Hubs for Trials Methodology Research.</a>")),
                      p("Investigators:",
                        HTML("<a href='http://www.lancaster.ac.uk/maths/about-us/people/lisa-hampson'>Lisa Hampson,</a>"),
                        HTML("<a href='http://www.lancaster.ac.uk/maths/about-us/people/thomas-jaki'>Thomas Jaki,</a>"),
                        HTML("<a href='https://www.mrc-bsu.cam.ac.uk/people/in-alphabetical-order/h-to-m/adrian-mander/'>Adrian Mander,</a>"),
                        HTML("<a href='http://ucl.academia.edu/GrahamWheeler'>Graham Wheeler,</a>"),
                        HTML("<a href='http://www.birmingham.ac.uk/staff/profiles/cancer-genomic/yap-christina.aspx'>Christina Yap,</a>"),
                        HTML("<a href='http://www.ed.ac.uk/cancer-centre/research/clive-group'>Sally Clive</a>")),
                      p("Programmers:",
                        HTML("<a href='http://www.cardiff.ac.uk/people/view/975927-pallmann-philip'>Philip Pallmann,</a>"),
                        HTML("<a href='http://www.lancaster.ac.uk/maths/about-us/people/fang-wan'>Fang Wan</a>")),
                      hr(),
                      p("This app was built with",
                        HTML("<a href='https://cran.r-project.org/'>R version 3.4.0</a>"),
                        "using the add-on packages", 
                        HTML("<a href='http://shiny.rstudio.com/'>shiny,</a>"),
                        HTML("<a href='https://ebailey78.github.io/shinyBS/'>shinyBS,</a>"),
                        HTML("<a href='https://yihui.name/knitr/'>knitr,</a>"),
                        HTML("<a href='http://jrowen.github.io/rhandsontable/'>rhandsontable,</a>"),
                        "and",
                        HTML("<a href='https://rstudio.github.io/DT/'>DT.</a>")),
                      p("Feedback and bug reports welcome, please email to: ",
                        HTML("<a href='mailto:pallmannp@cardiff.ac.uk'>pallmannp@cardiff.ac.uk</a>"))
             )
           )      
    )
    
  )
  
)

###########################################################################################################
###########################################################################################################
###########################################################################################################

port <- data.frame(Cohort = rep(1, 3),
                   Dose = rep(1.5, 3),
                   Event = c(FALSE, FALSE, TRUE))

###########################################################################################################
###########################################################################################################
###########################################################################################################

server <- function(input, output, session){
  
  inDesignFile <- reactive(x={
    inpd <- input$designfile
    if(is.null(inpd)){
      return(NULL)
    }else{
      return(read.csv(file=inpd$datapath, header=TRUE, sep=",", dec="."))
    }
  })
  
  output$DesignTable1 <- renderTable({
    DesignFileIn <- input$designfile
    if(is.null(DesignFileIn)){
      return(NULL)
    }else{
      
      if(ncol(inDesignFile())!=2){
        createAlert(session, anchorId="alertDesign1", alertId="AlertDesign1", title="File not recognised.",
                    content="Please upload a design file created by the 'Design' module. 
                    Do not reformat or otherwise manipulate it.",
                    dismiss=FALSE, append=TRUE)
      }else{
        closeAlert(session, "AlertDesign1")
        
        desfile <- data.frame(rbind(as.numeric(as.vector(inDesignFile()[2, 2])),
                                    as.numeric(as.vector(inDesignFile()[3, 2])),
                                    as.numeric(as.vector(inDesignFile()[11, 2])),
                                    paste(as.character(as.numeric(as.vector(inDesignFile()[16:nrow(inDesignFile()), 2]))), collapse=", "),
                                    as.numeric(as.vector(inDesignFile()[10, 2])),
                                    as.vector(inDesignFile()[1, 2]),
                                    as.logical(as.vector(inDesignFile()[12, 2])),
                                    as.logical(as.vector(inDesignFile()[13, 2])),
                                    as.logical(as.vector(inDesignFile()[14, 2])),
                                    as.numeric(as.vector(inDesignFile()[15, 2]))))
        rownames(desfile) <- c("Maximum number of patients", "Patients per cohort", "Target toxicity level",
                               "Dose levels", "Accurary for stopping", "Gain function", "Always start at lowest dose",
                               "No skipping over doses", "No escalating after toxicity", "Maximum consecutive patients at a dose")
        colnames(desfile) <- NULL
        return(desfile)
        
      }
      
    }
  }, colnames=TRUE, rownames=TRUE)
  
  output$DesignTable2 <- renderTable({
    DesignFileIn <- input$designfile
    if(is.null(DesignFileIn)){
      return(NULL)
    }else{
      
      if(ncol(inDesignFile())==2){
        
        desfilep <- data.frame(rbind(c(as.numeric(as.vector(inDesignFile()[5, 2])),
                                       as.numeric(as.vector(inDesignFile()[8, 2]))),
                                     c(as.numeric(as.vector(inDesignFile()[6, 2])),
                                       as.numeric(as.vector(inDesignFile()[9, 2]))),
                                     c(as.numeric(as.vector(inDesignFile()[4, 2])),
                                       as.numeric(as.vector(inDesignFile()[7, 2])))))
        rownames(desfilep) <- c("Dose", "Toxicity rate", "Pseudo-observations")
        colnames(desfilep) <- c("Lower dose", "Higher dose")
        return(desfilep)
        
      }
      
    }
  }, colnames=TRUE, rownames=TRUE)
  
  output$DesignText1 <- renderText({
    DesignFileIn <- input$designfile
    if(is.null(DesignFileIn)){
      return(NULL)
    }else{
      if(ncol(inDesignFile())==2){
        "Design parameters:"
      }
    }
  })
  
  output$DesignText2 <- renderText({
    DesignFileIn <- input$designfile
    if(is.null(DesignFileIn)){
      return(NULL)
    }else{
      if(ncol(inDesignFile())==2){
        "Prior information:"
      }
    }
  })
  
  values <- reactiveValues(hot=port)
  
  output$hot <- renderRHandsontable({
    
    if(input$howtoenter==FALSE){
      
      return(NULL)
      
    }else{
      
      DT <- NULL
      if(!is.null(input$hot)){
        DT <- setDT(hot_to_r(input$hot))
        values[["hot"]] <- DT
      }else{
        if(!is.null(values[["hot"]])){
          DT <- values[["hot"]]
        }
      }
      if(!is.null(DT)){
        rhandsontable(DT) %>%
          hot_cols(columnSorting=list(column=(which(names(DT)=="Cohort")), sortOrder=TRUE))
      }
      
    }
    
  })
  
  numcnames <- reactive(x={
    
    if(input$howtoenter==FALSE){
      
      inFile <- input$file
      if(is.null(inFile)){
        numcnames <- " "
      }else{
        dats <- read.csv(file=inFile$datapath, header=input$head, sep=input$sep, dec=input$dec)
        cnum <- unlist(lapply(X=as.list(dats), FUN=function(x){is.numeric(x) | is.integer(x)}))
        if(!any(cnum)){
          createAlert(session, anchorId="alertDataset0", alertId="AlertDataset0", title="Error",
                      content="The dataset contains no column with numeric or integer variables.",
                      dismiss=FALSE, append=TRUE)
        }else{
          closeAlert(session, "AlertDataset0")
          numcnames <- names(dats)[cnum]
        }
      }
      
    }
    
  })
  
  output$cohort <- renderUI(expr={
    selectInput("cohortchoice", label="Cohort variable", choices=c(numcnames()), multiple=FALSE)    
  })
  
  output$dose <- renderUI(expr={
    selectInput("dosechoice", label="Dose variable", choices=c(numcnames()), multiple=FALSE)    
  })
  
  output$outcome <- renderUI(expr={
    selectInput("outcomechoice", label="Response variable", choices=c(numcnames()), multiple=FALSE)    
  })
  
  CCCohort <- reactive(x={
    inFile <- input$file
    if(is.null(inFile)){
      CCCohort <- " "
    }else{
      dats <- read.csv(file=inFile$datapath, header=input$head, sep=input$sep, dec=input$dec)
      CCCohort <- dats[, input$cohortchoice]
    }
  })
  
  DDDose <- reactive(x={
    inFile <- input$file
    if(is.null(inFile)){
      DDDose <- " "
    }else{
      dats <- read.csv(file=inFile$datapath, header=input$head, sep=input$sep, dec=input$dec)
      DDDose <- dats[, input$dosechoice]
    }
  })
  
  OOOutcome <- reactive(x={
    inFile <- input$file
    if(is.null(inFile)){
      OOOutcome <- " "
    }else{
      dats <- read.csv(file=inFile$datapath, header=input$head, sep=input$sep, dec=input$dec)
      OOOutcome <- dats[, input$outcomechoice]
    }
  })
  
  output$DatasetTable <- renderDataTable({
    
    if(input$howtoenter==FALSE){
      
      inFile <- input$file
      if(is.null(inFile)){
        return(NULL)
      }else{
        return(read.csv(file=inFile$datapath, header=input$head, sep=input$sep, dec=input$dec))
      }
      
    }else{
      
      inFile <- values[["hot"]]
      
    }
    
  })
  
  output$DatasetText <- renderText({
    
    DesignFileIn <- input$designfile
    
    if(input$howtoenter==FALSE){
      FileIn <- input$file
    }else{
      FileIn <- values[["hot"]]
    }
    
    if(is.null(DesignFileIn) | is.null(FileIn)){
      return(NULL)
    }else{
      "Here are plots of the study data: doses administered and (non-)toxicities observed for 
      individual patients (left), and how often each dose was administered (right)."
    }
    })
  
  output$DatasetPlot <- renderPlot({
    
    DesignFileIn <- input$designfile
    
    if(input$howtoenter==FALSE){
      FileIn <- input$file
    }else{
      FileIn <- values[["hot"]]
    }
    
    if(is.null(DesignFileIn) | is.null(FileIn)){
      return(NULL)
    }else{
      
      doses <- as.numeric(as.vector(inDesignFile()[16:nrow(inDesignFile()), 2]))
      
      if(input$howtoenter==FALSE){
        dats <- read.csv(file=FileIn$datapath, header=input$head, sep=input$sep, dec=input$dec)
        newcohorts <- dats[, input$cohortchoice]
        newdoses <- dats[, input$dosechoice]
        newresponses <- dats[, input$outcomechoice]
      }else{
        newcohorts <- FileIn$Cohort
        newdoses <- FileIn$Dose
        newresponses <- FileIn$Event
      }
      
      if(all(levels(as.factor(newdoses)) %in% levels(as.factor(doses)))==FALSE){
        
        createAlert(session, anchorId="alertDataset1", alertId="AlertDataset1", title="Warning",
                    content="The dataset contains doses not among those in the specified dose set.",
                    dismiss=TRUE, append=TRUE)
      }else{
        closeAlert(session, "AlertDataset1")
      }
      
      par(mfrow=c(1, 2), cex=1, las=1)
      
      plot(NULL, xlim=c(1, length(newcohorts)), ylim=range(doses), xlab="Patient", ylab="Dose",
           main="Toxicities Observed")
      null <- sapply(1:length(newcohorts), function(n){
        points(x=n, y=newdoses[n], pch=newresponses[n] + 21, bg=newresponses[n])
      })
      legend(x=1, y=max(doses), legend=c("non-toxic", "toxic"), pch=c(21, 22), pt.bg=c(0, 1), bty="n")
      
      barplot(table(factor(newdoses, levels=levels(as.factor(newdoses)))), xlab="Dose", ylab="Patients",
              main="Doses Administered")
      
    }
    
  })
  
  makeStuff <- reactive(x={
    
    DesignFileIn <- input$designfile
    
    if(input$howtoenter==FALSE){
      FileIn <- input$file
    }else{
      FileIn <- values[["hot"]]
    }
    
    if(is.null(DesignFileIn) | is.null(FileIn)){
      return(NULL)
    }else{
    
      gainfunction <- as.vector(inDesignFile()[1, 2])
      samplesize <- as.numeric(as.vector(inDesignFile()[2, 2]))
      cohortsize <- as.numeric(as.vector(inDesignFile()[3, 2]))
      samplesizeA <- as.numeric(as.vector(inDesignFile()[4, 2]))
      doseA <- as.numeric(as.vector(inDesignFile()[5, 2]))
      eventrateA <- as.numeric(as.vector(inDesignFile()[6, 2]))
      samplesizeB <- as.numeric(as.vector(inDesignFile()[7, 2]))
      doseB <- as.numeric(as.vector(inDesignFile()[8, 2]))
      eventrateB <- as.numeric(as.vector(inDesignFile()[9, 2]))
      cstop <- as.numeric(as.vector(inDesignFile()[10, 2]))
      targetlevel <- as.numeric(as.vector(inDesignFile()[11, 2]))
      lowstart <- as.logical(as.vector(inDesignFile()[12, 2]))
      noskip <- as.logical(as.vector(inDesignFile()[13, 2]))
      notoxesc <- as.logical(as.vector(inDesignFile()[14, 2]))
      maxseq <- as.numeric(as.vector(inDesignFile()[15, 2]))
      doses <- as.numeric(as.vector(inDesignFile()[16:nrow(inDesignFile()), 2]))
      
      if(input$howtoenter==FALSE){
        dats <- read.csv(file=FileIn$datapath, header=input$head, sep=input$sep, dec=input$dec)
        newcohorts <- dats[, input$cohortchoice]
        newdoses <- dats[, input$dosechoice]
        newresponses <- dats[, input$outcomechoice]
      }else{
        newcohorts <- FileIn$Cohort
        newdoses <- FileIn$Dose
        newresponses <- FileIn$Event
      }
      
      dose_resp <- dose_escalation(r = targetlevel,
                                   prior = matrix(data=c(samplesizeA, samplesizeB, doseA, doseB,
                                                         eventrateA, eventrateB), ncol=3),
                                   dose = newdoses,
                                   response = newresponses,
                                   dose_set = doses,
                                   sample_size = samplesize,
                                   next_cohortsize = cohortsize,
                                   cstop = cstop,
                                   allocation_rule = gainfunction,
                                   prior_type = NULL,
                                   lowstart = lowstart,
                                   noskip = noskip,
                                   notoxesc = notoxesc,
                                   maxseq = maxseq)
      
      print(dose_resp)
      
    }
    
  })
  
  output$RecommendationText1 <- renderUI({  
    makeStuff()
  })
  
  output$RecommendationText2 <- renderText({
    
    DesignFileIn <- input$designfile
    
    if(input$howtoenter==FALSE){
      FileIn <- input$file
    }else{
      FileIn <- values[["hot"]]
    }
    
    if(is.null(DesignFileIn) | is.null(FileIn)){
      return(NULL)
    }else{
      "The logistic model used to describe the dose-toxicity relationship has the form 
      logit(P(toxicity)) = a + b log(dose). The values of the parameters a (intercept) and b (slope) are displayed 
      for the prior and posterior models."
    }
    
  })
  
  output$RecommendationTable <- renderTable({
    
    DesignFileIn <- input$designfile
    
    if(input$howtoenter==FALSE){
      FileIn <- input$file
    }else{
      FileIn <- values[["hot"]]
    }
    
    if(is.null(DesignFileIn) | is.null(FileIn)){
      return(NULL)
    }else{
      
      gainfunction <- as.vector(inDesignFile()[1, 2])
      samplesize <- as.numeric(as.vector(inDesignFile()[2, 2]))
      cohortsize <- as.numeric(as.vector(inDesignFile()[3, 2]))
      samplesizeA <- as.numeric(as.vector(inDesignFile()[4, 2]))
      doseA <- as.numeric(as.vector(inDesignFile()[5, 2]))
      eventrateA <- as.numeric(as.vector(inDesignFile()[6, 2]))
      samplesizeB <- as.numeric(as.vector(inDesignFile()[7, 2]))
      doseB <- as.numeric(as.vector(inDesignFile()[8, 2]))
      eventrateB <- as.numeric(as.vector(inDesignFile()[9, 2]))
      cstop <- as.numeric(as.vector(inDesignFile()[10, 2]))
      targetlevel <- as.numeric(as.vector(inDesignFile()[11, 2]))
      lowstart <- as.logical(as.vector(inDesignFile()[12, 2]))
      noskip <- as.logical(as.vector(inDesignFile()[13, 2]))
      notoxesc <- as.logical(as.vector(inDesignFile()[14, 2]))
      maxseq <- as.numeric(as.vector(inDesignFile()[15, 2]))
      doses <- as.numeric(as.vector(inDesignFile()[16:nrow(inDesignFile()), 2]))
      
      if(input$howtoenter==FALSE){
        dats <- read.csv(file=FileIn$datapath, header=input$head, sep=input$sep, dec=input$dec)
        newcohorts <- dats[, input$cohortchoice]
        newdoses <- dats[, input$dosechoice]
        newresponses <- dats[, input$outcomechoice]
      }else{
        newcohorts <- FileIn$Cohort
        newdoses <- FileIn$Dose
        newresponses <- FileIn$Event
      }
      
      dose_resp <- dose_escalation(r = targetlevel,
                                   prior = matrix(data=c(samplesizeA, samplesizeB, doseA, doseB,
                                                         eventrateA, eventrateB), ncol=3),
                                   dose = newdoses,
                                   response = newresponses,
                                   dose_set = doses,
                                   sample_size = samplesize,
                                   next_cohortsize = cohortsize,
                                   cstop = cstop,
                                   allocation_rule = gainfunction,
                                   prior_type = NULL,
                                   lowstart = lowstart,
                                   noskip = noskip,
                                   notoxesc = notoxesc,
                                   maxseq = maxseq)
      
      newdat <- data.frame(newcohorts=newcohorts,
                           newdoses=newdoses,
                           newresponses=newresponses)
      
      ncohorts <- nlevels(as.factor(newcohorts))
      
      thetatrue <- theta_compute(risk_high=eventrateB, risk_low=eventrateA, TD_high=doseB, TD_low=doseA)
      
      prior <- rbind(c(samplesizeA, doseA, eventrateA), c(samplesizeB, doseB, eventrateB))
      
      dodo <- c(rep(prior[, 2], times=prior[, 1]), newdat[, 2]) #NB: Wont work if a non-integer 
      rere <- c(rep(prior[, 3], times=prior[, 1]), newdat[, 3])
      glmfit <- suppressWarnings(glm(rere ~ log(dodo), family="binomial"))
      
      if(dose_resp@text %in% c("Stop recruitment: no safe dose could be identified.",
                               "Stop recruitment: no safe dose could be identified because the slope of the dose-response model was zero.",
                               "Stop recruitment: no safe dose could be identified because the slope of the dose-response model was negative.",
                               "Stop recruitment: the maximum number of patients has been reached.",
                               "Stop recruitment: the maximum number of patients and the required level of accuracy have both been reached.",
                               "Stop recruitment: the required level of accuracy has been reached.") | 
         input$terminate==TRUE){
        
        dod <- newdat[, 2]
        rer <- newdat[, 3]
        glmfi <- suppressWarnings(glm(rer ~ log(dod), family="binomial"))
        
        table <- rbind(thetatrue, glmfit$coef[1:2], glmfi$coef[1:2])
        
        rownames(table) <- c("Prior model", "Posterior model (prior & patient data)", "Final model (patient data only)")
        colnames(table) <- c("Intercept", "Slope")
        
        table
        
      }else{
        
        table <- rbind(thetatrue, glmfit$coef[1:2])
        
        rownames(table) <- c("Prior model", "Posterior model (prior & patient data)")
        colnames(table) <- c("Intercept", "Slope")
        
        table
        
      }
      
    }
    
  }, rownames=TRUE, colnames=TRUE, align='c')
  
  output$RecommendationText3 <- renderText({
    
    DesignFileIn <- input$designfile
    
    if(input$howtoenter==FALSE){
      FileIn <- input$file
    }else{
      FileIn <- values[["hot"]]
    }
    
    if(is.null(DesignFileIn) | is.null(FileIn)){
      return(NULL)
    }else{
      "Here is a plot of the dose-toxicity relationship as implied by the prior information 
      and after updating the model with study data, and the target toxicity level."
    }
    
  })
  
  output$RecommendationPlot <- renderPlot({
    
    DesignFileIn <- input$designfile
    
    if(input$howtoenter==FALSE){
      FileIn <- input$file
    }else{
      FileIn <- values[["hot"]]
    }
    
    if(is.null(DesignFileIn) | is.null(FileIn)){
      return(NULL)
    }else{

      gainfunction <- as.vector(inDesignFile()[1, 2])
      samplesize <- as.numeric(as.vector(inDesignFile()[2, 2]))
      cohortsize <- as.numeric(as.vector(inDesignFile()[3, 2]))
      samplesizeA <- as.numeric(as.vector(inDesignFile()[4, 2]))
      doseA <- as.numeric(as.vector(inDesignFile()[5, 2]))
      eventrateA <- as.numeric(as.vector(inDesignFile()[6, 2]))
      samplesizeB <- as.numeric(as.vector(inDesignFile()[7, 2]))
      doseB <- as.numeric(as.vector(inDesignFile()[8, 2]))
      eventrateB <- as.numeric(as.vector(inDesignFile()[9, 2]))
      cstop <- as.numeric(as.vector(inDesignFile()[10, 2]))
      targetlevel <- as.numeric(as.vector(inDesignFile()[11, 2]))
      lowstart <- as.logical(as.vector(inDesignFile()[12, 2]))
      noskip <- as.logical(as.vector(inDesignFile()[13, 2]))
      notoxesc <- as.logical(as.vector(inDesignFile()[14, 2]))
      maxseq <- as.numeric(as.vector(inDesignFile()[15, 2]))
      doses <- as.numeric(as.vector(inDesignFile()[16:nrow(inDesignFile()), 2]))
      
      if(input$howtoenter==FALSE){
        dats <- read.csv(file=FileIn$datapath, header=input$head, sep=input$sep, dec=input$dec)
        newcohorts <- dats[, input$cohortchoice]
        newdoses <- dats[, input$dosechoice]
        newresponses <- dats[, input$outcomechoice]
      }else{
        newcohorts <- FileIn$Cohort
        newdoses <- FileIn$Dose
        newresponses <- FileIn$Event
      }
      
      dose_resp <- dose_escalation(r = targetlevel,
                                   prior = matrix(data=c(samplesizeA, samplesizeB, doseA, doseB,
                                                         eventrateA, eventrateB), ncol=3),
                                   dose = newdoses,
                                   response = newresponses,
                                   dose_set = doses,
                                   sample_size = samplesize,
                                   next_cohortsize = cohortsize,
                                   cstop = cstop,
                                   allocation_rule = gainfunction,
                                   prior_type = NULL,
                                   lowstart = lowstart,
                                   noskip = noskip,
                                   notoxesc = notoxesc,
                                   maxseq = maxseq)
      
      newdat <- data.frame(newcohorts=newcohorts,
                           newdoses=newdoses,
                           newresponses=newresponses)
      
      ncohorts <- nlevels(as.factor(newcohorts))
      
      doodle <- seq(min(doses), max(doses), length.out=100)
      
      thetatrue <- theta_compute(risk_high=eventrateB, risk_low=eventrateA, TD_high=doseB, TD_low=doseA)
      true_value <- round(exp((log(targetlevel / (1 - targetlevel)) - thetatrue[1]) / thetatrue[2]), 2)
      
      plot(doodle, (1 + exp(-(thetatrue[1] + thetatrue[2] * log(doodle))))^(-1), type="n", xlab="Dose",
           ylab="P(Toxicity)", xlim=range(doses), ylim=c(0, 1), main="Dose-Toxicity Curves", las=1)
      abline(v=doses, col="grey90")
      abline(h=targetlevel, lwd=2, lty=2)
      
      prior <- data.frame(rbind(c(samplesizeA, doseA, eventrateA), c(samplesizeB, doseB, eventrateB)))
      colnames(prior) <- c("obs", "dos", "fai")
      glmfitt <- suppressWarnings(glm(fai ~ log(dos), weights=obs, data=prior, family="binomial"))
      preddata <- with(prior, data.frame(dos=seq(min(doodle), max(doodle), length=100)))
      preds <- predict(glmfitt, newdata=preddata, type="link", se.fit=TRUE)
      upr <- preds$fit + (qnorm(0.975) * preds$se.fit)
      lwr <- preds$fit - (qnorm(0.975) * preds$se.fit)
      fit <- preds$fit
      fit2 <- glmfitt$family$linkinv(fit)
      upr2 <- glmfitt$family$linkinv(upr)
      lwr2 <- glmfitt$family$linkinv(lwr)
      lines(preddata$dos, fit2, col=3, lwd=2)
      lines(preddata$dos, upr2, col=3, lwd=2, lty=3)
      lines(preddata$dos, lwr2, col=3, lwd=2, lty=3)
      #lines(doodle, (1 + exp(-(thetatrue[1] + thetatrue[2] * log(doodle))))^(-1), col=3, lwd=2)
      intcpt <- exp((log(input$target_level / (1 - input$target_level)) - glmfitt$coef[1]) / glmfitt$coef[2])
      
      dodo <- c(rep(prior[, 2], times=prior[, 1]), newdat[, 2]) #NB: Wont work if a non-integer 
      rere <- c(rep(prior[, 3], times=prior[, 1]), newdat[, 3])
      dada <- data.frame(cbind(dodo, rere))
      colnames(dada) <- c("dos", "res")
      glmfit <- suppressWarnings(glm(res ~ log(dos), data=dada, family="binomial"))
      preddata <- with(dada, data.frame(dos=seq(min(doodle), max(doodle), length=100)))
      preds <- predict(glmfit, newdata=preddata, type="link", se.fit=TRUE)
      upr <- preds$fit + (qnorm(0.975) * preds$se.fit)
      lwr <- preds$fit - (qnorm(0.975) * preds$se.fit)
      fit <- preds$fit
      fit2 <- glmfit$family$linkinv(fit)
      upr2 <- glmfit$family$linkinv(upr)
      lwr2 <- glmfit$family$linkinv(lwr)
      lines(preddata$dos, fit2, col=2, lwd=2)
      lines(preddata$dos, upr2, col=2, lwd=2, lty=3)
      lines(preddata$dos, lwr2, col=2, lwd=2, lty=3)
      #glmfit <- suppressWarnings(glm(rere ~ log(dodo), family="binomial"))
      #lines(doodle, (1 + exp(-(glmfit$coef[1] + glmfit$coef[2] * log(doodle))))^(-1), col=2, lwd=2)
      inter <- round(exp((log(targetlevel / (1 - targetlevel)) - glmfit$coef[1]) / glmfit$coef[2]), 2)
      
      if(dose_resp@text %in% c("Stop recruitment: no safe dose could be identified.",
                               "Stop recruitment: no safe dose could be identified because the slope of the dose-response model was zero.",
                               "Stop recruitment: no safe dose could be identified because the slope of the dose-response model was negative.",
                               "Stop recruitment: the maximum number of patients has been reached.",
                               "Stop recruitment: the maximum number of patients and the required level of accuracy have both been reached.",
                               "Stop recruitment: the required level of accuracy has been reached.") | 
         input$terminate==TRUE){
        
        dod <- newdat[, 2]
        rer <- newdat[, 3]
        dad <- data.frame(cbind(dod, rer))
        colnames(dad) <- c("dos", "res")
        glmfi <- suppressWarnings(glm(res ~ log(dos), data=dad, family="binomial"))
        preddata <- with(dad, data.frame(dos=seq(min(doodle), max(doodle), length=100)))
        preds <- predict(glmfi, newdata=preddata, type="link", se.fit=TRUE)
        upr <- preds$fit + (qnorm(0.975) * preds$se.fit)
        lwr <- preds$fit - (qnorm(0.975) * preds$se.fit)
        fit <- preds$fit
        fit2 <- glmfit$family$linkinv(fit)
        upr2 <- glmfit$family$linkinv(upr)
        lwr2 <- glmfit$family$linkinv(lwr)
        lines(preddata$dos, fit2, col=4, lwd=2)
        lines(preddata$dos, upr2, col=4, lwd=2, lty=3)
        lines(preddata$dos, lwr2, col=4, lwd=2, lty=3)
        #lines(doodle, (1 + exp(-(glmfi$coef[1] + glmfi$coef[2] * log(doodle))))^(-1), col=4, lwd=2)
        intermle <- round(exp((log(targetlevel / (1 - targetlevel)) - glmfi$coef[1]) / glmfi$coef[2]), 2)
        
        legend("topleft", lty=c(2, 1, 3, 1, 3, 1, 3), col=c(1, 3, 3, 2, 2, 4, 4),
               legend=c(paste("Target toxicity level (", formatC(targetlevel, format='f', digits=2), ")", sep=""),
                        paste("Prior dose-toxicity estimate (MTD: ", formatC(true_value, format='f', digits=2), ")", sep=""),
                        "95% pointwise confidence band (normal approximation)",
                        paste("Final dose-toxicity estimate based on prior & patient data (MTD: ", formatC(inter, format='f', digits=2), ")", sep=""),
                        "95% pointwise confidence band (normal approximation)",
                        paste("Final dose-toxicity estimate based on patient data only (MTD: ", formatC(intermle, format='f', digits=2), ")", sep=""),
                        "95% pointwise confidence band (normal approximation)"), lwd=2, bty="n")
        
      }else{
        
        legend("topleft", lty=c(2, 1, 3, 1, 3), col=c(1, 3, 3, 2, 2),
               legend=c(paste("Target toxicity level (", formatC(targetlevel, format='f', digits=2), ")", sep=""),
                        paste("Prior dose-toxicity estimate (MTD: ", formatC(true_value, format='f', digits=2), ")", sep=""),
                        "95% pointwise confidence band (normal approximation)",
                        paste("Current dose-toxicity estimate based on prior & patient data (MTD: ", formatC(inter, format='f', digits=2), ")", sep=""),
                        "95% pointwise confidence band (normal approximation)"),
               lwd=2, bty="n")
        
      }
      
    }
    
  })
  
  output$RecommendationTickbox <- renderUI({
    checkboxInput("terminate", label="The study has been stopped, display the final model estimates.")
  })
  
  output$report <- downloadHandler(
    filename = function(){paste("Report", Sys.Date(), ".pdf", sep='')},
    content = function(file){
      out = knit2pdf("ConductReport.Rnw", clean=TRUE)
      file.copy(out, file) # move pdf to file for downloading
    },
    contentType = "application/pdf"
  )
  
  session$onSessionEnded(stopApp)

}

shinyApp(ui=ui, server=server)

# shiny::runApp("C:/Users/pallmann/Desktop/Shiny_Dose_Finding/Conduct_Upload", launch.browser=TRUE)
# rsconnect::deployApp("C:/Users/pallmann/Desktop/Shiny_Dose_Finding/Conduct_Upload")
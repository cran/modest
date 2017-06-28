#library("dichromat")
library("DT")
#library("plyr")
#library("scales")

#library("ggplot2")
library("knitr")
library("shiny")
library("shinyBS")

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
             p("The dataset must be a CSV file that has (at least) three columns: one for
               the cohort, one for the dose, and one for the outcome (0: no toxicity; 1: toxicity)."),
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
                       placement="right", trigger="hover", options=list(container="body"))
             )
           
    ),
    
    column(3,
           
           wellPanel(
             tags$h3("3. Specify the variables"),
             p("A", strong("cohort"), ", a", strong("dose"), ", and an", strong("outcome"), "variable must be specified."),
             #tags$hr(),
             uiOutput(outputId="cohort"),
             uiOutput(outputId="dose"),
             uiOutput(outputId="outcome"),
             bsTooltip("cohort", "Which column in your dataset specifies the cohort?",
                       placement="right", trigger="hover", options=list(container="body")),
             bsTooltip("dose", "Which column in your dataset specifies the dose?",
                       placement="right", trigger="hover", options=list(container="body")),
             bsTooltip("outcome", "Which column in your dataset specifies the outcome (toxicity / no toxicity)?",
                       placement="right", trigger="hover", options=list(container="body"))
           )
           
    ),
    
    column(6,
           
           tabsetPanel(
             tabPanel("Design",
                      hr(),
                      p("Design parameters:"),
                      tableOutput("desf"),
                      hr(),
                      p("Prior opinion:"),
                      tableOutput("desfprior")),
             tabPanel("Dataset",
                      hr(),
                      dataTableOutput("dat"),
                      hr(),
                      p("Here are plots of the study data: doses administered and (non-)toxicities observed for 
                        individual patients (left), and how often each dose was administered (right)."),
                      plotOutput("fourfoldplot"),
                      uiOutput("warn")),
             tabPanel("Recommendation",
                      hr(),
                      h4("Recommendation"),
                      uiOutput("escal"),
                      hr(),
                      h4("Report"),
                      p("Download a pdf report summarising the design, study data, and dose recommendation."),
                      downloadButton("report"),
                      hr(),
                      p("Here is a plot of the dose-toxicity relationship as implied by the prior opinion and after 
                        updating the model with study data, and the target toxicity level."),
                      plotOutput("curveplot")),
             tabPanel("References",
                      hr(),
                      p("This is an implementation of Bayesian dose-escalation as proposed in:"),
                      p("Whitehead J, Brunier H (1995) Bayesian decision procedures for dose determining experiments. 
                        Statistics in Medicine, 14, 885-893."),
                      p("Whitehead J, Williamson D (1998) Bayesian decision procedures based on logistic regression models for dose-finding studies. 
                        Journal of Biopharmaceutical Statistics, 8, 445-467."),
                      #p("Whitehead J (2006) Using Bayesian decision theory in dose-escalation studies. Ch. 7 in: 
                      #  Chevret S (ed.) Statistical Methods for Dose-Finding Experiments. Wiley, Chichester, UK."),
                      p("Zhou Y, Whitehead J (2003) Practical implementation of Bayesian dose-escalation procedures. 
                        Drug Information Journal, 37, 45-59."),
                      hr(),
                      p("The development of this app was supported by a project grant from the",
                        HTML("<a href='https://www.methodologyhubs.mrc.ac.uk/'>MRC Network of Hubs for Trials Methodology Research</a>"),
                        "."),
                      p("Investigators: Lisa Hampson, Thomas Jaki, Adrian Mander, Graham Wheeler, Christina Yap, Sally Clive."),
                      p("Programmers: Philip Pallmann, Fang Wan."),
                      hr(),
                      p("This app was built with",
                        HTML("<a href='https://cran.r-project.org/'>R version 3.1.3</a>"),
                        "using the add-on packages", 
                        HTML("<a href='http://shiny.rstudio.com/'>shiny</a>"),
                        "and",
                        HTML("<a href='https://ebailey78.github.io/shinyBS/'>shinyBS</a>"),
                        ",",
                        HTML("<a href='https://yihui.name/knitr/'>knitr</a>"),
                        ", and",
                        HTML("<a href='https://rstudio.github.io/DT/'>DT</a>"),
                        "."),
                      p("Feedback and bug reports welcome, please email to: ",
                        HTML("<a href='mailto:p.pallmann@lancaster.ac.uk'>p.pallmann@lancaster.ac.uk</a>"),
                        ".")
             )
           )      
    )
    
  )
  
)

server <- function(input, output){
  
  inDesignFile <- reactive(x={
    inpd <- input$designfile
    if(is.null(inpd)){
      return(NULL)
    }else{
      return(read.csv(file=inpd$datapath, header=TRUE, sep=",", dec="."))
    }
  })
  
  output$desf <- renderTable({
    DesignFileIn <- input$designfile
    if(is.null(DesignFileIn)){
      return(NULL)
    }else{
      desfile <- data.frame(rbind(as.numeric(as.vector(inDesignFile()[2, 2])),
                                  as.numeric(as.vector(inDesignFile()[3, 2])),
                                  as.numeric(as.vector(inDesignFile()[11, 2])),
                                  paste(as.character(as.numeric(as.vector(inDesignFile()[15:nrow(inDesignFile()), 2]))), collapse=", "),
                                  as.numeric(as.vector(inDesignFile()[10, 2])),
                                  as.vector(inDesignFile()[1, 2]),
                                  as.logical(as.vector(inDesignFile()[12, 2])),
                                  as.logical(as.vector(inDesignFile()[13, 2])),
                                  as.logical(as.vector(inDesignFile()[14, 2]))))
      rownames(desfile) <- c("Maximum number of patients", "Patients per cohort", "Target toxicity level",
                             "Dose levels", "Accurary for stopping", "Gain function", "Always start at lowest dose",
                             "No skipping over doses", "No escalating after toxicity")
      colnames(desfile) <- NULL
      return(desfile)
    }
  })
  
  output$desfprior <- renderTable({
    DesignFileIn <- input$designfile
    if(is.null(DesignFileIn)){
      return(NULL)
    }else{
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
  })
  
  #inDesignFile <- read.csv(#"C:/Users/pallmann/Desktop/Shiny_Dose_Finding/Conduct/design.csv",
  #                         "design.csv",
  #                         header=TRUE, sep=",", dec=".")
  
  inFile <- reactive(x={
    inpd <- input$file
    if(is.null(inpd)){
      return(NULL)
    }else{
      return(read.csv(file=inpd$datapath, header=input$head, sep=input$sep, dec=input$dec))
    }
  })
  
  output$dat <- renderDataTable({
    FileIn <- input$file
    if(is.null(FileIn)){
      return(NULL)
    }else{
      return(read.csv(file=FileIn$datapath, header=input$head, sep=input$sep, dec=input$dec))
    }
  })
  
  numcnames <- reactive(x={
    if(is.null(inFile())){
      numcnames <- " "
    }else{
      cnum <- unlist(lapply(X=as.list(inFile()), FUN=function(x){is.numeric(x)|is.integer(x)}))
      if(!any(cnum)){stop("The imported data set contains no column with numeric or integer variables!")}
      numcnames <- names(inFile())[cnum]
    }
  })
  
  output$cohort <- renderUI(expr={
    selectInput("cohortchoice", label="Cohort variable", choices=c(numcnames()), multiple=FALSE)    
  })
  
  output$dose <- renderUI(expr={
    selectInput("dosechoice", label="Dose variable", choices=c(numcnames()), multiple=FALSE)    
  })
  
  output$outcome <- renderUI(expr={
    selectInput("outcomechoice", label="Outcome variable", choices=c(numcnames()), multiple=FALSE)    
  })
  
  CCCohort <- reactive(x={
    if(is.null(inFile())){
      CCCohort <- " "
    }else{
      CCCohort <- inFile()[, input$cohortchoice]
    }
  })
  
  DDDose <- reactive(x={
    if(is.null(inFile())){
      DDDose <- " "
    }else{
      DDDose <- inFile()[, input$dosechoice]
    }
  })
  
  OOOutcome <- reactive(x={
    if(is.null(inFile())){
      OOOutcome <- " "
    }else{
      OOOutcome <- inFile()[, input$outcomechoice]
    }
  })
  
  makeStuff <- reactive(x={
    
    if(is.null(inFile()) | is.null(inDesignFile())){
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
      doses <- as.numeric(as.vector(inDesignFile()[15:nrow(inDesignFile()), 2]))
      
      dose_resp <- dose_escalation(r = targetlevel,
                                   prior = matrix(data=c(samplesizeA, samplesizeB, doseA, doseB,
                                                         eventrateA, eventrateB), ncol=3),
                                   dose = inFile()[, input$dosechoice],
                                   response = inFile()[, input$outcomechoice],
                                   dose_set = doses,
                                   sample_size = samplesize,
                                   next_cohortsize = cohortsize,
                                   cstop = cstop,
                                   allocation_rule = gainfunction,
                                   prior_type = NULL,
                                   lowstart = lowstart,
                                   noskip = noskip,
                                   notoxesc = notoxesc)
      
      print(dose_resp)
      
    }
    
  })
  
  output$escal <- renderUI({  
    makeStuff()
  })
  
  makeCurvePlot <- reactive(x={
    
    if(is.null(inFile()) | is.null(inDesignFile())){
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
      doses <- as.numeric(as.vector(inDesignFile()[15:nrow(inDesignFile()), 2]))
      
      dose_resp <- dose_escalation(r = targetlevel,
                                   prior = matrix(data=c(samplesizeA, samplesizeB, doseA, doseB,
                                                         eventrateA, eventrateB), ncol=3),
                                   dose = inFile()[, input$dosechoice],
                                   response = inFile()[, input$outcomechoice],
                                   dose_set = doses,
                                   sample_size = samplesize,
                                   next_cohortsize = cohortsize,
                                   cstop = cstop,
                                   allocation_rule = gainfunction,
                                   prior_type = NULL,
                                   lowstart = lowstart,
                                   noskip = noskip,
                                   notoxesc = notoxesc)
      
      newcohorts <- inFile()[, input$cohortchoice]
      newdoses <- inFile()[, input$dosechoice]
      newresponses <- inFile()[, input$outcomechoice]
      
      newdat <- data.frame(newcohorts=newcohorts,
                           newdoses=newdoses,
                           newresponses=newresponses)
      
      ncohorts <- nlevels(as.factor(newcohorts))
      
      doodle <- seq(min(doses), max(doses), length.out=100)
      
      thetatrue <- theta_compute(risk_high=eventrateB, risk_low=eventrateA, TD_high=doseB, TD_low=doseA)
      true_value <- round(exp((log(targetlevel / (1 - targetlevel)) - thetatrue[1]) / thetatrue[2]), 2)
            
      prior <- rbind(c(samplesizeA, doseA, eventrateA), c(samplesizeB, doseB, eventrateB))
      
      plot(doodle, (1 + exp(-(thetatrue[1] + thetatrue[2] * log(doodle))))^(-1), type="l", xlab="Dose",
           ylab="P(Toxicity)", xlim=range(doses), ylim=c(0, 1), main="Dose-Toxicity Curves", las=1, col=1)
      
      #inter <- numeric(0)
      
      #for(i in 1:ncohorts){
      #  dodo <- c(rep(prior[, 2], times=prior[, 1]), newdat[newdat$newcohorts < (i + 1), 2]) #NB: Wont work if a non-integer 
      #  rere <- c(rep(prior[, 3], times=prior[, 1]), newdat[newdat$newcohorts < (i + 1), 3])
      #  glmfit <- suppressWarnings(glm(rere ~ log(dodo), family="binomial"))
      #  lines(doodle, (1 + exp(-(glmfit$coef[1] + glmfit$coef[2] * log(doodle))))^(-1), col=(i + 1))
      #  inter[i] <- round(exp((log(targetlevel / (1 - targetlevel)) - glmfit$coef[1]) / glmfit$coef[2]), 2)
      #}
      
      dodo <- c(rep(prior[, 2], times=prior[, 1]), newdat[, 2]) #NB: Wont work if a non-integer 
      rere <- c(rep(prior[, 3], times=prior[, 1]), newdat[, 3])
      glmfit <- suppressWarnings(glm(rere ~ log(dodo), family="binomial"))
      lines(doodle, (1 + exp(-(glmfit$coef[1] + glmfit$coef[2] * log(doodle))))^(-1), col=2)
      inter <- round(exp((log(targetlevel / (1 - targetlevel)) - glmfit$coef[1]) / glmfit$coef[2]), 2)
      
      if(dose_resp@text %in% c("Stop recruitment, the required level of accuracy has been reached.",
                               "Stop recruitment, the slope of the dose-response model is negative.",
                               "Stop recruitment, the maximum number of patients has been reached.")){
        
        dod <- newdat[, 2]
        rer <- newdat[, 3]
        glmfi <- suppressWarnings(glm(rer ~ log(dod), family="binomial"))
        lines(doodle, (1 + exp(-(glmfi$coef[1] + glmfi$coef[2] * log(doodle))))^(-1), col=1, lty="dashed")
        intermle <- round(exp((log(targetlevel / (1 - targetlevel)) - glmfi$coef[1]) / glmfi$coef[2]), 2)
        
        legend("topleft", lty=c("dotted", rep("solid", 2), "dashed"), col=c(1, 1, 2, 1),
               legend=c("Target", paste("Prior (", formatC(true_value, format='f', digits=2), ")", sep=""),
                        paste("Final Bayesian estimate (", formatC(inter, format='f', digits=2), ")", sep=""),
                        paste("Final MLE (", formatC(intermle, format='f', digits=2), ")", sep="")), bty="n")
        
      }else{
        
        legend("topleft", lty=c("dotted", rep("solid", 2)), col=c(1, 1, 2),
               legend=c("Target", paste("Prior (", formatC(true_value, format='f', digits=2), ")", sep=""),
                        paste("Current Bayesian estimate (", formatC(inter, format='f', digits=2), ")", sep="")), bty="n")
        
      }
      
      abline(h=targetlevel, col=1, lty="dotted")
      
      #legend("topleft", lty=c("dotted", rep("solid", (i + 1)), "dashed"), col=c(1, 1, 2:(i + 1), 1),
      #       legend=c("Target", paste("Prior (", formatC(true_value, format='f', digits=2), ")", sep=""),
      #                paste("After cohort ", 1:i, " (", formatC(inter[1:i], format='f', digits=2), ")", sep=""),
      #                paste("Final MLE (", formatC(intermle, format='f', digits=2), ")", sep="")), bty="n", cex=0.7)
      
    }
    
  })
  
  output$curveplot <- renderPlot({
    makeCurvePlot()
  }, width=800, height=600)
  
  makeWarn <- reactive(x={
    
    if(is.null(inFile()) | is.null(inDesignFile())){
      return(NULL)
    }else{
      
      doses <- as.numeric(as.vector(inDesignFile()[15:nrow(inDesignFile()), 2]))
      
      newdoses <- inFile()[, input$dosechoice]
      
      if(all(levels(as.factor(newdoses)) %in% levels(as.factor(doses)))==FALSE){
        print("Warning: Data contains doses not in the specified dose set.")
      }else{
        print("")
      }
      
    }
    
  })
  
  output$warn <- renderUI({  
    makeWarn()
  })
  
  makeFourPlots <- reactive(x={
    
    if(is.null(inFile()) | is.null(inDesignFile())){
      return(NULL)
    }else{
      
      doses <- as.numeric(as.vector(inDesignFile()[15:nrow(inDesignFile()), 2]))
      
      newcohorts <- inFile()[, input$cohortchoice]
      newdoses <- inFile()[, input$dosechoice]
      newresponses <- inFile()[, input$outcomechoice]
      
      par(mfrow=c(1, 2), cex=1, las=1)
      
      plot(NULL, xlim=c(1, length(newcohorts)), ylim=range(doses), xlab="Patient", ylab="Dose",
           main="Toxicities Observed")
      null <- sapply(1:length(newcohorts), function(n){
        points(x=n, y=newdoses[n], pch=newresponses[n] + 21, bg=newresponses[n])
      })
      legend(x=1, y=max(doses), legend=c("non-toxic", "toxic"), pch=c(21, 22), pt.bg=c(0, 1), bty="n")
      
      barplot(table(factor(newdoses, levels=levels(as.factor(newdoses)))), xlab="Dose", ylab="Patients",
              main="Doses Administered")
      
      #plot(x@recs$round, x@recs$pme_estimate, type="l",
      #     ylim=c(min(x@recs$lower, x@true_value), max(x@recs$upper, x@true_value)),
      #     xlab="Cohort", ylab="Estimate", main="Estimates")
      #lines(x@recs$round, x@recs$lower, type="l", lty=2)
      #lines(x@recs$round, x@recs$upper, type="l", lty=2)
      #abline(h=x@true_value, col=2)
      
      #plot(x@recs$round, x@recs$pme_estimate, type="l",
      #     ylim=c(min(x@dose_set, x@true_value, x@recs$round), max(x@dose_set, x@true_value, x@recs$round)),
      #     xlab="Cohort", ylab="Estimate", main="Estimates (zoomed in)")
      #lines(x@recs$round, x@recs$lower, type="l", lty=2)
      #lines(x@recs$round, x@recs$upper, type="l", lty=2)
      #abline(h=x@true_value, col=2)
      
    }
    
  })
  
  output$fourfoldplot <- renderPlot({  
    makeFourPlots()
  })
  
  output$report <- downloadHandler(
    filename = function(){paste("Report", Sys.Date(), ".pdf", sep='')},
    content = function(file){
      out = knit2pdf("report.Rnw", clean=TRUE)
      file.copy(out, file) # move pdf to file for downloading
    },
    contentType = "application/pdf"
  )

}

shinyApp(ui=ui, server=server)

# shiny::runApp("C:/Users/pallmann/Desktop/Shiny_Dose_Finding/Conduct_Upload", launch.browser=TRUE)
# rsconnect::deployApp("C:/Users/pallmann/Desktop/Shiny_Dose_Finding/Conduct_Upload")
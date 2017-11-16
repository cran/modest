library("knitr")
library("shiny")
library("shinyBS")

#library("DT")

#source("../mainDoseEscalation.R")
source("mainDoseEscalation.R")

ui <- fluidPage(
  
  img(src="MRC.png", height="100", align="right"),
  titlePanel("Designing a Model-Based Dose-Escalation Study"),
  h5("Philip Pallmann & Fang Wan"),
  h6("Department of Mathematics & Statistics, Lancaster University, UK"),
  
  fluidRow(
    
    column(3,
           
           wellPanel(
             h3("1. Basic settings"),
             p("Specify some key parameters of your study."),
             hr(),
             numericInput("n_patients", "Maximum number of patients:", 30, min=1, step=1, width=220),
             numericInput("cohort_size", "Patients per cohort:", 3, min=1, step=1, width=220),
             sliderInput("target_level", "Target toxicity level:", min=0.01, max=0.5, value=0.3, step=0.01),
             textInput("doses", "Doses (comma-separated):", "1, 1.5, 2, 2.5, 3"),
             selectInput("gainfunction", "Gain function:", width=150,
                         list("Patient gain"="PatientGain", "Variance gain"="VarianceGain")),
             bsTooltip("n_patients", "How many patients are you willing to recruit at most?",
                       placement="right", trigger="hover", options=list(container="body")),
             bsTooltip("cohort_size", "How many patients should enter the study at the same time?",
                       placement="right", trigger="hover", options=list(container="body")),
             bsTooltip("target_level", "What proportion of DTLs are you targeting?",
                       placement="right", trigger="hover", options=list(container="body")),
             bsTooltip("doses", "Which doses would you like to investigate?",
                       placement="right", trigger="hover", options=list(container="body")),
             bsTooltip("gainfunction", "What gain function would you like to use? (patient gain: recommend dose with posterior response probability closest to the target toxicity level; variance gain: recommend dose to maximise learning about the dose-response relationship)",
                       placement="right", trigger="hover", options=list(container="body"))
             ),
           
           wellPanel(
             h3("3. Simulation model"),
             p("Specify the 'true' dose-toxicity relationship for simulation 
               in terms of toxicity rates for two distinct doses."),
             hr(),
             
             #checkboxInput("howto", "Specify model via intercept and slope"),
             #bsTooltip("howto", "Tick here if you want to specify the simulation model in terms of its intercept and slope rather than via the toxicity rates at two doses.",
             #          placement="right", trigger="hover", options=list(container="body")),
             #radioButtons(inputId="howto", label="How to specify true model",
             #             choices=c("Two doses"="TRUE", "Intercept slope"="FALSE"),
             #             selected="TRUE", inline=TRUE),
            # conditionalPanel(
            #   condition="input.howto == false",
               fluidRow(
                 column(6,
                        h4("Lower dose"),
                        hr(),
                        numericInput("dose1true", "Dose:", 1),
                        sliderInput("failrate1true", "Toxicity rate:", min=0.01, max=0.99, value=0.1, step=0.01),
                        bsTooltip("dose1true", "What is your lower dose?",
                                  placement="right", trigger="hover", options=list(container="body")),
                        bsTooltip("failrate1true", "What is the toxicity rate for your lower dose?",
                                  placement="right", trigger="hover", options=list(container="body"))
                 ),
                 column(6,
                        h4("Higher dose"),
                        hr(),
                        numericInput("dose2true", "Dose:", 3),
                        sliderInput("failrate2true", "Toxicity rate:", min=0.01, max=0.99, value=0.4, step=0.01),
                        bsTooltip("dose2true", "What is your higher dose?",
                                  placement="right", trigger="hover", options=list(container="body")),
                        bsTooltip("failrate2true", "What is the toxicity rate for your higher dose?",
                                  placement="right", trigger="hover", options=list(container="body"))
                 )
               )
             #),
             #conditionalPanel(
            #   condition="input.howto == true",
            #   numericInput("theta1", "Model intercept:", -2.20, width=120),
            #   numericInput("theta2", "Model slope:", 1.63, width=120),
            #   bsTooltip("theta1", "Adjust the intercept of the true model here.",
            #             placement="right", trigger="hover", options=list(container="body")),
            #   bsTooltip("theta2", "Adjust the slope of the true model here.",
            #             placement="right", trigger="hover", options=list(container="body"))
            # )
           ),
           
           wellPanel(
             h3("5. Simulations"),
             p("Specify the simulation settings. The six scenarios are shown under the 'Scenarios' tab."),
             hr(),
             
             radioButtons(inputId="whatscen", label="Scenario:",
                          choices=c("Standard"="Standard", "Potent"="Potent", "Inactive"="Inactive",
                                    "Steep"="Steep", "Very potent"="Very potent", "Very inactive"="Very inactive"),
                          selected="Standard", inline=TRUE),
             bsTooltip("whatscen", "Under what scenario would you like to run the simulations?",
                       placement="right", trigger="hover", options=list(container="body")),
             
             
             numericInput("num_sim", "Repetitions:", 1000, min=10, max=10000, step=1, width=100),
             actionButton("runsim", "Run simulations"),
             bsTooltip("num_sim", "How many simulations would you like to run?",
                       placement="right", trigger="hover", options=list(container="body"))
           )
           
    ),
    
    column(3,
           
           wellPanel(
             h3("2. Prior information"),
             p("Specify your prior opinion about the toxicity rates for two distinct doses, 
               and the strength of your opinion in terms of pseudo-observations."),
             hr(),
             fluidRow(
               column(6,
                      h4("Lower dose"),
                      hr(),
                      numericInput("dose1", "Dose:", 1),
                      sliderInput("failrate1", "Toxicity rate:", min=0.01, max=0.99, value=0.2, step=0.01),
                      numericInput("obs1", "Pseudo-observations:", 3, min=1, max=100, step=1)
                      ),
               column(6,
                      h4("Higher dose"),
                      hr(),
                      numericInput("dose2", "Dose:", 3),
                      sliderInput("failrate2", "Toxicity rate:", min=0.01, max=0.99, value=0.5, step=0.01),
                      numericInput("obs2", "Pseudo-observations:", 3, min=1, max=100, step=1)
                      )
             ),
             bsTooltip("dose1", "What is your lower dose?",
                       placement="right", trigger="hover", options=list(container="body")),
             bsTooltip("failrate1", "What is your prior opinion about the toxicity rate for your lower dose?",
                       placement="right", trigger="hover", options=list(container="body")),
             bsTooltip("obs1", "How many pseudo-observations should your prior opinion about your lower dose be worth?",
                       placement="right", trigger="hover", options=list(container="body")),
             bsTooltip("dose2", "What is your higher dose?",
                       placement="right", trigger="hover", options=list(container="body")),
             bsTooltip("failrate2", "What is your prior opinion about the toxicity rate for your higher dose?",
                       placement="right", trigger="hover", options=list(container="body")),
             bsTooltip("obs2", "How many pseudo-observations should your prior opinion about your higher dose be worth?",
                       placement="right", trigger="hover", options=list(container="body"))
           ),
           
           wellPanel(
             h3("4. Escalation & stopping rules"),
             p("Specify rules for dose escalation and stopping the study."),
             hr(),
             checkboxInput("lowstart", "Always start at the lowest dose", value=TRUE),
             checkboxInput("noskip", "Don't skip over any doses when escalating", value=TRUE),
             checkboxInput("notoxesc", "Don't escalate upon observing a toxicity", value=FALSE),
             checkboxInput("consec", "Stop after a given number of consecutive patients at the same dose"),
             conditionalPanel(
               condition="input.consec == true",
               numericInput("consecutive", "Number of patients:", 9, min=1, max=100, step=1, width=150)
             ),
             sliderInput("cstop", "Accuracy for stopping:", min=1, max=7, value=1.5, step=0.1),
             bsTooltip("lowstart", "Tick here if you want to start from the lowest dose.",
                       placement="right", trigger="hover", options=list(container="body")),
             bsTooltip("noskip", "Tick here if you do not want to skip doses when escalating.",
                       placement="right", trigger="hover", options=list(container="body")),
             bsTooltip("notoxesc", "Tick here if you do not want to escalate if one or more toxicities were observed in the previous cohort.",
                       placement="right", trigger="hover", options=list(container="body")),
             bsTooltip("consec", "Tick here if you want to specify a maximum number of consecutive patients that can be given the same dose before stopping the trial.",
                       placement="right", trigger="hover", options=list(container="body")),
             bsTooltip("consecutive", "What should be the maximum number of consecutive patients to be given the same dose?",
                       placement="right", trigger="hover", options=list(container="body")),
             bsTooltip("cstop", "What level of precision for the estimated dose (ratio lower/upper 95% confidence bound) would you consider sufficient for stopping? Setting this to 1 prevents early stopping for precision.",
                       placement="right", trigger="hover", options=list(container="body"))
           )
           
    ),
    
    column(6,
           tabsetPanel(
             tabPanel("Model",
                      hr(),
                      bsAlert("alertModel1"),
                      bsAlert("alertModel2"),
                      bsAlert("alertModel3"),
                      bsAlert("alertSimulationsX"),
                      textOutput("ModelText1"),
                      tableOutput("ModelTable"),
                      hr(),
                      textOutput("ModelText2"),
                      plotOutput("ModelPlot", height=600, width=800)),
             tabPanel("Example",
                      hr(),
                      bsAlert("alertExample1"),
                      bsAlert("alertExample2"),
                      bsAlert("alertExample3"),
                      bsAlert("alertSimulationsX"),
                      textOutput("ExampleText"),
                      plotOutput("ExamplePlot", height=800, width=800),
                      actionButton("anotherrun", "Show another run")),
             tabPanel("Scenarios",
                      hr(),
                      bsAlert("alertScenarios1"),
                      bsAlert("alertScenarios2"),
                      bsAlert("alertScenarios3"),
                      bsAlert("alertSimulationsX"),
                      textOutput("ScenariosText1"),
                      tableOutput("ScenariosTable"),
                      hr(),
                      textOutput("ScenariosText2"),
                      plotOutput("ScenariosPlot", height=600, width=800)),
             tabPanel("Simulations",
                      hr(),
                      bsAlert("alertSimulations0"),
                      bsAlert("alertSimulations1"),
                      bsAlert("alertSimulations2"),
                      bsAlert("alertSimulations3"),
                      bsAlert("alertSimulationsX"),
                      textOutput("SimulationsText1"),
                      p(""),
                      tableOutput("SimulationsTable1"),
                      hr(),
                      textOutput("SimulationsText2"),
                      p(""),
                      tableOutput("SimulationsTable2"),
                      h6(textOutput("SimulationsText2x")),
                      #hr(),
                      #textOutput("SimulationsText3"),
                      #p(""),
                      #tableOutput("SimulationsTable3"),
                      hr(),
                      textOutput("SimulationsText4"),
                      plotOutput("SimulationsPlot", width=800, height=800),
                      hr(),
                      textOutput("SimulationsText5"),
                      p(""),
                      tableOutput("SimulationsTable4")),
             tabPanel("Downloads",
                      hr(),
                      h4("Design"),
                      p("Download a csv file containing the design parameters as they will be used for the study."),
                      downloadButton("design"),
                      hr(),
                      h4("Report"),
                      p("Download a pdf report summarising the input parameters and key characteristics of the design."),
                      downloadButton("report"),
                      hr(),
                      h4("Simulations"),
                      p("Download a csv file of the detailed simulation results under the current scenario."),
                      downloadButton("simulations")),
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
                        "and",
                        HTML("<a href='https://yihui.name/knitr/'>knitr.</a>")),
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

server <- function(input, output, session){
  
  output$ModelText1 <- renderText({
    
    doo <- as.numeric(unlist(strsplit(input$doses, ",")))
    
    if(is.na(sum(as.numeric(unlist(strsplit(input$doses, ",")))))==TRUE){
      createAlert(session, anchorId="alertModel1", alertId="AlertModel1", title="Error",
                  content="Your doses must be numeric values separated by commas.",
                  dismiss=FALSE, append=TRUE)
    }else{
      closeAlert(session, "AlertModel1")
      if(input$dose1 < min(doo)){
        createAlert(session, anchorId="alertModel2", alertId="AlertModel2", title="Error",
                    content="Your lower dose is outside your interesting dose range.",
                    dismiss=FALSE, append=TRUE)
      }else{
        closeAlert(session, "AlertModel2")
        if(input$dose2 > max(doo)){
          createAlert(session, anchorId="alertModel3", alertId="AlertModel3", title="Error",
                      content="Your higher dose is outside your interesting dose range.",
                      dismiss=FALSE, append=TRUE)
        }else{
          closeAlert(session, "AlertModel3")
          
          "The logistic model used to describe the dose-toxicity relationship has the form 
          logit(P(toxicity)) = a + b log(dose). The values of the parameters a (intercept) and b (slope) are displayed 
          for the simulation model (representing the assumed true dose-toxicity relationship) and for the model 
          based on the prior information."
        }
        
      }
      
    }
    
  })
  
  output$ModelTable <- renderTable({
    
    doo <- as.numeric(unlist(strsplit(input$doses, ",")))
    
    if(is.na(sum(as.numeric(unlist(strsplit(input$doses, ",")))))==FALSE &
       input$dose1 >= min(doo) &
       input$dose2 <= max(doo)){
      
      #if(input$howto==TRUE){
      #  thetatrue <- c(input$theta1, input$theta2)
      #}else{
        thetatrue <- theta_compute(risk_high=input$failrate2true, risk_low=input$failrate1true,
                                   TD_high=input$dose2true, TD_low=input$dose1true)
      #}
      
      prior <- data.frame(rbind(c(input$obs1, input$dose1, input$failrate1), c(input$obs2, input$dose2, input$failrate2)))
      colnames(prior) <- c("obs", "dos", "fai")
      glmfit <- suppressWarnings(glm(fai ~ log(dos), weights=obs, data=prior, family="binomial"))
      
      table <- rbind(thetatrue, glmfit$coef[1:2])
      
      rownames(table) <- c("Simulation model", "Prior model")
      colnames(table) <- c("Intercept", "Slope")
      
      table
      
    }
    
  }, rownames=TRUE, colnames=TRUE, align='c')
  
  output$ModelText2 <- renderText({
    
    doo <- as.numeric(unlist(strsplit(input$doses, ",")))
    
    if(is.na(sum(as.numeric(unlist(strsplit(input$doses, ",")))))==FALSE &
       input$dose1 >= min(doo) &
       input$dose2 <= max(doo)){
      "Here is a plot of the simulation model, the dose-toxicity relationship implied by the prior information, 
      and the target toxicity level. The target dose is the dose for which, under the true model, the toxicity 
      rate is equal to the target level."
    }
    })
  
  output$ModelPlot <- renderPlot({
    
    doo <- as.numeric(unlist(strsplit(input$doses, ",")))
    
    if(is.na(sum(as.numeric(unlist(strsplit(input$doses, ",")))))==FALSE &
       input$dose1 >= min(doo) &
       input$dose2 <= max(doo)){
      
      doodle <- seq(min(doo), max(doo), length.out=100)
      
      #if(input$howto==TRUE){
      #  thetatrue <- c(input$theta1, input$theta2)
      #}else{
        thetatrue <- theta_compute(risk_high=input$failrate2true, risk_low=input$failrate1true,
                                   TD_high=input$dose2true, TD_low=input$dose1true)
      #}
      
      true_value <- exp((log(input$target_level / (1 - input$target_level)) - thetatrue[1]) / thetatrue[2])
      plot(doodle, (1 + exp(-(thetatrue[1] + thetatrue[2] * log(doodle))))^(-1), type="n", xlab="Dose",
           ylab="P(Toxicity)", xlim=range(doo), ylim=c(0, 1), main="Dose-Toxicity Curves", las=1)
      abline(v=doo, col="grey90")
      #abline(v=true_value, col="grey80", lty=2)
      abline(h=input$target_level, lwd=2, lty=2)
      lines(doodle, (1 + exp(-(thetatrue[1] + thetatrue[2] * log(doodle))))^(-1), type="l", col=4, lwd=2)
      
      prior <- data.frame(rbind(c(input$obs1, input$dose1, input$failrate1), c(input$obs2, input$dose2, input$failrate2)))
      colnames(prior) <- c("obs", "dos", "fai")
      glmfit <- suppressWarnings(glm(fai ~ log(dos), weights=obs, data=prior, family="binomial"))
      preddata <- with(prior, data.frame(dos=seq(min(doodle), max(doodle), length=100)))
      preds <- predict(glmfit, newdata=preddata, type="link", se.fit=TRUE)
      upr <- preds$fit + (qnorm(0.975) * preds$se.fit)
      lwr <- preds$fit - (qnorm(0.975) * preds$se.fit)
      fit <- preds$fit
      fit2 <- glmfit$family$linkinv(fit)
      upr2 <- glmfit$family$linkinv(upr)
      lwr2 <- glmfit$family$linkinv(lwr)
      lines(preddata$dos, fit2, col=3, lwd=2)
      lines(preddata$dos, upr2, col=3, lwd=2, lty=3)
      lines(preddata$dos, lwr2, col=3, lwd=2, lty=3)
      intcpt <- exp((log(input$target_level / (1 - input$target_level)) - glmfit$coef[1]) / glmfit$coef[2])
      #prior <- rbind(c(input$obs1, input$dose1, input$failrate1), c(input$obs2, input$dose2, input$failrate2))
      #glmfit <- suppressWarnings(glm(prior[, 3] ~ log(prior[, 2]), weights=prior[, 1], family="binomial"))
      #theta1p <- glmfit$coef[1]
      #theta2p <- glmfit$coef[2]
      #lines(doodle, (1 + exp(-(theta1p + theta2p * log(doodle))))^(-1), col=3, lwd=2, lty=3)
      points(input$dose1, input$failrate1, pch="+", cex=2, col=3)
      points(input$dose2, input$failrate2, pch="+", cex=2, col=3)
      legend("topleft", lty=c(2, 1, 1, 3), lwd=2, col=c(1, 4, 3, 3),
             legend=c(paste("Target toxicity level (", formatC(input$target_level, format='f', digits=2), ")", sep=""),
                      paste("Assumed true dose-toxicity relationship (target dose: ", formatC(true_value, format='f', digits=2), ")", sep=""),
                      paste("Prior dose-toxicity estimate (MTD: ", formatC(intcpt, format='f', digits=2), ")", sep=""),
                      "95% pointwise confidence band (normal approximation)"), bty="n")
      
    }
    
  })
  
  output$ExampleText <- renderText({
    
    doo <- as.numeric(unlist(strsplit(input$doses, ",")))
    
    if(is.na(sum(as.numeric(unlist(strsplit(input$doses, ",")))))==TRUE){
      createAlert(session, anchorId="alertExample1", alertId="AlertExample1", title="Error",
                  content="Your doses must be numeric values separated by commas.",
                  dismiss=FALSE, append=TRUE)
    }else{
      closeAlert(session, "AlertExample1")
      if(input$dose1 < min(doo)){
        createAlert(session, anchorId="alertExample2", alertId="AlertExample2", title="Error",
                    content="Your lower dose is outside your interesting dose range.",
                    dismiss=FALSE, append=TRUE)
      }else{
        closeAlert(session, "AlertExample2")
        if(input$dose2 > max(doo)){
          createAlert(session, anchorId="alertExample3", alertId="AlertExample3", title="Error",
                      content="Your higher dose is outside your interesting dose range.",
                      dismiss=FALSE, append=TRUE)
        }else{
          closeAlert(session, "AlertExample3")
          
          "Here are plots of one set of random study data generated under the current simulation scenario: 
          doses administered and (non-)toxicities observed for individual patients (top left); how often 
          each dose was administered (top right); target dose and optimal dose estimates with 
          95% CIs after each cohort (bottom)."
        }
        
        }
      
      }
    
      })
  
  examplot <- eventReactive(input$anotherrun, {
    
    doo <- as.numeric(unlist(strsplit(input$doses, ",")))
    
    if(is.na(sum(as.numeric(unlist(strsplit(input$doses, ",")))))==FALSE &
       input$dose1 >= min(doo) &
       input$dose2 <= max(doo)){
      
      prior <- rbind(c(input$obs1, input$dose1, input$failrate1), c(input$obs2, input$dose2, input$failrate2))
      
      #if(input$howto==TRUE){
      #  thetatrue <- c(input$theta1, input$theta2)
      #}else{
      thetatrue <- theta_compute(risk_high=input$failrate2true, risk_low=input$failrate1true,
                                 TD_high=input$dose2true, TD_low=input$dose1true)
      #}
      
      if(input$consec==TRUE){
        maxseq <- input$consecutive
      }else{
        maxseq <- 1e6
      }
      
      plot(simulate_escalation(theta_true=thetatrue, r=input$target_level, prior=prior, dose_set=doo,
                               sample_size=input$n_patients, next_cohortsize=input$cohort_size, cstop=input$cstop,
                               allocation_rule=input$gainfunction, prior_type=NULL, lowstart=input$lowstart,
                               noskip=input$noskip, notoxesc=input$notoxesc, maxseq=maxseq))
      
    }
    
  })
  
  output$ExamplePlot <- renderPlot({
    
    if(input$anotherrun==FALSE){
      
      doo <- as.numeric(unlist(strsplit(input$doses, ",")))
      
      if(is.na(sum(as.numeric(unlist(strsplit(input$doses, ",")))))==FALSE &
         input$dose1 >= min(doo) &
         input$dose2 <= max(doo)){
        
        prior <- rbind(c(input$obs1, input$dose1, input$failrate1), c(input$obs2, input$dose2, input$failrate2))
        
        #if(input$howto==TRUE){
        #  thetatrue <- c(input$theta1, input$theta2)
        #}else{
        thetatrue <- theta_compute(risk_high=input$failrate2true, risk_low=input$failrate1true,
                                   TD_high=input$dose2true, TD_low=input$dose1true)
        #}
        
        if(input$consec==TRUE){
          maxseq <- input$consecutive
        }else{
          maxseq <- 1e6
        }
        
        plot(simulate_escalation(theta_true=thetatrue, r=input$target_level, prior=prior, dose_set=doo,
                                 sample_size=input$n_patients, next_cohortsize=input$cohort_size, cstop=input$cstop,
                                 allocation_rule=input$gainfunction, prior_type=NULL, lowstart=input$lowstart,
                                 noskip=input$noskip, notoxesc=input$notoxesc, maxseq=maxseq))
        
      }
      
    }else{
      
      examplot()
      
    }
    
  }, height=800, width=800)

  output$ScenariosText1 <- renderText({
    
    doo <- as.numeric(unlist(strsplit(input$doses, ",")))
    
    if(is.na(sum(as.numeric(unlist(strsplit(input$doses, ",")))))==TRUE){
      createAlert(session, anchorId="alertScenarios1", alertId="AlertScenarios1", title="Error",
                  content="Your doses must be numeric values separated by commas.",
                  dismiss=FALSE, append=TRUE)
    }else{
      closeAlert(session, "AlertScenarios1")
      if(input$dose1 < min(doo)){
        createAlert(session, anchorId="alertScenarios2", alertId="AlertScenarios2", title="Error",
                    content="Your lower dose is outside your interesting dose range.",
                    dismiss=FALSE, append=TRUE)
      }else{
        closeAlert(session, "AlertScenarios2")
        if(input$dose2 > max(doo)){
          createAlert(session, anchorId="alertScenarios3", alertId="AlertScenarios3", title="Error",
                      content="Your higher dose is outside your interesting dose range.",
                      dismiss=FALSE, append=TRUE)
        }else{
          closeAlert(session, "AlertScenarios3")
          
          "Here are six default scenarios for simulation based on Table 1 of Zhou & Whitehead (2003, Drug Inf J). 
          The 'standard' scenario is the one determined by the prior information, and the other five are directly 
          derived from it."
        }
        
        }
      
      }
    
    })
  
  output$ScenariosTable <- renderTable({
    
    doo <- as.numeric(unlist(strsplit(input$doses, ",")))
    
    if(is.na(sum(as.numeric(unlist(strsplit(input$doses, ",")))))==FALSE &
       input$dose1 >= min(doo) &
       input$dose2 <= max(doo)){
      
      scenar <- truth_scenar(TD_high=input$dose2true, TD_low=input$dose1true,
                             risk_high=input$failrate2true, risk_low=input$failrate1true)
      thetas <- matrix(NA, 6, 2)
      for(i in 1:6){
        thetas[i, ] <- theta_compute(scenar[i, 4], scenar[i, 3], scenar[i, 2], scenar[i, 1])
      }
      
      table <- cbind(scenar, thetas)
      
      rownames(table) <- c("Standard", "Potent", "Inactive", "Steep", "Very potent", "Very inactive")
      colnames(table) <- c("Dose (low)", "Dose (high)", "Toxicity rate (low)", "Toxicity rate (high)", "Intercept", "Slope")
      
      table
      
    }
    
  }, rownames=TRUE, colnames=TRUE, align='c')
  
  output$ScenariosText2 <- renderText({
    
    doo <- as.numeric(unlist(strsplit(input$doses, ",")))
    
    if(is.na(sum(as.numeric(unlist(strsplit(input$doses, ",")))))==FALSE &
       input$dose1 < min(doo) &
       input$dose2 > max(doo)){
      
      "Here is a plot of the dose-toxicity models implied by the six default scenarios."
      
    }
    
  })
  
  output$ScenariosPlot <- renderPlot({
    
    doo <- as.numeric(unlist(strsplit(input$doses, ",")))
    
    if(is.na(sum(as.numeric(unlist(strsplit(input$doses, ",")))))==FALSE &
       input$dose1 >= min(doo) &
       input$dose2 <= max(doo)){
      
      prior <- rbind(c(input$obs1, input$dose1, input$failrate1), c(input$obs2, input$dose2, input$failrate2))
      glmfit <- suppressWarnings(glm(prior[, 3] ~ log(prior[, 2]), weights=prior[, 1], family="binomial"))
      theta1p <- glmfit$coef[1]
      theta2p <- glmfit$coef[2]
      
      scenar <- truth_scenar(TD_high=input$dose2true, TD_low=input$dose1true,
                             risk_high=input$failrate2true, risk_low=input$failrate1true)
      #print(scenar)
      #scenar is a 8 by 6 matrix. The columns are obs1, obs2, dose1, dose2, risk1, risk2.
      #dose_resp1 <- dose_escalation(r=input$target_level, matrix(data=scenar[1, ], ncol=3), dose=NULL,
      #response=NULL, dl, input$n_patients, input$cohort_size, input$cstop, input$gainfunction, prior_type=NULL)
      plot(new("Scenario", theta=theta_compute(scenar[1, 4], scenar[1, 3], scenar[1, 2], scenar[1, 1]),
               r=input$target_level, dose_set=doo))
      abline(v=doo, col="grey90")
      par(lwd=2)
      for(i in 2:6){
        #dose_resp <- dose_escalation(r=input$target_level, matrix(data=scenar[i, ], ncol=3), dose=NULL,
        #response=NULL, dl, input$n_patients, input$cohort_size, input$cstop, input$gainfunction, prior_type=NULL)
        lines(new("Scenario", theta=theta_compute(scenar[i, 4], scenar[i, 3], scenar[i, 2], scenar[i, 1]),
                  r=input$target_level, dose_set=doo), col=1 + i)
      }
      par(lwd=1)
      lines(doo, (1 + exp(-(theta1p + theta2p * log(doo))))^(-1), col="grey", lwd=2)
      legend("topleft", legend=c("Target toxicity level", "Standard", "Potent", "Inactive", "Steep",
                                 "Very potent", "Very inactive", "Prior"), col=c(1, 1000 + 2:7, "grey"),
             lty=c(2, rep(1, 7)), lwd=2, bty="n")
      
    }
    
  })
  
  output$SimulationsText1 <- renderText({
    
    doo <- as.numeric(unlist(strsplit(input$doses, ",")))
    
    if(is.na(sum(as.numeric(unlist(strsplit(input$doses, ",")))))==TRUE){
      createAlert(session, anchorId="alertSimulations1", alertId="AlertSimulations1", title="Error",
                  content="Your doses must be numeric values separated by commas.",
                  dismiss=FALSE, append=TRUE)
    }else{
      closeAlert(session, "AlertSimulations1")
      if(input$dose1 < min(doo)){
        createAlert(session, anchorId="alertSimulations2", alertId="AlertSimulations2", title="Error",
                    content="Your lower dose is outside your interesting dose range.",
                    dismiss=FALSE, append=TRUE)
      }else{
        closeAlert(session, "AlertSimulations2")
        if(input$dose2 > max(doo)){
          createAlert(session, anchorId="alertSimulations3", alertId="AlertSimulations3", title="Error",
                      content="Your higher dose is outside your interesting dose range.",
                      dismiss=FALSE, append=TRUE)
        }else{
          closeAlert(session, "AlertSimulations3")
          if(w$round > 0){
            
            "Here are simulation results. Sample size, maximum likelihood estimate (MLE) of the MTD, 
            mean squared error (MSE), bias, and toxicity rate are averaged over all simulation runs."
            
          }
          
        }
        
      }
      
    }
    
  })
  
  output$SimulationsText2 <- renderText({
    
    doo <- as.numeric(unlist(strsplit(input$doses, ",")))
    
    if(is.na(sum(as.numeric(unlist(strsplit(input$doses, ",")))))==FALSE &
       input$dose1 >= min(doo) &
       input$dose2 <= max(doo) &
       w$round > 0){
      
      "Percentage of simulation runs where the study was stopped for the following reasons:"
      
    }
    
  })
  
  output$SimulationsText2x <- renderText({
    
    doo <- as.numeric(unlist(strsplit(input$doses, ",")))
    
    if(is.na(sum(as.numeric(unlist(strsplit(input$doses, ",")))))==FALSE &
       input$dose1 >= min(doo) &
       input$dose2 <= max(doo) &
       w$round > 0){
      
      "NB: more than one reason may apply at a time."
      
    }
    
  })
  
  output$SimulationsText3 <- renderText({
    
    doo <- as.numeric(unlist(strsplit(input$doses, ",")))
    
    if(is.na(sum(as.numeric(unlist(strsplit(input$doses, ",")))))==FALSE &
       input$dose1 >= min(doo) &
       input$dose2 <= max(doo) &
       w$round > 0){
      
      "Failed simulation runs:"
      
    }
    
  })

  output$SimulationsText4 <- renderText({
    
    doo <- as.numeric(unlist(strsplit(input$doses, ",")))
    
    if(is.na(sum(as.numeric(unlist(strsplit(input$doses, ",")))))==FALSE &
       input$dose1 >= min(doo) &
       input$dose2 <= max(doo) &
       w$round > 0){
      
      "Here are plots summarising the simulation results for the current scenario: number of patients used in each study 
      (top left); number of toxicities observed per study (top right); reasons for stopping each
      study (bottom left); dose recommendations at the end of each study (bottom right)."
      
    }
    
  })

  output$SimulationsText5 <- renderText({
    
    doo <- as.numeric(unlist(strsplit(input$doses, ",")))
    
    if(is.na(sum(as.numeric(unlist(strsplit(input$doses, ",")))))==FALSE &
       input$dose1 >= min(doo) &
       input$dose2 <= max(doo) &
       w$round > 0){
      
      "Here are detailed results of all simulation runs under the current scenario.
      You can download them as a csv file (see 'Downloads' tab)."
      
    }
    
  })
  
  v <- reactiveValues()
  v$da <- v$dat <- v$data <- data.frame()
  
  w <- reactiveValues()
  w$round <- 0
  
  observeEvent(input$runsim, {
    
    doo <- as.numeric(unlist(strsplit(input$doses, ",")))
    
    if(is.na(sum(as.numeric(unlist(strsplit(input$doses, ",")))))==TRUE |
       input$dose1 < min(doo) |
       input$dose2 > max(doo)){
      
      createAlert(session, anchorId="alertSimulationsX", alertId="AlertSimulationsX", title="First things first!",
                  content="Please fix all other issues before running simulations.",
                  dismiss=FALSE, append=TRUE)
    }else{
      closeAlert(session, "AlertSimulationsX")
      
      #Run and collate a full simulation
      w$round <- w$round + 1
      doo <- as.numeric(unlist(strsplit(input$doses, ",")))
      
      prior <- rbind(c(input$obs1, input$dose1, input$failrate1), c(input$obs2, input$dose2, input$failrate2))
      
      #if(input$howto==TRUE){
      #  thetatrue <- c(input$theta1, input$theta2)
      #}else{
      #  thetatrue <- theta_compute(risk_high=input$failrate2true, risk_low=input$failrate1true,
      #                             TD_high=input$dose2true, TD_low=input$dose1true)
      #}
      
      if(input$whatscen=="Standard"){
        thetatrue <- theta_compute(risk_high=input$failrate2true, risk_low=input$failrate1true,
                                   TD_high=input$dose2true, TD_low=input$dose1true)
      }
      if(input$whatscen=="Potent"){
        thetatrue <- theta_compute(risk_high=1.2 * input$failrate2true, risk_low=1.2 * input$failrate1true,
                                   TD_high=input$dose2true, TD_low=input$dose1true)
      }
      if(input$whatscen=="Inactive"){
        thetatrue <- theta_compute(risk_high=0.8 * input$failrate2true, risk_low=0.8 * input$failrate1true,
                                   TD_high=input$dose2true, TD_low=input$dose1true)
      }
      if(input$whatscen=="Steep"){
        thetatrue <- theta_compute(risk_high=1.2 * input$failrate2true, risk_low=0.8 * input$failrate1true,
                                   TD_high=input$dose2true, TD_low=input$dose1true)
      }
      if(input$whatscen=="Very potent"){
        thetatrue <- theta_compute(risk_high=1.4 * input$failrate2true, risk_low=1.4 * input$failrate1true,
                                   TD_high=input$dose2true, TD_low=input$dose1true)
      }
      if(input$whatscen=="Very inactive"){
        thetatrue <- theta_compute(risk_high=0.6 * input$failrate2true, risk_low=0.6 * input$failrate1true,
                                   TD_high=input$dose2true, TD_low=input$dose1true)
      }
      
      if(input$consec==TRUE){
        maxseq <- input$consecutive
      }else{
        maxseq <- 1e6
      }
      
      #sim_out_vals <- list()
      sim_tab_vals <- data.frame()
      
      withProgress(message='Running simulation', value=0, {
        
        for(a in 1:input$num_sim){
          out <- simulate_escalation(theta_true=thetatrue, r=input$target_level, prior=prior, dose_set=doo,
                                     sample_size=input$n_patients, next_cohortsize=input$cohort_size, cstop=input$cstop,
                                     allocation_rule=input$gainfunction, tuning=numeric(0), prior_type=NULL,
                                     lowstart=input$lowstart, noskip=input$noskip, notoxesc=input$notoxesc,
                                     maxseq=maxseq)
          sim_tab_vals <- rbind(sim_tab_vals, summary(out))
          
          incProgress(1/input$num_sim, detail=paste(a, "/", input$num_sim))
        }
        
      })
      
      mean_obs <- round(mean(sim_tab_vals$nobs, na.rm=TRUE), 3)
      mean_est <- round(mean(sim_tab_vals$mle_estimate, na.rm=TRUE), 3)
      mean_se <- round(mean((sim_tab_vals$mle_estimate - exp((log(input$target_level / (1 - input$target_level)) -
                                                                thetatrue[1]) / thetatrue[2]))^2, na.rm=TRUE), 4)
      bias <-  round(mean((sim_tab_vals$mle_estimate - exp((log(input$target_level / (1 - input$target_level)) -
                                                              thetatrue[1]) / thetatrue[2])), na.rm=TRUE), 4)
      mean_cov <- round(100 * mean(sim_tab_vals$coverage_mle, na.rm=TRUE), 1)
      mean_toxpc <- round(100 * mean(sim_tab_vals$ntox/sim_tab_vals$nobs, na.rm=TRUE), 1)
      
      prop_noslope <- round(100 * mean(sim_tab_vals$noslope), 1)
      prop_negslope <- round(100 * mean(sim_tab_vals$negslope), 1)
      prop_notox <- round(100 * mean(sim_tab_vals$notox), 1)
      prop_alltox <- round(100 * mean(sim_tab_vals$alltox), 1)
      prop_hugemle <- round(100 * mean(sim_tab_vals$hugemle), 1)    
      
      stop_all <- sum(sim_tab_vals$text=="Stop recruitment: the maximum number of patients has been reached.") +
        sum(sim_tab_vals$text=="Stop recruitment: the maximum number of patients, and the desired level of accuracy, have both been reached.") +
        sum(sim_tab_vals$text=="Stop recruitment: the maximum number of consecutive patients at the same dose, and the maximum number of patients, have both been reached.") +
        sum(sim_tab_vals$text=="Stop recruitment: the maximum number of consecutive patients at the same dose, the maximum number of patients, and the desired level of accuracy, have all been reached.")
      stop_acc <- sum(sim_tab_vals$text=="Stop recruitment: the desired level of accuracy has been reached.") +
        sum(sim_tab_vals$text=="Stop recruitment: the maximum number of patients, and the desired level of accuracy, have both been reached.") +
        sum(sim_tab_vals$text=="Stop recruitment: the maximum number of consecutive patients at the same dose, and the desired level of accuracy, have both been reached.") +
        sum(sim_tab_vals$text=="Stop recruitment: the maximum number of consecutive patients at the same dose, the maximum number of patients, and the desired level of accuracy, have all been reached.")
      stop_con <- sum(sim_tab_vals$text=="Stop recruitment: the maximum number of consecutive patients at the same dose has been reached.") +
        sum(sim_tab_vals$text=="Stop recruitment: the maximum number of consecutive patients at the same dose, and the desired level of accuracy, have both been reached.") +
        sum(sim_tab_vals$text=="Stop recruitment: the maximum number of consecutive patients at the same dose, and the maximum number of patients, have both been reached.") +
        sum(sim_tab_vals$text=="Stop recruitment: the maximum number of consecutive patients at the same dose, the maximum number of patients, and the desired level of accuracy, have all been reached.")
      stop_saf <- sum(sim_tab_vals$text=="Stop recruitment: no safe dose could be identified.") +
        sum(sim_tab_vals$text=="Stop recruitment: no safe dose could be identified because the slope of the dose-response model was zero.") +
        sum(sim_tab_vals$text=="Stop recruitment: no safe dose could be identified because the slope of the dose-response model was negative.")
      stop_neg <- sum(sim_tab_vals$text=="Stop recruitment: no safe dose could be identified because the slope of the dose-response model was negative.")
      stop_zer <- sum(sim_tab_vals$text=="Stop recruitment: no safe dose could be identified because the slope of the dose-response model was zero.")
      
      all_obs <- sim_tab_vals$nobs
      all_est <- sim_tab_vals$mle_estimate
      all_tox <- sim_tab_vals$ntox
      all_nos <- sim_tab_vals$noslope
      all_neg <- sim_tab_vals$negslope
      all_not <- sim_tab_vals$notox
      all_alt <- sim_tab_vals$alltox
      all_hug <- sim_tab_vals$hugemle
      all_sto <- sim_tab_vals$text
      
      all_rec <- sim_tab_vals$finalrec
      
      levels(all_sto)[levels(all_sto) %in% c("Stop recruitment: the maximum number of patients has been reached.",
                                             "Stop recruitment: the maximum number of patients, and the desired level of accuracy, have both been reached.",
                                             "Stop recruitment: the maximum number of consecutive patients at the same dose, and the maximum number of patients, have both been reached.",
                                             "Stop recruitment: the maximum number of consecutive patients at the same dose, the maximum number of patients, and the desired level of accuracy, have all been reached.")] <- "Max. n"
      levels(all_sto)[levels(all_sto) %in% c("Stop recruitment: the desired level of accuracy has been reached.",
                                             "Stop recruitment: the maximum number of patients, and the desired level of accuracy, have both been reached.",
                                             "Stop recruitment: the maximum number of consecutive patients at the same dose, and the desired level of accuracy, have both been reached.",
                                             "Stop recruitment: the maximum number of consecutive patients at the same dose, the maximum number of patients, and the desired level of accuracy, have all been reached.")] <- "Accuracy"
      levels(all_sto)[levels(all_sto) %in% c("Stop recruitment: the maximum number of consecutive patients at the same dose has been reached.",
                                             "Stop recruitment: the maximum number of consecutive patients at the same dose, and the desired level of accuracy, have both been reached.",
                                             "Stop recruitment: the maximum number of consecutive patients at the same dose, and the maximum number of patients, have both been reached.",
                                             "Stop recruitment: the maximum number of consecutive patients at the same dose, the maximum number of patients, and the desired level of accuracy, have all been reached.")] <- "Consec. n"
      levels(all_sto)[levels(all_sto) %in% c("Stop recruitment: no safe dose could be identified.",
                                             "Stop recruitment: no safe dose could be identified because the slope of the dose-response model was negative.",
                                             "Stop recruitment: no safe dose could be identified because the slope of the dose-response model was zero")] <- "Safety"
      levels(all_sto)[levels(all_sto)=="Stop recruitment: no safe dose could be identified because the slope of the dose-response model was negative."] <- "Neg. slope"
      levels(all_sto)[levels(all_sto)=="Stop recruitment: no safe dose could be identified because the slope of the dose-response model was zero"] <- "Zero slope"
      
      v$data <- rbind(v$data, data.frame(Scenario=input$whatscen,
                                         Runs=input$num_sim,
                                         "Sample size"=mean_obs,
                                         MLE=mean_est,
                                         MSE=mean_se, #round=w$round,
                                         Bias=bias,
                                         Coverage=mean_cov,
                                         "Toxicity rate"=mean_toxpc/100,
                                         "Dose-response model has slope zero"=100 * prop_noslope/input$num_sim,
                                         "Dose-response model has negative slope"=100 * prop_negslope/input$num_sim,
                                         "No toxicities observed"=100 * prop_notox/input$num_sim,
                                         "Only toxicities observed"=100 * prop_alltox/input$num_sim,
                                         "MLE exceeds 3 times the maximum dose"=100 * prop_hugemle/input$num_sim,
                                         "All patients used"=100 * stop_all/input$num_sim,
                                         "Accuracy reached"=100 * stop_acc/input$num_sim,
                                         "All doses unsafe"=100 * stop_saf/input$num_sim,
                                         "Consecutive patients at a dose reached"=100 * stop_con/input$num_sim,
                                         Negative=100 * stop_neg/input$num_sim,
                                         Zero=100 * stop_zer/input$num_sim))
      
      v$dat <- data.frame()
      v$dat <- rbind(v$dat, data.frame(SampleSize=all_obs,
                                       MLE=all_est,
                                       Toxicities=all_tox,
                                       NoSlope=all_nos,
                                       NegSlope=all_neg,
                                       NoTox=all_not,
                                       AllTox=all_alt,
                                       HugeMLE=all_hug,
                                       Stopping=all_sto,
                                       Recommendation=ifelse(all_rec==-999, "None", all_rec)))
      
      v$da <- data.frame()
      #v$da <- rbind(v$da, data.frame(SampleSize=all_obs[(input$num_sim - 99):input$num_sim],
      #                               MLE=all_est[(input$num_sim - 99):input$num_sim],
      #                               Toxicities=all_tox[(input$num_sim - 99):input$num_sim],
      #                               NoSlope=all_nos[(input$num_sim - 99):input$num_sim],
      #                               NegSlope=all_neg[(input$num_sim - 99):input$num_sim],
      #                               NoTox=all_not[(input$num_sim - 99):input$num_sim],
      #                               AllTox=all_alt[(input$num_sim - 99):input$num_sim],
      #                               HugeMLE=all_hug[(input$num_sim - 99):input$num_sim],
      #                               Stopping=all_sto[(input$num_sim - 99):input$num_sim],
      #                               Recommendation=all_rec[(input$num_sim - 99):input$num_sim]))
      
      v$da <- rbind(v$da, data.frame("Sample size"=all_obs[1:input$num_sim],
                                     Toxicities=as.integer(all_tox)[1:input$num_sim],
                                     MLE=all_est[1:input$num_sim],
                                     #NoSlope=all_nos[1:input$num_sim],
                                     #NegSlope=all_neg[1:input$num_sim],
                                     #NoTox=all_not[1:input$num_sim],
                                     #AllTox=all_alt[1:input$num_sim],
                                     #HugeMLE=all_hug[1:input$num_sim],
                                     "Stopping reason"=all_sto[1:input$num_sim],
                                     "Dose recommendation"=ifelse(all_rec==-999, "None", all_rec)[1:input$num_sim]))

    }
    
  })
  
  output$SimulationsTable1 <- renderTable({
    
    if(w$round < 1){
      createAlert(session, anchorId="alertSimulations0", alertId="AlertSimulations0", title="Sorry, nothing to display.",
                  content="No simulations have been run so far.",
                  dismiss=FALSE, append=TRUE)
    }else{
      closeAlert(session, "AlertSimulations0")
      
      v$data[, c(1:6, 8)]
    }
    
  }, align='c', sanitize.colnames.function=function(x)gsub("\\.", " ", x))
  
  output$SimulationsTable2 <- renderTable({
    
    if(w$round > 0){
      #v$data[, 9:18]

      #tab <- data.frame(Reason=c("All patients used", "Sufficient accuracy of the dose estimate",
      #                           "Enough consecutive patients at the same dose", "No safe dose identified",
      #                           "Dose-response model has negative slope", "Dose-response model has slope zero"),
      #                  Total=t(v$data[, 14:19]),
      #                  Percent=100 * t(v$data[, 14:19])/input$num_sim)
      
      v$data[, c(1, 14:17)]

    }
    
  }, colnames=TRUE, rownames=FALSE, align='c', digits=1,
  sanitize.colnames.function=function(x)gsub("\\.", " ", x))
  
  output$SimulationsTable3 <- renderTable({
    
    if(w$round > 0){

      #tuff <- data.frame(Reason=c("Dose-response model has slope zero",
      #                            "Dose-response model has negative slope",
      #                            "No toxicities observed", "Only toxicities observed",
      #                            "MLE of the MTD exceeds 3 times the maximum dose"),
      #                   Total=t(v$data[, 9:13]),
      #                   Percent=100 * t(v$data[, 9:13])/input$num_sim)
      
      v$data[, c(1, 9:13)]
      
    }
    
  }, colnames=TRUE, rownames=FALSE, align='c', digits=1,
  sanitize.colnames.function=function(x)gsub("\\.", " ", x))
  
  output$SimulationsTable4 <- renderTable({
    
    if(w$round > 0){
      v$da
    }
    
  }, rownames=TRUE, align='c',
  sanitize.colnames.function=function(x)gsub("\\.", " ", x))
  
  output$SimulationsPlot <- renderPlot({
    
    if(w$round > 0){
      
      par(mfrow=c(2, 2))
      barplot(table(factor(v$dat[, "SampleSize"])), xlab="Sample Size", ylab="Trials", main="Sample Sizes", las=1)
      barplot(table(factor(v$dat[, "Toxicities"])), xlab="Toxicities", ylab="Trials", main="Toxicities Observed", las=1)
      barplot(table(factor(v$dat[, "Stopping"])), xlab="Reason", ylab="Trials", main="Stopping Reasons", las=1)
      
      if("None" %in% levels(factor(v$dat[, "Recommendation"]))){
        fff <- factor(v$dat[, "Recommendation"],
                      levels=c("None", sort(as.numeric(levels(factor(v$dat[, "Recommendation"]))[-length(levels(factor(v$dat[, "Recommendation"])))]))))
        barplot(table(fff), xlab="Dose", ylab="Trials", main="Final Dose Recommendations", las=1)
      }else{
        fff <- factor(v$dat[, "Recommendation"], levels=sort(as.numeric(levels(factor(v$dat[, "Recommendation"])))))
        barplot(table(fff), xlab="Dose", ylab="Trials", main="Final Dose Recommendations", las=1)
      }
      
      #if("None" %in% levels(factor(v$dat[, "Recommendation"]))){
      #  barplot(table(relevel(factor(v$dat[, "Recommendation"]), ref="None")), xlab="Dose", ylab="Trials", main="Final Dose Recommendations", las=1)
      #}else{
      #  barplot(table(factor(v$dat[, "Recommendation"])), xlab="Dose", ylab="Trials", main="Final Dose Recommendations", las=1)
      #}
      
    }
    
  })
  
  output$report <- downloadHandler(
    filename = function(){paste('Report', Sys.Date(), '.pdf', sep='')},
    content = function(file){
      out = knit2pdf('DesignReport.Rnw', clean=TRUE)
      file.copy(out, file) # move pdf to file for downloading
    },
    contentType = 'application/pdf'
  )
  
  output$design <- downloadHandler(
    filename = function(){paste('Design', Sys.Date(), '.csv', sep='')},
    content = function(file){
      inputdata <- c(input$gainfunction, input$n_patients, input$cohort_size,
                     input$obs1, input$dose1, input$failrate1,
                     input$obs2, input$dose2, input$failrate2,
                     input$cstop, input$target_level,
                     input$lowstart, input$noskip, input$notoxesc,
                     ifelse(input$consec==TRUE, input$consecutive, 1e6),
                     as.numeric(unlist(strsplit(input$doses, ","))))
      out = write.csv(inputdata, file)
      file.copy(out, file)
    },
    contentType = 'text/csv'
  )
  
  output$simulations <- downloadHandler(
    filename = function(){paste('Simulations', Sys.Date(), '.csv', sep='')},
    content = function(file){
      outputdata <- v$da
      out = write.csv(outputdata, file)
      file.copy(out, file)
    },
    contentType = 'text/csv'
  )
  
  session$onSessionEnded(stopApp)
  
}

shinyApp(ui=ui, server=server)

# shiny::runApp("C:/Users/pallmann/Desktop/Shiny_Dose_Finding/Design", launch.browser=TRUE)
# rsconnect::deployApp("C:/Users/pallmann/Desktop/Shiny_Dose_Finding/Design")
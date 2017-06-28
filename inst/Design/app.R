library("knitr")
library("shiny")
library("shinyBS")

library("DT")

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
             sliderInput("n_patients", "Maximum number of patients:", min=1, max=200, value=30),
             sliderInput("cohort_size", "Patients per cohort:", min=1, max=10, value=3),
             sliderInput("target_level", "Target toxicity level:", min=0.01, max=0.5, value=0.3, step=0.01),
             textInput("doses", "Doses:", "1, 1.5, 2, 2.5, 3"),
             bsTooltip("n_patients", "How many patients are you willing to recruit at most?",
                       placement="right", trigger="hover", options=list(container="body")),
             bsTooltip("cohort_size", "How many patients should enter the study at the same time?",
                       placement="right", trigger="hover", options=list(container="body")),
             bsTooltip("target_level", "What proportion of DTLs are you targeting?",
                       placement="right", trigger="hover", options=list(container="body")),
             bsTooltip("doses", "Which doses would you like to investigate?",
                       placement="right", trigger="hover", options=list(container="body"))
             ),
           
           wellPanel(
             h3("2. Prior opinion"),
             p("Specify your prior opinion about the toxicity rates of two distinct doses, 
               and the strength of your opinion in terms of pseudo-observations."),
             hr(),
             fluidRow(
               column(6,
                      h4("Lower dose"),
                      hr(),
                      numericInput("dose1", "Dose:", 1),
                      sliderInput("failrate1", "Toxicity rate:", min=0.01, max=0.99, value=0.2, step=0.01),
                      numericInput("obs1", "Pseudo-observations:", 3, min=1, max=100, step=1, width=170)
                      ),
               column(6,
                      h4("Higher dose"),
                      hr(),
                      numericInput("dose2", "Dose:", 3),
                      sliderInput("failrate2", "Toxicity rate:", min=0.01, max=0.99, value=0.5, step=0.01),
                      numericInput("obs2", "Pseudo-observations:", 3, min=1, max=100, step=1, width=170)
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
           )
           
    ),
    
    column(3,
           
           wellPanel(
             h3("3. Simulation model"),
             p("Specify the 'true' model for simulation."),
             hr(),
             
             checkboxInput("howto", "Specify model via intercept and slope"),
             bsTooltip("howto", "Tick here if you want to specify the simulation model in terms of its intercept and slope rather than via the toxicity rates at two doses.",
                       placement="right", trigger="hover", options=list(container="body")),
             #radioButtons(inputId="howto", label="How to specify true model",
             #             choices=c("Two doses"="TRUE", "Intercept slope"="FALSE"),
             #             selected="TRUE", inline=TRUE),
             conditionalPanel(
               condition="input.howto == false",
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
             ),
             conditionalPanel(
               condition="input.howto == true",
               numericInput("theta1", "Model intercept:", -1, width=120),
               numericInput("theta2", "Model slope:", 1, width=120),
               bsTooltip("theta1", "Adjust the intercept of the true model here.",
                         placement="right", trigger="hover", options=list(container="body")),
               bsTooltip("theta2", "Adjust the slope of the true model here.",
                         placement="right", trigger="hover", options=list(container="body"))
             )
             
           ),
           
           wellPanel(
             h3("4. Further settings"),
             p("Specify some additional settings."),
             hr(),
             sliderInput("cstop", "Stopping precision:", min=1, max=7, value=1.5, step=0.1),
             selectInput("gainfunction", "Gain function:", width=150,
                         list("Patient gain"="PatientGain", "Variance gain"="VarianceGain")),
             checkboxInput("lowstart", "Always start at lowest dose", value=TRUE),
             checkboxInput("noskip", "Don't skip any doses when escalating", value=TRUE),
             checkboxInput("notoxesc", "Don't escalate upon observing a toxicity", value=FALSE),
             bsTooltip("cstop", "What level of precision for the estimated dose (ratio lower/upper 95% confidence bound) would you consider sufficient for stopping? Setting this to 1 prevents early stopping for precision.",
                       placement="right", trigger="hover", options=list(container="body")),
             bsTooltip("gainfunction", "What gain function would you like to use? (patient gain: recommend dose with posterior response probability closest to the target toxicity level; variance gain: recommend dose to maximise learning about the dose-response relationship)",
                       placement="right", trigger="hover", options=list(container="body")),
             bsTooltip("lowstart", "Tick here if you want to start from the lowest dose.",
                       placement="right", trigger="hover", options=list(container="body")),
             bsTooltip("noskip", "Tick here if you do not want to skip doses when escalating.",
                       placement="right", trigger="hover", options=list(container="body")),
             bsTooltip("notoxesc", "Tick here if you do not want to escalate if one or more toxicities were observed in the previous cohort.",
                       placement="right", trigger="hover", options=list(container="body"))
           ),
           
           wellPanel(
             h3("5. Simulations"),
             p("Specify the simulation settings. The six scenarios are shown under the 'Scenarios' tab."),
             hr(),
             
             radioButtons(inputId="whatscen", label="Scenario:",
                          choices=c("Standard"="standard", "Potent"="potent", "Inactive"="inactive",
                                    "Steep"="steep", "Very potent"="very potent", "Very inactive"="very inactive"),
                          selected="standard", inline=TRUE),
             bsTooltip("whatscen", "Under what scenario would you like to run the simulations?",
                       placement="right", trigger="hover", options=list(container="body")),
             
             
             numericInput("num_sim", "Repetitions:", 1000, min=10, max=10000, step=1, width=100),
             actionButton("runsim", "Run simulation"),
             bsTooltip("num_sim", "How many simulations would you like to run?",
                       placement="right", trigger="hover", options=list(container="body"))
           )
           
    ),
    
    column(6,
           tabsetPanel(
             tabPanel("Model",
                      hr(),
                      p("Here is a plot of the simulation model (representing the assumed true dose-toxicity 
                        relationship), the dose-toxicity relationship implied by the prior opinion, and the target 
                        toxicity level. The target dose is the dose for which, under the true model, the toxicity 
                        rate is equal to the target level."),
                      bsAlert("dosealert"), plotOutput("DoseCurve", width="100%")),
             tabPanel("Example",
                      hr(),
                      p("Here are plots of one set of random study data generated under the current simulation 
                        scenario: doses administered and (non-)toxicities observed for individual patients (top 
                        left); how often each dose was administered (top right); target dose (red) and optimal dose 
                        estimates (black) with 95% CIs (dashed) after each cohort (bottom)."),
                      bsAlert("dosealert"), plotOutput("SimOut"), width="100%"),
             tabPanel("Scenarios",
                      hr(),
                      p("Here are six default scenarios for simulation based on Table 1 of Zhou & Whitehead (2003, 
                        Drug Inf J). The 'standard' scenario is the one determined by the prior opinion, and the
                        other five are directly derived from it."),
                      bsAlert("dosealert"), tableOutput("ScenTab"),
                      hr(),
                      p("Here is a plot of the dose-toxicity models implied by the six default scenarios."),
                      plotOutput("scenarios", width="100%")),
             tabPanel("Simulations",
                      hr(),
                      p("Here are simulation results."),
                      tableOutput("ResultTab1"),
                      p("Sample size, maximum likelihood estimate, mean squared error, bias, coverage, and % 
                        toxicities are averaged over all simulation runs."),
                      tableOutput("ResultTab2"),
                      h4("Reasons for failed simulation runs: % of simulation runs where ..."),
                      p("NoSlo: ... no slope could be estimated for the dose-toxicity model (because all patients 
                        received the same dose)"),
                      p("NegSlo: ... the slope of the dose-toxicity model was estimated to be negative (i.e. lower 
                        doses turned out to be more toxic than higher doses)"),
                      p("NoTox: ... no toxicities were observed"),
                      p("AllTox: ... only toxicities were observed"),
                      p("HugeMLE: ... the MLE of the optimal dose exceeded three times the maximum dose"),
                      h4("Reasons for stopping the study: number of simulation runs where the study was stopped ..."),
                      p("AllUsed: ... upon using all patients"),
                      p("Acc: ... for sufficient accuracy of the estimate"),
                      p("Neg: ... because of a negative slope estimate"),
                      p("HugeEst: ... because of a very large dose estimate"),
                      hr(),
                      p("Here are some more detailed results of the last 10 runs under the current simulation 
                        scenario."),
                      tableOutput("TextTab"),
                      hr(),
                      p("Here are plots summarising the simulation results: number of patients used in each study 
                        (top left); number of toxicities observed per study (top right); reasons for stopping each 
                        study (bottom left); dose recommendations at the end of each study (bottom right)."),
                      plotOutput("LastPlot")),
             tabPanel("Downloads",
                      hr(),
                      h4("Design"),
                      p("Download a csv file containing the design parameters as they will be used for the study."),
                      downloadButton('design'),
                      hr(),
                      h4("Report"),
                      p("Download a pdf report summarising the input parameters and key characteristics of the design."),
                      downloadButton('report')),
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
                        ",",
                        HTML("<a href='https://ebailey78.github.io/shinyBS/'>shinyBS</a>"),
                        ", and",
                        HTML("<a href='https://yihui.name/knitr/'>knitr</a>"),
                        "."),
                      p("Feedback and bug reports welcome, please email to: ",
                        HTML("<a href='mailto:p.pallmann@lancaster.ac.uk'>p.pallmann@lancaster.ac.uk</a>"),
                        ".")
           )
    )
  )
)
)

server <- function(input, output, session){
  
  output$DoseCurve <- renderPlot({
    
    doo <- as.numeric(unlist(strsplit(input$doses, ",")))
    doodle <- seq(min(doo), max(doo), length.out=100)
    
    if(input$dose1 < min(doo)){
      createAlert(session, anchorId="dosealert", alertId="doseAlert", title="Oops!",
                  content="Your lower dose is outside your interesting dose range.",
                  dismiss=TRUE, append=FALSE)
    }else if(input$dose2 > max(doo)){
      createAlert(session, anchorId="dosealert", alertId="doseAlert", title="Oops!",
                  content="Your higher dose is outside your interesting dose range.",
                  dismiss=TRUE, append=FALSE)
    }else{
      closeAlert(session, "doseAlert")
      
      if(input$howto==TRUE){
        thetatrue <- c(input$theta1, input$theta2)
      }else{
        thetatrue <- theta_compute(risk_high=input$failrate2true, risk_low=input$failrate1true,
                                   TD_high=input$dose2true, TD_low=input$dose1true)
      }
      
      true_value <- exp((log(input$target_level / (1 - input$target_level)) - thetatrue[1]) / thetatrue[2])
      plot(doodle, (1 + exp(-(thetatrue[1] + thetatrue[2] * log(doodle))))^(-1), type="l", xlab="Dose",
           ylab="P(Toxicity)", xlim=range(doo), ylim=c(0, 1), main=paste("Target Dose:", round(true_value, 2)),
           las=1)
      prior <- rbind(c(input$obs1, input$dose1, input$failrate1), c(input$obs2, input$dose2, input$failrate2))
      glmfit <- suppressWarnings(glm(prior[, 3] ~ log(prior[, 2]), weights=prior[, 1], family="binomial"))
      theta1p <- glmfit$coef[1]
      theta2p <- glmfit$coef[2]
      lines(doodle, (1 + exp(-(theta1p + theta2p * log(doodle))))^(-1), col=3)
      abline(h=input$target_level, col=2)
      legend("topleft", lty=c(1, 1, 1), col=c(1, 3, 2), legend=c("True", "Prior", "Target"), bty="n")
      #abline(v=true_value, col=3, lty=2)
    }
    
  },
  height=600, width=800)
  
  output$scenarios <- renderPlot({
    
    doo <- as.numeric(unlist(strsplit(input$doses, ",")))
    
    if(input$dose1 < min(doo)){
      createAlert(session, anchorId="dosealert", alertId="doseAlert", title="Oops!",
                  content="Your lower dose is outside your interesting dose range.",
                  dismiss=TRUE, append=FALSE)
    }else if(input$dose2 > max(doo)){
      createAlert(session, anchorId="dosealert", alertId="doseAlert", title="Oops!",
                  content="Your higher dose is outside your interesting dose range.",
                  dismiss=TRUE, append=FALSE)
    }else{
      closeAlert(session, "doseAlert")
      
      scenar <- prior_scenar(input$dose2, input$dose1, input$failrate2, input$failrate1, input$obs2, input$obs1)
      #print(scenar)
      #scenar is a 8 by 6 matrix. The columns are obs1, obs2, dose1, dose2, risk1, risk2.
      #dose_resp1 <- dose_escalation(r=input$target_level, matrix(data=scenar[1, ], ncol=3), dose=NULL,
      #response=NULL, dl, input$n_patients, input$cohort_size, input$cstop, input$gainfunction, prior_type=NULL)
      plot(new("Scenario", theta=theta_compute(scenar[1, 6], scenar[1, 5], scenar[1, 4], scenar[1, 3]),
               r=input$target_level, dose_set=doo))
      for(i in 2:6){
        #dose_resp <- dose_escalation(r=input$target_level, matrix(data=scenar[i, ], ncol=3), dose=NULL,
        #response=NULL, dl, input$n_patients, input$cohort_size, input$cstop, input$gainfunction, prior_type=NULL)
        lines(new("Scenario", theta=theta_compute(scenar[i, 6], scenar[i, 5], scenar[i, 4], scenar[i, 3]),
                  r=input$target_level, dose_set=doo), col=1 + i)
      }
      legend("topleft", legend= c("Standard", "Potent", "Inactive", "Steep", "Very potent", "Very inactive"),
             col=1000 + 2:7, lty=rep(1, 6), bty="n")
    }
    
  },
  height=600, width=800)
  
  output$SimOut <- renderPlot({
    
    doo <- as.numeric(unlist(strsplit(input$doses, ",")))
    
    if(input$dose1 < min(doo)){
      createAlert(session, anchorId="dosealert", alertId="doseAlert", title="Oops!",
                  content="Your lower dose is outside your interesting dose range.",
                  dismiss=TRUE, append=FALSE)
    }else if(input$dose2 > max(doo)){
      createAlert(session, anchorId="dosealert", alertId="doseAlert", title="Oops!",
                  content="Your higher dose is outside your interesting dose range.",
                  dismiss=TRUE, append=FALSE)
    }else{
      closeAlert(session, "doseAlert")
      
      prior <- rbind(c(input$obs1, input$dose1, input$failrate1), c(input$obs2, input$dose2, input$failrate2))
      
      if(input$howto==TRUE){
        thetatrue <- c(input$theta1, input$theta2)
      }else{
        thetatrue <- theta_compute(risk_high=input$failrate2true, risk_low=input$failrate1true,
                                   TD_high=input$dose2true, TD_low=input$dose1true)
      }
      
      plot(simulate_escalation(theta_true=thetatrue, r=input$target_level, prior=prior, dose_set=doo,
                               sample_size=input$n_patients, next_cohortsize=input$cohort_size, cstop=input$cstop,
                               allocation_rule=input$gainfunction, prior_type=NULL, lowstart=input$lowstart,
                               noskip=input$noskip, notoxesc=input$notoxesc))
    }
    
  },
  height=800, width=800)
  
  output$ScenTab <- renderTable({
    
    scenar <- prior_scenar(input$dose2, input$dose1, input$failrate2, input$failrate1, input$obs2, input$obs1)
    thetas <- matrix(NA, 6, 2)
    for(i in 1:6){
      thetas[i, ] <- theta_compute(scenar[i, 6], scenar[i, 5], scenar[i, 4], scenar[i, 3])
    }
    
    table <- cbind(scenar, thetas)
    
    rownames(table) <- c("Standard", "Potent", "Inactive", "Steep", "Very potent", "Very inactive")
    colnames(table) <- c("Pseudo-obs. (lo)", "Pseudo-obs. (hi)", "Dose (lo)", "Dose (hi)",
                         "Tox. rate (lo)", "Tox. rate (hi)", "Intercept", "Slope")
    
    table
    
  },
  rownames=T, colnames=T, align='c')
  
  v <- reactiveValues()
  v$da <- v$dat <- v$data <- data.frame()
  
  w <- reactiveValues()
  w$round <- 0
  
  observeEvent(input$runsim,{
    #Run and collate a full simulation
    w$round <- w$round + 1
    doo <- as.numeric(unlist(strsplit(input$doses, ",")))
    
    if(input$whatscen=="standard"){
      prior <- rbind(c(input$obs1, input$dose1, input$failrate1), c(input$obs2, input$dose2, input$failrate2))
    }
    if(input$whatscen=="potent"){
      prior <- rbind(c(input$obs1, input$dose1, 1.2 * input$failrate1), c(input$obs2, input$dose2, 1.2 * input$failrate2))
    }
    if(input$whatscen=="inactive"){
      prior <- rbind(c(input$obs1, input$dose1, 0.8 * input$failrate1), c(input$obs2, input$dose2, 0.8 * input$failrate2))
    }
    if(input$whatscen=="steep"){
      prior <- rbind(c(input$obs1, input$dose1, 0.8 * input$failrate1), c(input$obs2, input$dose2, 1.2 * input$failrate2))
    }
    if(input$whatscen=="very potent"){
      prior <- rbind(c(input$obs1, input$dose1, 1.4 * input$failrate1), c(input$obs2, input$dose2, 1.4 * input$failrate2))
    }
    if(input$whatscen=="very inactive"){
      prior <- rbind(c(input$obs1, input$dose1, 0.6 * input$failrate1), c(input$obs2, input$dose2, 0.6 * input$failrate2))
    }
    
    if(input$howto==TRUE){
      thetatrue <- c(input$theta1, input$theta2)
    }else{
      thetatrue <- theta_compute(risk_high=input$failrate2true, risk_low=input$failrate1true,
                                 TD_high=input$dose2true, TD_low=input$dose1true)
    }
    
    #sim_out_vals <- list()
    sim_tab_vals <- data.frame()
    
    withProgress(message='Running simulation', value=0, {
      
      for(a in 1:input$num_sim){
        out <- simulate_escalation(theta_true=thetatrue, r=input$target_level, prior=prior, dose_set=doo,
                                   sample_size=input$n_patients, next_cohortsize=input$cohort_size, cstop=input$cstop,
                                   allocation_rule=input$gainfunction, tuning=numeric(0), prior_type=NULL,
                                   lowstart=input$lowstart, noskip=input$noskip, notoxesc=input$notoxesc)
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
    
    stop_all <- sum(sim_tab_vals$text=="Stop recruitment, the maximum number of patients has been reached.")
    stop_acc <- sum(sim_tab_vals$text=="Stop recruitment, the required level of accuracy has been reached.")
    stop_neg <- sum(sim_tab_vals$text=="Stop recruitment, the slope of the dose-response model is zero or negative.")
    stop_lar <- sum(sim_tab_vals$text=="Stop recruitment, the dose estimate is very large.")
    
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
    
    levels(all_sto)[levels(all_sto)=="Stop recruitment, the maximum number of patients has been reached."] <- "Max. number"
    levels(all_sto)[levels(all_sto)=="Stop recruitment, the required level of accuracy has been reached."] <- "Accuracy"
    levels(all_sto)[levels(all_sto)=="Stop recruitment, the slope of the dose-response model is zero or negative."] <- "Neg. slope"
    levels(all_sto)[levels(all_sto)=="Stop recruitment, the dose estimate is very large."] <- "Huge est."
    
    v$data <- rbind(v$data, data.frame(Scenario=input$whatscen,
                                       Runs=input$num_sim,
                                       SampleSize=mean_obs,
                                       MLE=mean_est,
                                       MSE=mean_se, #round=w$round,
                                       Bias=bias,
                                       Coverage=mean_cov,
                                       Toxicity=mean_toxpc,
                                       NoSlo=prop_noslope,
                                       NegSlo=prop_negslope,
                                       NoTox=prop_notox,
                                       AllTox=prop_alltox,
                                       HugeMLE=prop_hugemle,
                                       AllUsed=stop_all,
                                       Acc=stop_acc,
                                       Neg=stop_neg,
                                       HugeEst=stop_lar))
    
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
                                     Recommendation=all_rec))
    
    v$da <- data.frame()
    v$da <- rbind(v$da, data.frame(SampleSize=all_obs[(input$num_sim - 9):input$num_sim],
                                   MLE=all_est[(input$num_sim - 9):input$num_sim],
                                   Toxicities=all_tox[(input$num_sim - 9):input$num_sim],
                                   NoSlope=all_nos[(input$num_sim - 9):input$num_sim],
                                   NegSlope=all_neg[(input$num_sim - 9):input$num_sim],
                                   NoTox=all_not[(input$num_sim - 9):input$num_sim],
                                   AllTox=all_alt[(input$num_sim - 9):input$num_sim],
                                   HugeMLE=all_hug[(input$num_sim - 9):input$num_sim],
                                   Stopping=all_sto[(input$num_sim - 9):input$num_sim],
                                   Recommedation=all_rec[(input$num_sim - 9):input$num_sim]))
    
  })
  
  output$ResultTab1 <- renderTable(v$data[, 1:8], align='c')
  
  output$ResultTab2 <- renderTable(v$data[, 9:17], align='c')
  
  output$TextTab <- renderTable(v$da, align='c')
  
  output$LastPlot <- renderPlot({par(mfrow=c(2, 2))
                                 barplot(table(factor(v$dat[, "SampleSize"])), xlab="Sample Size", ylab="Trials",
                                         main="Sample Sizes", las=1)
                                 barplot(table(factor(v$dat[, "Toxicities"])), xlab="Toxicities", ylab="Trials",
                                         main="Toxicities Observed", las=1)
                                 barplot(table(factor(v$dat[, "Stopping"])), xlab="Reason", ylab="Trials",
                                         main="Stopping Reasons", las=1)
                                 barplot(table(factor(v$dat[, "Recommendation"])), xlab="Dose", ylab="Trials",
                                         main="Final Dose Recommendations", las=1)
  }, width=800, height=800)
  
  #makeSim <- reactive(x={
    
    #if(is.null(inFile())){
    #  return(NULL)
    #}else{
    
  #  dose_resp <- dose_escalation(r = targetlevel,
  #                               prior = matrix(data=c(samplesizeA, samplesizeB, doseA, doseB,
  #                                                     eventrateA, eventrateB), ncol=3),
  #                               dose = inFile()[, input$dosechoice],
  #                               response = inFile()[, input$outcomechoice],
  #                               dose_set = seq(mindose, maxdose, by=doseincrement),
  #                               sample_size = samplesize,
  #                               next_cohortsize = cohortsize,
  #                               cstop = cstop,
  #                               allocation_rule = gainfunction,
  #                               prior_type = NULL)
    
  #  print(dose_resp)
  
  #})
  
  output$report = downloadHandler(
    filename = function(){paste('Report', Sys.Date(), '.pdf', sep='')},
    content = function(file){
      out = knit2pdf('a.Rnw', clean=TRUE)
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
                     as.numeric(unlist(strsplit(input$doses, ","))))
      out = write.csv(inputdata, file)
      file.copy(out, file)
    },
    contentType = 'text/csv'
  )
  
}

shinyApp(ui=ui, server=server)

#dat <- data.frame(response=c(0.2, 0.5), dose=c(1, 3))
#fit <- glm(response ~ log(dose), family="binomial", data=dat)
#intercept <- fit$coef[1]
#slope <- fit$coef[2]

#scenar <- prior_scenar(input$dose2, input$dose1, input$failrate2, input$failrate1, input$obs2, input$obs1)
#theta <- theta_compute(scenar[1, 6], scenar[1, 5], scenar[1, 4], scenar[1, 3]),

#output$SlopeUI <- renderUI({
#if(is.null(input$input_type))
#  return()
#  sliderInput("dynamic", "Dynamic", min=1, max=20, value=10),
#})

# shiny::runApp("C:/Users/pallmann/Desktop/Shiny_Dose_Finding/Design", launch.browser=TRUE)
# rsconnect::deployApp("C:/Users/pallmann/Desktop/Shiny_Dose_Finding/Design")
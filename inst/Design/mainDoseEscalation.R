#prior <- matrix(data=c(3,3,1.2,1.8,0.2,0.5),ncol = 3)
#dose_set <- seq(1, 3, by=0.1)
library("methods")

#dose_resp <- dose_escalation(r=0.3, prior, dose=rep(1,20), response=rep(0,20), dose_set, sample_size=10,
# next_cohortsize=3 ,cstop=1.4,allocation_rule="PatientGain",prior_type=NULL)
##############################################################################################################

PatientGain <- setClass("PatientGain", representation(pars="numeric"), prototype=list(pars=0),
                        validity=function(object){return(TRUE)})

VarianceGain <- setClass("VarianceGain", representation(pars="numeric"), prototype=list(pars=0),
                         validity=function(object){return(TRUE)})

setGeneric(name="rec_dose",
           def=function(x, info){
             standardGeneric("rec_dose")
           })

setMethod(f="rec_dose", signature("PatientGain", "list"),
          definition=function(x, info){
            gain <- abs(info$respProb_doseSet - info$r)
            d <- which.min(gain)
            return(info$dose_set[d])
          })

setMethod(f="rec_dose", signature("VarianceGain", "list"),
          definition=function(x, info){
            nume_l <- sum(info$respProb_doses * (1 - info$respProb_doses) * log(info$doses)) +
              info$next_cohortsize * info$respProb_doseSet * (1 - info$respProb_doseSet) * info$logdoseSet
            denom_l <- sum(info$respProb_doses * (1 - info$respProb_doses)) + info$next_cohortsize *
              info$respProb_doseSet * (1 - info$respProb_doseSet)
            weightedaverage_l <- nume_l/denom_l
            nume_part_1 <- sum(info$respProb_doses * (1 - info$respProb_doses)) + info$next_cohortsize *
              info$respProb_doseSet * (1 - info$respProb_doseSet)
            nume_part_2 <- sum(info$respProb_doses * (1 - info$respProb_doses) * log(info$doses)^2)
            nume_part_3 <- 2 * weightedaverage_l * sum(info$respProb_doses * (1 - info$respProb_doses) * log(info$doses))    
            nume_part_4 <- weightedaverage_l^2 * sum(info$respProb_doses * (1 - info$respProb_doses))
            nume_part_5 <- info$next_cohortsize * info$respProb_doseSet * (1 - info$respProb_doseSet) *
              (info$logdoseSet - weightedaverage_l)^2
            nume_gain <- nume_part_1 * (nume_part_2 - nume_part_3 + nume_part_4 + nume_part_5)
            denom_gain <- sum(info$respProb_doses * (1 - info$respProb_doses) *
                                (log(info$doses) - log(info$dose_estimate))^2) + info$next_cohortsize *
              info$respProb_doseSet * (1 - info$respProb_doseSet) * (info$logdoseSet - log(info$dose_estimate))^2
            gain <- (info$theta2/log(info$dose_estimate))^2 * nume_gain/denom_gain
            d <- which.max(gain)
            return(info$dose_set[d])
          })

######################################################################
SimulationHistory <- setClass("SimulationHistory",
                              representation(history="data.frame", recs="data.frame", stopping="character",
                                             theta_true="numeric", dose_set="numeric", true_value="numeric",
                                             target="numeric", finalrec="numeric"),
                              prototype=list(history=data.frame(round=0, dose=0, response=0),
                                             recs=data.frame(round=0, pme_estimate=0, pme_intercept=0, pme_slop=0,
                                                             lower=0, upper=0), stopping=character(0)),
                              validity=function(object){
                                if(dim(object@history)[2]!=3 | dim(object@recs)[2]!=6) return(FALSE)
                                return(TRUE)
                              })

setMethod(f="summary", signature("SimulationHistory"),
          definition=function(object){
            x <- object
            nobs <- dim(x@history)[1]
            ntox <- sum(x@history$response)
            pme_estimate <- x@recs$pme_estimate[dim(x@recs)[1]] #final pme estimate
            pme_intercept <- x@recs$pme_intercept[dim(x@recs)[1]] #final pme intercept
            pme_slop <- x@recs$pme_slop[dim(x@recs)[1]] #final pme slope
            coverage <- 1 * (x@recs$lower[dim(x@recs)[1]] < x@true_value & x@recs$upper[dim(x@recs)[1]] > x@true_value)
            text <- x@stopping
            finalrec <- x@finalrec
            #Compute the MLE from the observed data only
            fit_mle <- glm(response ~ log(dose), family="binomial", data=x@history)
            intercept <- fit_mle$coef[1]
            slope <- fit_mle$coef[2]
            mle <- exp((log(x@target/(1 - x@target)) - fit_mle$coef[1])/fit_mle$coef[2])
            
            #if(is.na(fit_mle$coef[2] > 0 & sum(x@history$response) > 0 & mle < (3 * max(x@dose_set)))){
            #  slope <- intercept <- mle <- coverage_mle <- NA
            #}else{
            
            if(min(x@history$dose)==max(x@history$dose)){
              slope <- intercept <- mle <- coverage_mle <- NA
              noslope <- TRUE
              negslope <- notox <- alltox <- hugemle <- FALSE
            }else{
              if(sum(x@history$response) < 1){
                slope <- intercept <- mle <- coverage_mle <- NA
                notox <- TRUE
                noslope <- negslope <- alltox <- hugemle <- FALSE
              }else{
                if(sum(x@history$response)==length(x@history$response)){
                  slope <- intercept <- mle <- coverage_mle <- NA
                  alltox <- TRUE
                  notox <- noslope <- negslope <- hugemle <- FALSE
                }else{
                  if(fit_mle$coef[2] < 0){
                    slope <- intercept <- mle <- coverage_mle <- NA
                    negslope <- TRUE
                    noslope <- notox <- alltox <- hugemle <- FALSE
                  }else{
                    if(mle > (3 * max(x@dose_set))){
                      slope <- intercept <- mle <- coverage_mle <- NA
                      hugemle <- TRUE
                      noslope <- negslope <- notox <- alltox <- FALSE
                    }else{
                      vec1 <- c(1/fit_mle$coef[2], log(mle)/fit_mle$coef[2])
                      vcov <- summary(fit_mle)$cov.unscaled
                      asympVar_log <- t(vec1) %*% vcov %*% vec1
                      CIlow95 <- mle * exp(-qnorm(0.975) * sqrt(asympVar_log))
                      CIupp95 <- mle * exp(qnorm(0.975) * sqrt(asympVar_log))
                      coverage_mle <- 1 * (CIlow95 < x@true_value & CIupp95 > x@true_value)
                      negslope <- notox <- alltox <- hugemle <- noslope <- FALSE
                    }
                  }
                }
              }
            }
          
              #if(fit_mle$coef[2] > 0 & sum(x@history$response) > 0 & mle < (3 * max(x@dose_set))){
                
              #}else{
              #  slope <- intercept <- mle <- coverage_mle <- NA
              #}
            
            
            #invalid <- is.na(mle)
            
            return(data.frame(nobs=nobs, ntox=ntox, pme_estimate=pme_estimate, pme_intercept=pme_intercept,
                              pme_slop=pme_slop, coverage=coverage, text=text, mle_estimate=mle,
                              coverage_mle=coverage_mle, mle_intercept=intercept, mle_slope=slope,
                              finalrec=finalrec, noslope=noslope, negslope=negslope, notox=notox,
                              alltox=alltox, hugemle=hugemle))
          })

setMethod(f="plot", signature("SimulationHistory"),
          definition=function(x){
            par(mfrow=c(2, 2), cex=1, las=1)
            plot(NULL, xlim=c(1, dim(x@history)[1]), ylim=range(x@dose_set), xlab="Patient", ylab="Dose",
                 main="Toxicities Observed")
            null <- sapply(1:dim(x@history)[1], function(n){
              points(x=n, y=x@history$dose[n], pch=x@history$response[n] + 21, bg=x@history$response[n])
            })
            legend(x=1, y=max(x@dose_set), legend=c("non-toxic", "toxic"), pch=c(21, 22), pt.bg=c(0, 1),
                   bty="n")
            barplot(table(factor(x@history$dose, levels=x@dose_set)), xlab="Dose", ylab="Patients",
                    main="Doses Administered")
            plot(x@recs$round, x@recs$pme_estimate, type="l",
                 ylim=c(min(x@recs$lower, x@true_value), max(if(max(x@recs$upper)==Inf){x@true_value}else{x@recs$upper}, x@true_value)),
                 xlab="Cohort", ylab="Dose", main="MTD Estimates", lwd=2, col=4)
            lines(x@recs$round, x@recs$lower, type="l", lty=3, lwd=2, col=4)
            lines(x@recs$round, x@recs$upper, type="l", lty=3, lwd=2, col=4)
            abline(h=x@true_value, lty=2, lwd=2)
            legend("topright", lty=c(2, 1, 3), lwd=2, col=c(1, 4, 4),
                   legend=c("True MTD", "MTD estimate", "95% confidence band"), bty="n")
            #plot(x@recs$round, x@recs$pme_estimate, type="l",
            #     ylim=c(min(x@dose_set, x@true_value, x@recs$round), max(x@dose_set, x@true_value, x@recs$round)),
            #     xlab="Cohort", ylab="Dose", main="MTD Estimates (zoomed in)", lwd=2, col=4)
            #lines(x@recs$round, x@recs$lower, type="l", lty=3, lwd=2, col=4)
            #lines(x@recs$round, x@recs$upper, type="l", lty=3, lwd=2, col=4)
            #abline(h=x@true_value, lty=2, lwd=2)
            #legend("topright", lty=c(2, 1, 3), lwd=2, col=c(1, 4, 4),
            #       legend=c("True MTD", "MTD estimate", "95% confidence band"), bty="n")
          }
)

#################################
Scenario <- setClass("Scenario",
                     representation(theta="numeric", r="numeric", dose_set="numeric"),
                     prototype=list(theta=c(-2, 2), r=0.3, dose_set=seq(1, 3, by=0.1)), # why seq(1, 3, by=0.1) ?
                     validity=function(object) return(TRUE)
)


########################
setMethod(f="plot", signature("Scenario"),
          definition=function(x){
            par(mfrow=c(1, 1))
            ds <- seq(min(x@dose_set), max(x@dose_set), length.out=100)
            plot(ds, (1 + exp(-(x@theta[1] + x@theta[2] * log(ds))))^-1, type="l", ylim=c(0, 1), col=2,
                 xlab="Dose", ylab="P(Toxicity)", main="Simulation Scenarios", las=1, lwd=2)
            abline(h=x@r, col=1, lty=2, lwd=2)
            #abline(v=x@dose_set,lty=3)
          }
)


# DoseOutput<-setClass("DoseOutput",
#                      representation(text="character",recommendation="numeric",estimate="numeric",CI="numeric"),
#                      prototype=list(text="",recommendation=integer(0),estimate=integer(0),CI=integer(0)),
#                      validity=function(object) {
#                        return(TRUE)
#                      }
# ) 


setMethod(f="lines", signature("Scenario"),
          definition=function(x, col=1){
            ds <- seq(min(x@dose_set), max(x@dose_set), length.out=100)
            lines(ds, (1 + exp(-(x@theta[1] + x@theta[2] * log(ds))))^-1, type="l", ylim=c(0, 1), col=col)
          }
)

###################

DoseOutput <- setClass("DoseOutput",
                       representation(text="character", recommendation="numeric", pme_estimate="numeric",
                                      pme_intercept="numeric", pme_slop="numeric", CI="numeric", finalrec="numeric"),
                       prototype=list(text="", recommendation=integer(0), pme_estimate=integer(0),
                                      pme_intercept=integer(0), pme_slop=integer(0), CI=integer(0), finalrec=integer(0)),
                       validity=function(object){
                         return(TRUE)
                       })

setMethod(f="print", signature("DoseOutput"),
          definition=function(x){
            print(x@text)
          })

#############
# dose-escalation input prior and response data so far, main output is recommended dose for next cohort. 
# output: DoseOutput list including text(stopping reason), recommendation (dose for the next cohort), estimate(dose at r), CI
dose_escalation <- function(r, prior, dose, response, dose_set, sample_size, next_cohortsize,
                            cstop, allocation_rule, tuning=numeric(0), prior_type, lowstart,
                            noskip, notoxesc, maxseq){
  
  allocation_rule <- new(allocation_rule, pars=tuning)
  logdoseSet <- log(dose_set)
    
    ##########################***************************########################################
    #combine the pseudo data prior to observations
    ##########################***************************########################################
    doses <- c(rep(prior[, 2], times=prior[, 1]), dose) #NB: Wont work if a non-integer 
    responses <- c(rep(prior[, 3], times=prior[, 1]), response)
    pseudo_length <- length(responses) - length(response)
    resp_type <- rep(c(0, 1), c(pseudo_length, length(response))) #Establish which parts are pseudo-data
    index <- match(round(doses, 1), round(dose_set, 1))
    #if(any(is.na(index))) stop("Data contains doses not in the specific dose set")
    ## Find modal estimate of dose-response model parameters
    FIT <- suppressWarnings(glm(responses ~ log(doses), family=binomial(link="logit")))#,weights=w1)) #PME
    SF <- summary(FIT)
    vcov <- SF$cov.unscaled
    theta1 <- coef(SF)[1, 1] #pme intercept
    theta2 <- coef(SF)[2, 1] #pme slope
   # theta <- c(theta1,theta2)
    dose_estimate <- exp((log(r/(1 - r)) - theta1)/theta2)#pme ed30
    
    respProb_doseSet<- 1.0/(1.0 + exp(-(theta1 + theta2 * logdoseSet))) 
    respProb_doses <- 1.0/(1.0 + exp(-(theta1 + theta2 * log(doses)))) 
    #Place all the required information into a single list called info...
    info <- list(theta1=theta1, theta2=theta2, logdoseSet=logdoseSet, r=r, doses=doses,
                 respProb_doses=respProb_doses, next_cohortsize=next_cohortsize, dose_set=dose_set,
                 respProb_doseSet=respProb_doseSet, dose_estimate=dose_estimate)
    
    ### dose for the next cohort
    RecDose <- rec_dose(allocation_rule, info)
  
  if(noskip==TRUE){
    if(is.null(dose)==TRUE){
      if(match(RecDose, dose_set) > 2){
        RecDose <- dose_set[2]
      }
    }else{
      if((match(RecDose, dose_set) - match(max(dose), dose_set)) > 1){
        RecDose <- dose_set[match(max(dose), dose_set) + 1]
      }
    }
  }
  
  if(lowstart==TRUE & is.null(dose)==TRUE){ # alternative: sum(resp_type) < 1
    RecDose <- min(dose_set)
  }
  
  if(notoxesc==TRUE & is.null(dose)==FALSE){
    len <- length(responses)
    if(sum(responses[(len - next_cohortsize + 1):len]) > 0 & RecDose > doses[len]){
      RecDose <- doses[len]
    }
  }
    
  ##########################################
  # stopping rule 
  #####################################
    
  if(dose_estimate==Inf){
    
    text <- "Stop recruitment: no safe dose could be identified because the slope of the dose-response model was zero."
    recommendation <- integer(0)
    CIlow95 <- 0
    CIupp95 <- 1e6
    finalrec <- -999
    
  }else{
    
    if(FIT$converged==FALSE){

      text <- "Repeat the previous dose."
      
    }else{
      
      ## Calculate 95% CI for TD100r
      vec1 <- c(1/theta2, log(dose_estimate)/theta2)
      asympVar_log <- t(vec1) %*% vcov %*% vec1
      CIlow95 <- dose_estimate * exp(-qnorm(0.975) * sqrt(asympVar_log))
      CIupp95 <- dose_estimate * exp(qnorm(0.975) * sqrt(asympVar_log))
      #############################################
      #Statements for the displayed text
      
      if(dose_estimate < min(dose_set) & theta2 > 0){
        text <- "Stop recruitment: no safe dose could be identified."
        recommendation <- integer(0)
        finalrec <- -999
      }
      
      if(theta2 <= 0){
        text <- "Stop recruitment: no safe dose could be identified because the slope of the dose-response model was negative."
        recommendation <- integer(0)
        finalrec <- -999
      }
      
      if(dose_estimate >= min(dose_set) & theta2 > 0){
        
        if(length(doses) - pseudo_length >= sample_size & CIupp95/CIlow95 > cstop){
          text <- "Stop recruitment: the maximum number of patients has been reached."
          recommendation <- integer(0)
          finalrec <- RecDose
        }
        
        if(length(doses) - pseudo_length >= sample_size & CIupp95/CIlow95 <= cstop){
          text <- "Stop recruitment: the maximum number of patients, and the desired level of accuracy, have both been reached."
          recommendation <- integer(0)
          finalrec <- RecDose
        }
        
        if(length(doses) - pseudo_length < sample_size & CIupp95/CIlow95 <= cstop){
          text <- "Stop recruitment: the desired level of accuracy has been reached."
          recommendation <- integer(0)
          finalrec <- RecDose
        }
        
        if(length(dose) >= maxseq & length(unique(dose[max(length(dose) - maxseq + 1, 1):length(dose)]))==1 &
           CIupp95/CIlow95 > cstop & length(doses) - pseudo_length < sample_size){
          text <- "Stop recruitment: the maximum number of consecutive patients at the same dose has been reached."
          recommendation <- integer(0)
          finalrec <- RecDose
        }
        
        if(length(dose) >= maxseq & length(unique(dose[max(length(dose) - maxseq + 1, 1):length(dose)]))==1 &
           CIupp95/CIlow95 <= cstop & length(doses) - pseudo_length < sample_size){
          text <- "Stop recruitment: the maximum number of consecutive patients at the same dose, and the desired level of accuracy, have both been reached."
          recommendation <- integer(0)
          finalrec <- RecDose
        }
        
        if(length(dose) >= maxseq & length(unique(dose[max(length(dose) - maxseq + 1, 1):length(dose)]))==1 &
           CIupp95/CIlow95 > cstop & length(doses) - pseudo_length >= sample_size){
          text <- "Stop recruitment: the maximum number of consecutive patients at the same dose, and the maximum number of patients, have both been reached."
          recommendation <- integer(0)
          finalrec <- RecDose
        }
        
        if(length(dose) >= maxseq & length(unique(dose[max(length(dose) - maxseq + 1, 1):length(dose)]))==1 &
           CIupp95/CIlow95 <= cstop & length(doses) - pseudo_length >= sample_size){
          text <- "Stop recruitment: the maximum number of consecutive patients at the same dose, the maximum number of patients, and the desired level of accuracy, have all been reached."
          recommendation <- integer(0)
          finalrec <- RecDose
        }
        
        if(length(dose) < maxseq | (length(dose) >= maxseq &
                                    length(unique(dose[max(length(dose) - maxseq + 1, 1):length(dose)])) > 1)){
          if(CIupp95/CIlow95 > cstop & theta2 > 0 & length(doses) - pseudo_length < sample_size &
             FIT$converged==TRUE & dose_estimate >= min(dose_set)){
            text <- paste("Given the prior information and data from the last ", length(dose),
                          " patients, the recommended dose for the next cohort of patients is ", RecDose, ".", sep="")
            recommendation <- RecDose
            finalrec <- integer(0)
          }else{
            recommendation <- integer(0)
            finalrec <- RecDose
          }
        }
        
      }
      
    }
    
  }
  
    output <- new("DoseOutput", text=text, recommendation=recommendation, pme_estimate=dose_estimate,
                  pme_intercept=theta1, pme_slop=theta2, CI=c(CIlow95, CIupp95), finalrec=finalrec)
    
    return(output)
}

#simulation_escalation gives one simulation
#output: history(), recs(recommended dose),stopping (reason),
simulate_escalation <- function(theta_true, r, prior, dose_set, sample_size, next_cohortsize, cstop,
                                allocation_rule, tuning=numeric(0), prior_type, lowstart, noskip,
                                notoxesc, maxseq, seed=NULL){
  true_value <- exp((log(r/(1 - r)) - theta_true[1])/theta_true[2])
  if (!is.null(seed)) set.seed(seed)
  dose <- response <- NULL
  rec <- dose_escalation(r, prior, dose, response, dose_set, sample_size, next_cohortsize, cstop,
                         allocation_rule, tuning, prior_type, lowstart, noskip, notoxesc, maxseq)
  history <- data.frame()
  round <- 0
  recs <- data.frame(round=round, pme_estimate=rec@pme_estimate, pme_intercept=rec@pme_intercept,
                     pme_slop=rec@pme_slop, lower=rec@CI[1], upper=rec@CI[2])
  while(length(rec@recommendation)==1){
    round <- round + 1
    new_resp <- simulate_responses(theta_true, rec@recommendation, next_cohortsize)
    dose <- c(dose, rep(rec@recommendation, next_cohortsize))
    response <- c(response, new_resp)
    history <- rbind(history, data.frame(round=rep(round, next_cohortsize),
                                         dose=rep(rec@recommendation, next_cohortsize), response=new_resp))
    rec <- dose_escalation(r, prior, dose, response, dose_set, sample_size, next_cohortsize, cstop,
                           allocation_rule, tuning, prior_type, lowstart, noskip, notoxesc, maxseq)
    recs <- rbind(recs, data.frame(round=round, pme_estimate=rec@pme_estimate, pme_intercept=rec@pme_intercept,
                                   pme_slop=rec@pme_slop, lower=rec@CI[1], upper=rec@CI[2]))
  }
  finalrec <- rec@finalrec
  output <- new("SimulationHistory", history=history, recs=recs, stopping=rec@text, dose_set=dose_set,
                true_value=true_value, target=r, finalrec=finalrec)
  return(output)
}


simulate_responses <- function(theta_true, dose, cohortsize){
  p <- (1 + exp(-(theta_true[1] + theta_true[2] * log(dose))))^-1
  return(1 * (runif(cohortsize) < p))
}

# covs <- dest <- npat<-0
# for (i in 1:100) {
#   a<-simulate_escalation(c(-11.93,7.5),r=0.3,prior=rbind(c(3,1,0.1),c(3,3,0.5)),dose_set,sample_size=60,
#      next_cohortsize=3,cstop=1.4,allocation_rule="VarianceGain",prior_type=NULL)
#   npat[i] <- dim(a@history)[1]
#   dest[i] <- a@recs$estimate[dim(a@recs)[1]]
#   covs[i] <- 1*(a@recs$lower[dim(a@recs)[1]] < a@true_value & a@recs$upper[dim(a@recs)[1]] > a@true_value)
# }


###############################
# plot the scenarios
#######################
#prior matrix(data=c(3,3,1.2,1.8,0.2,0.5),ncol = 3)

#solve theta from the scenarios: use glmfit instead

###############################################
### Compute intercept & slope for scenarios ###
###############################################

# TD_x: x% of patients have toxicities

theta_compute <- function(risk_high, risk_low, TD_high, TD_low){
  slop <- log((risk_high/(1 - risk_high))/(risk_low/(1 - risk_low)))/log(TD_high/TD_low)  
  inter <- log(risk_high/(1 - risk_high)) - slop * log(TD_high)
  para <- c(inter, slop)
  return(para)
}

#######################################################
### Compute different scenarios based on prior data ###
#######################################################

#prior_scenar <- function(TD_high, TD_low, risk_high, risk_low, number_high, number_low){
#  scenar <- matrix(0, 6, 6)
#  scenar[, 1] <- number_low
#  scenar[, 2] <- number_high
#  scenar[, 5] <- risk_low
#  scenar[, 6] <- risk_high
#  scenar34 <- matrix(round(c(TD_low * 1.0, TD_high * 1.0, # standard
#                             TD_low * 0.7, TD_high * 0.8, # potent
#                             TD_low * 1.1, TD_high * 1.2, # inactive
#                             TD_low * 1.1, TD_high * 1.0, # steep
#                             TD_low * 0.8, TD_high * 1.0, # very potent
#                             TD_low * 1.5, TD_high * 2.0), # very inactive
#                             #TD_low * 1.0, TD_high * 0.8, # TBC
#                             #TD_low * 1.0, TD_high * 1.2), # TBC
#                           digit=1), 2, 6)
#  #scenar34[1, scenar34[1, ] < TD_low] <- TD_low
#  #scenar34[2, scenar34[2, ] > TD_high] <- TD_high
#  scenar[, 3:4] <- t(scenar34)
#  return(scenar)
#}

truth_scenar <- function(TD_high, TD_low, risk_high, risk_low){
  scenar <- matrix(0, 6, 4)
  #scenar[, 1] <- number_low
  #scenar[, 2] <- number_high
  scenar[, 1] <- TD_low
  scenar[, 2] <- TD_high
  scenar34 <- matrix(round(c(risk_low * 1.0, risk_high * 1.0, # standard
                             risk_low * 1.2, risk_high * 1.2, # potent
                             risk_low * 0.8, risk_high * 0.8, # inactive
                             risk_low * 0.8, risk_high * 1.2, # steep
                             risk_low * 1.4, risk_high * 1.4, # very potent
                             risk_low * 0.6, risk_high * 0.6), # very inactive
                           digit=2), 2, 6)
  #scenar34[1, scenar34[1, ] < TD_low] <- TD_low
  #scenar34[2, scenar34[2, ] > TD_high] <- TD_high
  scenar[, 3:4] <- t(scenar34)
  return(scenar)
}


# #####################
# #figure 1
# ####################
# 
# #ed30
# dose_resp1 <- dose_escalation(r=0.3, matrix(data=scenar[1,],ncol = 3), dose=NULL, response=NULL, dose_set,
# sample_size=30, next_cohortsize=3 ,cstop=1.4,allocation_rule="VarianceGain",prior_type=NULL)
# plot(new("Scenario",theta=theta_scenarios(scenar[1,3],scenar[1,4]),r=0.3,dose_set=dose_set))
# for (i in 2:8){
# dose_resp <- dose_escalation(r=0.3, matrix(data=scenar[i,],ncol = 3), dose=NULL, response=NULL, dose_set,
# sample_size=30, next_cohortsize=3 ,cstop=1.4,allocation_rule="VarianceGain",prior_type=NULL)
# lines(new("Scenario",theta=theta_scenarios(scenar[i,3],scenar[i,4]),r=0.3,dose_set=dose_set),col=1+i)
# }
# legend("bottomright", legend= c("Standard","Potent","Inactive","Steep","Very potent","Very inactive","TBC","TBC"),
# col = 1000+2:9, pt.cex = 2,pch = rep(-1,8), lty=rep(1,8),lwd = 1, cex=0.75, horiz=FALSE)
# 
# 


# summaries <- data.frame()# table 2
# summaries1 <- list()#figure 2
# for (i in 1:100) {
#   set.seed(1000+i)
#   a<-simulate_escalation(theta_scenarios(TD_low,TD_high),r=0.3,prior=rbind(c(3,1.2,0.2),c(3,1.8,0.5)),dose_set,
#   sample_size=21,next_cohortsize=3,cstop=1.4,allocation_rule="VarianceGain",prior_type=NULL)
#   summaries <- rbind(summaries,summary(a))  
#   summaries1 <- list(summaries1,a@history$dose)
#   }
# vec_summaries1 <- unlist(summaries1)
# 
# barplot(table(vec_summaries1)/100, ylim = c(0,1.1*max(table(vec_summaries1)/100)))
#  #hist(vec_summaries1,breaks = 19)
# lines(density(vec_summaries1),col="red")
# summary(vec_summaries1)
#  
# 
# 
# 
# plot(NULL,xlim=c(1,dim(a@history)[1]),ylim=c(min(dose_set),max(dose_set)+0.1),xlab="Patient",ylab="Dose")
# null <- sapply(1:dim(a@history)[1],function(n) polygon(x=c(n-1,n,n,n-1),y=c(a@history$dose[n],a@history$dose[n],
# a@history$dose[n]+0.1,a@history$dose[n]+0.1),col=a@history$response[n]))
# 

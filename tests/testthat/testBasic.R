context("Excercise Operations")

test_that("testBO: Works As Expected", {
  d <- data.frame(lifetime=c(2,1,2),censored=c(FALSE,FALSE,TRUE))
  dTrain <- QSurvival::buildQuasiObsForTraining(d,d$lifetime,ifelse(d$censored,NA,d$lifetime),
                                            'origRow','sampleAge','deathEvent')
  model <- glm(deathEvent~1,data=dTrain,family=binomial) # time free model
  dA <- QSurvival::buildQuasiObsForComparison(d,5,d$lifetime,ifelse(d$censored,NA,d$lifetime),
                                     'origRow','sampleAge','deathEvent')
  dA$hazardPred <- predict(model,newdata=dA,type='response')
  QSurvival::summarizeHazard(dA,'origRow','sampleAge','hazardPred',
                             survivalColumnName='survival',
                             deathIntensityColumnName='deathIntensity')
  dB <- QSurvival::buildQuasiObsForApplication(d,5,'origRow','sampleAge')
  dB$hazardPred <- predict(model,newdata=dB,type='response')
  QSurvival::summarizeHazard(dB,'origRow','sampleAge','hazardPred',
                             survivalColumnName='survival',
                             deathIntensityColumnName='deathIntensity')
})

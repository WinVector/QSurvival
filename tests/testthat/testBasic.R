context("Excercise Operations")

test_that("testBO: Works As Expected", {
  d <- data.frame(lifetime=c(2,1,2),censored=c(FALSE,FALSE,TRUE))
  d2 <- QSurvival::buildQuasiObsForTraining(d,d$lifetime,ifelse(d$censored,NA,d$lifetime),
                                            'origRow','sampleAge','deathEvent')
  QSurvival::buildQuasiObsForComparison(d,5,d$lifetime,ifelse(d$censored,NA,d$lifetime),
                                     'origRow','sampleAge','deathEvent')
  QSurvival::buildQuasiObsForApplication(d,5,'origRow','sampleAge')
  d2$hazardPred <- 0.1
  QSurvival::summarizeHazard(d2,'origRow','sampleAge','hazardPred',
                             survivalColumnName='survival',
                             deathIntensityColumnName='deathIntensity')
})

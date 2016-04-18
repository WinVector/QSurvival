context("Compatible with dplyr")

test_that("testdplyrNotation: Works with dplyr and magrittr and dplyr", {
  if(requireNamespace("dplyr",quietly=TRUE)) {
    library("dplyr")
    library("QSurvival")
    d <- dplyr::as.tbl(data.frame(lifetime=c(2,1,2),censored=c(FALSE,FALSE,TRUE)))
    d %>% buildQuasiObsForTraining(d$lifetime,
                                   ifelse(d$censored,NA,d$lifetime),
                                   'origRow','sampleAge','deathEvent') %>%
      glm(formula=deathEvent~1,family=binomial) -> model # time free model
    d %>% buildQuasiObsForComparison(5,d$lifetime,
                                     ifelse(d$censored,NA,d$lifetime),
                                     'origRow','sampleAge','deathEvent') -> dC
    dC$hazardPred <- predict(model,newdata=dC,type='response')
    dC %>% summarizeHazard('origRow','sampleAge','hazardPred',
                      survivalColumnName='survival',
                      deathIntensityColumnName='deathIntensity')
    d %>% buildQuasiObsForApplication(5,'origRow','sampleAge') ->dA
    dA$hazardPred = predict(model,newdata=dA,type='response')
    dA %>% summarizeHazard('origRow','sampleAge','hazardPred',
                      survivalColumnName='survival',
                      deathIntensityColumnName='deathIntensity')
  }
})

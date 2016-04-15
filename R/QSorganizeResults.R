

#' Convert hazard to survival function and instantaneous
#' event intensities.
#'
#' Sums up hazard function grouped by original ID variable ordered by step.
#'
#' @param d data.frame
#' @param idColumnName character scalar, column containing original row ids
#' @param indexColumnName character scalar, column containing quasi event indices
#' @param hazardColumnName character scalar, column containing hazard scores
#' @param survivalColumnName character scalar, column to write survival probability in
#' @param deathIntensityColumnName character scalar, column to write death intensities
#' @param parallelCluster optional parallel cluster to run on
#' @return list, details=survival data.frame, expectedLifetime has lifetime estimates
#'
#' @examples
#'
#' d <- data.frame(lifetime=c(2,1,2),censored=c(FALSE,FALSE,TRUE))
#' d2 <- buildQuasiObsForComparison(d,5,d$lifetime,ifelse(d$censored,NA,d$lifetime),
#'    'origRow','sampleAge','deathEvent')
#' d2$hazardPred <- 0.1
#' summarizeHazard(d2,'origRow','sampleAge','hazardPred',
#'    survivalColumnName='survival',
#'    deathIntensityColumnName='deathIntensity')
#'
#'
#' @importFrom dplyr bind_rows
#' @export
summarizeHazard <- function(d,idColumnName,indexColumnName,hazardColumnName,
                      survivalColumnName='survival',
                      deathIntensityColumnName='deathIntensity',
                      parallelCluster=NULL) {
  dlist <- split(d,d[[idColumnName]])
  mkWorker <- function(idColumnName,
                        indexColumnName,
                        hazardColumnName,
                        survivalColumnName,
                        deathIntensityColumnName) {
    force(idColumnName)
    force(indexColumnName)
    force(hazardColumnName)
    force(survivalColumnName)
    force(deathIntensityColumnName)
    function(di) {
      di <- di[order(di[[indexColumnName]]),]
      di[[survivalColumnName]] <- cumprod(pmax(0,1-pmin(1,di[[hazardColumnName]])))
      before <- c(1,di[[survivalColumnName]])
      before <- before[-length(before)]
      di[[deathIntensityColumnName]] <-  before - di[[survivalColumnName]]
      di[['expectedLifetime']] <- sum(di[[survivalColumnName]])
      di
    }
  }
  worker <- mkWorker(idColumnName,
                     indexColumnName,
                     hazardColumnName,
                     survivalColumnName,
                     deathIntensityColumnName)
  if(is.null(parallelCluster) || (!requireNamespace("parallel",quietly=TRUE))) {
    dlist <- lapply(dlist,worker)
  } else {
    dlist <- parallel::parLapply(parallelCluster,dlist,worker)
  }
  dH <- dplyr::bind_rows(dlist)
  expectedLifetime <- aggregate(as.formula(paste('expectedLifetime ~ ',idColumnName)),
                                data=dH,FUN=mean)
  dH[['expectedLifetime']] <- NULL
  list(details=dH,expectedLifetime=expectedLifetime)
}

#' Calculate what fraction of ages are below each threshold. Do NOT use this on censored data (as all ages are interpreted as end).
#'
#' @param ages numeric vector non-negative with integer values
#' @param range integer scalar integer posotive range to calculate to
#' @return data frame with fraction less than our equal to each value
#'
#' @examples
#'
#' summarizeActual(1:5,7)
#'
#' @export
summarizeActual <- function(ages,range) {
  if(min(ages)<0) {
    stop('summarizeActual ages must be non-negative')
  }
  if(range<=0) {
    stop('summarizeActual range must be positive')
  }
  ages <- as.data.frame(table(age=as.character(ages)))
  ages$age <- as.numeric(as.character(ages$age))
  ages <- ages[order(ages$age),]
  tot <- sum(ages$Freq)
  res <- data.frame(age=0:range)
  count <- numeric(range+1)
  count[ages$age+1] <- ages$Freq
  data.frame(age=0:range,count=(tot - cumsum(count))/tot)
}

#' Build observed survival curves for a frame.  Do NOT use this on censored data (as all ages are interpreted as end).
#'
#' @param d data.frame
#' @param groupColumnName name of grouping column
#' @param ageColumnName name of age column
#' @param parallelCluster optional parallel cluster to run on
#' @return survival curves
#'
#' @examples
#'
#' s <- summarizeActualFrame(data.frame(age=1:10,
#'                                      group=as.factor((1:10)%%2)),
#'    'group','age')
#' # ggplot() + geom_line(data=s,mapping=aes(x=age,y=survival,color=group))
#'
#' @export
summarizeActualFrame <- function(d,groupColumnName,ageColumnName,
                                 parallelCluster=NULL) {
  range <- max(d[[ageColumnName]])
  dlist <- split(d,d[[groupColumnName]])
  mkWorker <- function(groupColumnName,ageColumnName) {
    force(groupColumnName)
    force(ageColumnName)
    function(di) {
      ri <- summarizeActual(di[[ageColumnName]],range)
      colnames(ri) <- c(ageColumnName,'survival')
      ri[[groupColumnName]] <- di[1,groupColumnName]
      ri
    }
  }
  worker <- mkWorker(groupColumnName,ageColumnName)
  if(is.null(parallelCluster) || (!requireNamespace("parallel",quietly=TRUE))) {
    reslist <- lapply(dlist,worker)
  } else {
    reslist <- parallel::parLapply(parallelCluster,dlist,worker)
  }
  res <- dplyr::bind_rows(reslist)
  res
}
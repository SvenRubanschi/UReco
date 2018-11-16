#' Dose the R²-cutoff
#'
#' what it dose
#'
#' @param fit fitted enviermental vecoter from vegen
#' @param r.select R²-cutoff
#'
#' @return the fittet enviremantal vectors witour the r² valu ounder the cutoff
#'
#' @examples
#' fit <- envfit(ord, env, permutations = 1000)
#' out <- select.envfit(fit, r.select = 0.3))
#' out
#'
#' @export
select.envfit<-function(fit, r.select){
  for (i in 1:length(fit$vectors$r)) {
    if (fit$vectors$r[i]<r.select) {
      fit$vectors$arrows[i,]=NA
      i=i+1
    }
  }
  return(fit)
}

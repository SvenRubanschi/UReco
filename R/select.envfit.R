#' Dose the R²-cutoff
#'
#' Selects the fitted vectors of a ordination by a certai the r²-cutoff
#'
#' @param fit fitted vecoter from vegan
#' @param r.select r²-cutoff
#'
#' @return the fitted vectors without the r² value higher than the cutoff
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

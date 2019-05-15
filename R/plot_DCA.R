#' Plots a Detrended Correspondence Analysis (DCA)
#' 
#' Performs a DCA with fitted vectors of species and environment selected by a certain r2-cutoff
#'
#' @param veg vegetation matrix
#' @param env environment matrix (only numeric will be consider)
#' @param weight Downweighting of rare species (0: no)
#' @param group.col column of the group info
#' @param r.cutoff_env the R²-cutoff of the environment
#' @param r.cutoff_spec the R²-cutoff of the species
#' @param colvec colour vector of the groups like c("col1","col2","col3") or c(1,2,3)
#' @param pch point shape
#' @param lty line shape
#' @param lwd line size
#' @param cex scales plot
#' @param cex.lab scale of label
#' @param cex.leg scale of legend
#' @param ordhull circled the groups
#'
#'
#' @note The points are the sites. Environmental parameter will be transformed into numeric.
#' 
#' @examples
#' library(vegan) 
#' data(dune)
#' data(dune.env)
#' plot_DCA(veg = dune, env = dune.env[-4], group.col = 3 , r.cutoff_env = 0.2, r.cutoff_spec = 0.2)
#'
#' @export
plot_DCA <- function(veg, env = NULL, weigth = 0, group.col = 0, r.cutoff_env = 0.3, r.cutoff_spec = 0.3, colvec = NULL,
                     pch = 20, lty = 1, lwd = 1, cex = 1, cex.lab = 1,  cex.leg = 1, ordihull = F){
  if (group.col > 0) {
    names(env)[group.col] <- "group"
    env$group <- as.factor(env$group)
    env$group <- droplevels(env$group)
    vec.env <- env[-group.col]
    num <- unlist(lapply(vec.env, is.numeric))
    vec.env <- vec.env[,num]
    colvec <- if (is.null(colvec) == T){
      grDevices::rainbow(nlevels(env$group))
    } else{
      colvec
    }
  } else {
    num <- unlist(lapply(env, is.numeric))
    vec.env <- env[,num]
  }
  DCA <- vegan::decorana(veg, iweigh = weigth)
  SPEC <- simpECO::select.envfit(vegan::envfit(DCA, veg, perm = 1000), r.select = r.cutoff_spec)
  ENV <- if (is.null(env) == F) {
    simpECO::select.envfit(vegan::envfit(DCA, vec.env, perm = 1000), r.select = r.cutoff_env)
  }
  DCA_Ax1 <- paste("DCA1:", format(round(DCA$evals[1], 3), nsmall = 3))
  DCA_Ax2 <- paste("DCA2:", format(round(DCA$evals[2], 3), nsmall = 3))
  plot(DCA, type = "n",xlab= DCA_Ax1, ylab= DCA_Ax2, cex = cex, cex.lab=cex.lab)
  if (group.col > 0) {
    with(env,points(DCA, display = "sites", col=colvec[env$group], pch = pch))
  } else {
    with(veg, points(DCA, display = "sites", pch = pch))
  }
  plot(SPEC, col = "black", cex = cex)
  if (is.null(env) == F) {
    plot(ENV, col = "gray65", cex = cex)
  }
  if (group.col > 0) {
    legend("topright", legend = levels(env$group), col=colvec, lty = lty, lwd = lwd, cex = cex.leg)
  }
  if (ordihull == T) {
    with(env, vegan::ordihull(DCA, group, col=colvec, lty = lty, lwd = lwd))
  }
}

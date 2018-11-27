#' Plots a Nonmetric Multidimensional Scaling
#'
#' Points are the sites
#'
#' @param veg vegetation matrix
#' @param env environment matrix
#' @param group.col column in which the group is standing
#' @param r.cutoff_env the R²-cutoff of the environment
#' @param r.cutoff_spec the R²-cutoff of the species
#' @param colvec colour vector of the groups
#' @param pch point shape
#' @param lty line shape
#' @param lwd line size
#' @param cex scaled plot
#' @param cex.lab scale of label
#' @param cex.leg scale of legend
#' @param ordhull circled the groups
#'
#'
#' @return Plots a grouped NMDS with fitted species and environment vectors
#' 
#' @examples
#' NO
#'  
#' @export
plot_NMDS <- function(veg, env, group.col = 0, r.cutoff_env = 0.3, r.cutoff_spec = 0.3, colvec,
                     pch = 20, lty = 1, lwd = 1, cex = 1, cex.lab = 1,  cex.leg = 1, ordihull = F){
  if (group.col > 0) {
    names(env)[group.col] <- "group"
    env$group <- as.factor(env$group)
    env$group <- droplevels(env$group)
    vec.env <- env[-c(group.col)]
    colvec <- if (exists("colvec") == T){
      grDevices::rainbow(nlevels(env$group))
    } else{
      colvec
    }
  } else {
    num <- unlist(lapply(env, is.numeric))
    vec.env <- env[,num]
  }
  NMDS = vegan::metaMDS(veg, k= 2, try= 100, trace = 0)
  SPEC <- simpECO::select.envfit(vegan::envfit(NMDS, veg, perm = 1000), r.select = r.cutoff_spec)
  ENV <- if (exists("env") == T) {
    simpECO::select.envfit(vegan::envfit(NMDS, vec.env, perm = 1000), r.select = r.cutoff_env)
  }
  plot(NMDS, type = "n",xlab= "NMDS 1", ylab= "NMDS 2", cex = cex, cex.lab=cex.lab)
  if (group.col > 0) {
    with(env,points(NMDS, display = "sites", col=colvec[env$group], pch = pch))
  } else {
    with(veg, points(NMDS, display = "sites", pch = pch))
  }
  plot(SPEC, col = "black", cex = cex)
  if (exists("env") == T) {
    plot(ENV, col = "gray65", cex = cex)
  }
  if (group.col > 0) {
    legend("topright", legend = levels(env$group), col=colvec, lty = lty, lwd = lwd, cex = cex.leg)
  }
  if (ordihull == T) {
    with(env, vegan::ordihull(NMDS, group, col=colvec, lty = lty, lwd = lwd))
  }
}
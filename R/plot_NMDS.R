#' @export
plot_NMDS <- function(veg, env, group.col = 0, r.cutoff_env = 0.3, r.cutoff_spec = 0.3, colvec,
                     pch = 20, lty = 1, lwd = 1, cex = 1, cex.lab = 1,  cex.leg = 1, ordihull = F){
  if (group.col > 0) {
    names(env)[group.col] <- "group"
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
  SPEC <- UReco::select.envfit(vegan::envfit(NMDS, veg, perm = 1000), r.select = r.cutoff_spec)
  ENV <- if (exists("env") == T) {
    UReco::select.envfit(vegan::envfit(NMDS, vec.env, perm = 1000), r.select = r.cutoff_env)
  }
  plot(DCA, type = "n",xlab= "NMDS 1", ylab= "NMDS 2", cex = cex, cex.lab=cex.lab)
  if (group.col > 0) {
    with(env,points(DCA, display = "sites", col=colvec[env$group], pch = pch))
  } else {
    with(veg, points(DCA, display = "sites", pch = pch))
  }
  plot(SPEC, col = "black", cex = cex)
  if (exists("env") == T) {
    plot(ENV, col = "gray65", cex = cex)
  }
  if (group.col > 0) {
    legend("topright", legend = levels(env$group), col=colvec, lty = lty, lwd = lwd, cex = cex.leg)
  }
  if (ordihull == T) {
    with(env, vegan::ordihull(DCA, group, col=colvec, lty = lty, lwd = lwd))
  }
}

#' Produces a boxplot for more than two groups
#' 
#' Produces a boxplot and performs a Kruskal-Wallis-Test with a posthoc Dunn’s Test. The letters are showing a significant difference with a p value of < 0.05.
#' 
#' @param env table of data (must be numeric)
#' @param group.col column of the group info (must be a factor)
#' @param col.sel possible selection of column
#' 
#' @note A tie correction will be employed according to Glantz (2012).\cr
#' Glantz SA (2012). \emph{Primer of biostatistics}. 7 edition. McGraw Hill, New York.
#' 
#' @examples 
#' results <- env_boxplot(env = iris, group.col = 5, col.sel = c(1:3))
#' results
#' 
#' @export
env_boxplot <- function(env, group.col = 0, col.sel = NULL){
  if (group.col == 0){
    stop('column of the groups is missing')
  } else {
    group <- env[group.col]
    env <- env[-c(group.col)]
  }
  if (is.null(col.sel) == F){
    env <- env[col.sel-1]
    env_Obs <- colnames(env)
  } else {
    env_Obs <- colnames(env)
  }
  X <- ncol(env)
  result <- NULL
  for(i in 1:X) {
    use <- data.frame(env[i], group)
    colnames(use) <- c("Var", "Group")
    out <- PMCMR::posthoc.kruskal.dunn.test(Var~Group,
                                            data = use,
                                            p.adjust="bonf")
    out.p <- PMCMR::get.pvalues(out)
    out.mcV <- multcompView::multcompLetters(out.p, threshold = 0.05)
    letter <- data.frame(out.mcV$Letters)
    a <-boxplot(use$Var~use$Group, ylab = env_Obs[i],
                ylim=c(min(use$Var, na.rm = T) , 1.1*max(use$Var, na.rm = T))
                ,las=1)
    over <- 0.1*max( a$stats[nrow(a$stats),] )
    text(c(1:nlevels(use$Group)) , a$stats[nrow(a$stats),]+over , letter[,1])
    p <- format.pval(out.p, digits = 2, eps = 0.001, nsmall = 3)
    lin <- c(env_Obs[i],p)
    result <- rbind(result, lin)
    result_p.value <- as.data.frame(result)
    rownames(result_p.value) <- result_p.value[,1]
    result_p.value <- result_p.value[,-1]
    colnames(result_p.value) <- names(out.p)
  }
  return(result_p.value)
}

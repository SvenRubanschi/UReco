#' Produces a boxplots for more than two groups
#' 
#' Produces boxplots and performs a Kruskal-Wallis-Test with a posthoc Dunn’s Test. The letters are showing a significant difference with a p value of < 0.05.
#' 
#' @param env table of data (must be numeric)
#' @param group.col column of the group info (must be a factor)
#' @param col.sel possible selection of column (with or without the group.col)
#' 
#' @note A tie correction will be employed according to Glantz (2012).\cr
#' Glantz SA (2012). \emph{Primer of biostatistics}. 7 edition. McGraw Hill, New York.
#' 
#' @examples 
#' results <- env_boxplot_kurskal(env = iris, group.col = 5, col.sel = c(1:3))
#' results
#' 
#' @export
env_boxplot_kruskal <- function(env, group.col = 0, col.sel = NULL){
  if (group.col == 0){
    stop('column of the group information is missing')
  } else {
    names(env)[group.col] <- "group"
    env$group <- as.factor(env$group)
    env$group <- droplevels(env$group)
  }
  if (nlevels(env$group) < 3){
    stop('less than 3 groups')
  } else {
    group <- env[group.col]
  }
  if (is.null(col.sel) == F){
    env <- env[col.sel]
    if ("group" %in% colnames(env)){
      env <- env[,-which(names(env) == "group")]
      env_Obs <- colnames(env)
    } else {
      env_Obs <- colnames(env)
    }
  } else {
    env <- env[-group.col]
    env_Obs <- colnames(env)
  }
  X <- ncol(env)
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
    mean_val <- aggregate(use[,1], list(use$Group), mean, na.rm = T)
    sd_val <- aggregate(use[,1], list(use$Group), sd, na.rm = T)
    block <- as.data.frame(c(mean_val,letter[1],sd_val[2]), row.names = 1)
    result_row <- t(t(block)[,1])
    for (j in 2:nlevels(group$group)) {
      result_row <- cbind(result_row, t(t(block)[,j]))
    }
    rownames(result_row)<- colnames(env[i])
    for (j in 0:I(nlevels(group$group)-1)) {
      colnames(result_row)[1+3*j] <- paste(levels(group$group)[j+1], "_mean", sep="")
      colnames(result_row)[2+3*j] <- paste(levels(group$group)[j+1], "_letter", sep="")
      colnames(result_row)[3+3*j] <- paste(levels(group$group)[j+1], "_sd", sep="")
    }
    p.val <- kruskal.test(Var~Group, data = use)
    p.val_s <- format.pval(p.val$p.value, digits = 2, eps = 0.001, nsmall = 3)
    result_row <- as.data.frame(result_row)
    result_row$p.value_Krusal_Wallis <- p.val_s
    result_row$chi_squared_Krusal_Walli <- format(round(p.val$statistic[[1]], 2), nsmall = 2)
    if (i == 1) {
      result <- result_row
    } else {
      result <- rbind(result, result_row)
    }
  }
  return(result)
}
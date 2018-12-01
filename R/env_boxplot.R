#' Produces a boxplot for more than two groups
#' 
#' Produces a boxplot and performs a Kruskal-Wallis-Test with a posthoc Dunn’s Test. The letters are showing a significant difference with a p value of < 0.05.
#' 
#' @param env table od data
#' @param group.col column of the group info 
#' @param col.sel possible selection of column
#' 
#' @note A tie correction will be employed according to Glantz (2012).\cr
#' Glantz SA (2012). \emph{Primer of biostatistics}. 7 edition. McGraw Hill, New York.
#' 
#' @examples 
#' results <- env_boxplot(env = iris,group.col = 5, col.sel = c(1:3))
#' results
#' 
#' @export
env_boxplot <- function(env, group.col = 0, col.sel = NULL){
  if (group.col == 0){
    stop('column of the groups is missing')
  } else {
    env[,group.col] <- as.factor(env[,group.col])
    group <- env[group.col]
    env <- env[-c(group.col)]
  }
  if (is.null(col.sel) == F){
    env <- env[col.sel-1] # important to write in the help the selection is like no group is in the data set
    env_Obs <- colnames(env)
  } else {
    env_Obs <- colnames(env)
  }
  X <- ncol(env)
  result <- list()
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
    result[[i]] <- c(env_Obs[i],out.p)
    result_p.value <- as.data.frame(do.call(rbind, result))
    trans <- sapply(result_p.value, is.factor)
    trans[1] <- FALSE
    result_p.value[trans] <- lapply(result_p.value[trans], function(x) as.numeric(as.character(x)))
    colnames(result_p.value)[1]<- "Variables"
    is.num <- sapply(result_p.value, is.numeric)
    result_p.value[is.num] <- lapply(result_p.value[is.num], round, 3)
  }
  return(result_p.value)
}

#' Produces boxplots for two groups
#' 
#' Produces boxplots and performs a U-Test or a t-Test. 
#' 
#' @param env table of data (must be numeric)
#' @param type selection of test (“u.test”, “t.test”)
#' @param group.col column of the group info (must be a factor)
#' 
#' @note Information about the U-Test read \code{\link[stats]{wilcox.test}} and the t-Test read \code{\link[stats]{t.test}}
#' 
#' @examples 
#' dat <- iris[1:100,]
#'
#' results <- env_boxplot_2G(env = dat, type = "u.test", group.col = 5)
#' results
#' 
#' @export
env_boxplot_2G <- function(env, type = NULL, group.col = 0){
  if (group.col == 0){
    stop('column of the group information is missing')
  } else {
    names(env)[group.col] <- "group"
    env$group <- as.factor(env$group)
    env$group <- droplevels(env$group)
  }
  if (nlevels(env$group) > 2){
    stop('more than 2 groups')
  } else {
    group <- env[group.col]
    G1 <- env[which(env$group == levels(env$group)[1]),]
    G2 <- env[which(env$group == levels(env$group)[2]),]
    env <- env[-group.col]
    G1 <- G1[-group.col]
    G2 <- G2[-group.col]
  }
  if (is.null(type) == T){
    stop('select test!')
  }
  if (type == "u.test"){
    # U-Test
    for (i in 1:ncol(env)) {
      if (i == 1) {
        u_result <- wilcox.test(env[,i] ~ group[,1]) 
        result <- c(u_result$p.value, format.pval(u_result$p.value, digits = 2, eps = 0.001, nsmall = 3),
                    mean(G1[,i]), sd(G1[,i]), mean(G2[,i]), sd(G2[,i]))
      } else {
        u_result <- wilcox.test(env[,i] ~ group[,1])
        result <- rbind(result,c(u_result$p.value, format.pval(u_result$p.value, digits = 2, eps = 0.001, nsmall = 3),
                                 mean(G1[,i]), sd(G1[,i]), mean(G2[,i]), sd(G2[,i])))
      }
    }
  }
  if (type == "t.test"){
    # t-Test
    for (i in 1:ncol(env)){
      if (i == 1){
        t.result <- t.test(env[,i] ~ group[,1])
        result <- result <- c(t_result$p.value, format.pval(t_result$p.value, digits = 2, eps = 0.001, nsmall = 3),
                              mean(G1[,i]), sd(G1[,i]), mean(G2[,i]), sd(G2[,i]))
      } else {
        t.result <- t.test(env[,i] ~ group[,1])
        result <- rbind(result,c(u_result$p.value, format.pval(u_result$p.value, digits = 2, eps = 0.001, nsmall = 3),
                                 mean(G1[,i]), sd(G1[,i]), mean(G2[,i]), sd(G2[,i])))
      }
    }
  }
  
  result <- as.data.frame(result)
  colnames(result) <- c("p.val", "p.val_short", 
                        paste("mean_", levels(group$group)[1], sep = ""), paste("sd_", levels(group$group)[1], sep = ""),
                        paste("mean_", levels(group$group)[2], sep = ""), paste("sd_", levels(group$group)[2], sep = ""))
  rownames(result) <- colnames(env)[1:ncol(env)]
  result$p.val <- as.numeric(as.character(result$p.val))
  result$p.star <- unclass(symnum(result$p.val, corr = FALSE, na = FALSE, 
                                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                                  symbols = c("***", "**", "*", ".ns", "ns")))
  result <- result[c(1,2,7,3,4,5,6)]
  for (i in 1:ncol(env)) {
    boxplot(env[,i]~group[,1], ylab = colnames(env)[i])
    over <- quantile(env[,i])[[5]]
    text(x = 1.5, y = over , labels = result$p.star[i])
  }
  return(result)
}


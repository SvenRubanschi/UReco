#' Hierarchical Cluster (Dendrogram)
#'
#' Creates a hierachical dendrogram. It is also possible to colour the leaves by group.
#'
#' @param veg vegetation matrix
#' @param method method of the dissimilarity index
#' @param env environment matrix with the group information
#' @param group.col column in which the group is located
#' @param colvec colour vector of the groups; like c("col1","col2","col3") or c(1,2,3)
#' @param pch point vector of the groups; like c(1,2,3)
#' @param cex scales plot
#' @param lab.cex scales of label
#' 
#' @note Information about the different dissimilarity indexes read \code{\link[vegan]{vegdist}}
#' 
#' @examples
#' library(vegan)
#' data(dune)
#' data(dune.env)
#' 
#' # Simple dendrogram
#' den <- dendro(veg = dune)
#' plot(den, horiz = TRUE)
#' 
#' # Dendrogram coloured by group
#' Cvec = c("black", "gray20", "gray40", "gray80")
#' Pch = c(15, 16, 17, 18)
#' 
#' den <- dendro(veg = dune, env = dune.env, group.col = 3, colvec = Cvec, pch = Pch, cex = 2)
#' plot(den, leaflab = "none", horiz = TRUE)
#'  
#' @export
dendro <- function(veg, method = "bray", env = NULL, group.col = NULL, colvec = "black", pch = 20 , cex = 1, lab.cex = 0.7){
  if(is.null(env)){
    if(is.null(group.col)){
      den_col = FALSE
    }else{
      stop("environment matrix is missing")
    }
  }else{
    if(is.null(group.col)){
      stop("information of group column is missing")
    }else{
      den_col = TRUE
    }
  }
  dis <- vegan::vegdist(veg, method = method)
  clua <- hclust(dis, "average")
  den <- as.dendrogram(clua)
  if(den_col == FALSE){
    return(den)
  }
  if(den_col == TRUE){
    colnames(env)[group.col] <- "group"
    if(length(colvec) == 1){
      colvec <- grDevices::rainbow(nlevels(env$group))
    }
    if(length(pch) == 1){
      pch <- rep(1:nlevels(env$group))
    }
    plotname <- as.data.frame(rownames(veg))
    colnames(plotname) <- "plot"
    group <- as.data.frame(cbind(rownames(env), env$group))
    colnames(group) <- c("plot", "group")
    den_group <- merge(plotname, group, by = "plot")
    order_den <- den_group$group[order.dendrogram(den)]
    colvec_grouped <- colvec[order_den]
    pchvec_grouped <- pch[order_den]
    local({
      colLab <<- function(n) {
        if(is.leaf(n)) {
          a <- attributes(n)
          i <<- i+1
          attr(n, "nodePar") <-
            c(a$nodePar, list(lab.col = colvec_grouped[i], lab.cex = lab.cex,
                              col = colvec_grouped[i], cex = cex, pch = pchvec_grouped[i]))
        }
        n
      }
      i <- 0
    })
    den <- dendrapply(den, colLab)
    return(den)
  }
}

#' Transforming expended Braun-Blanquet-Categories
#'
#' Transforms the expended Braun-Blanquet-Categories of a vegetation data frame
#' into percentage
#'
#' @param veg vegetation data frame
#'
#'
#' @note Changes the Braun-Blanquet-Categories in to following proportions: \cr
#' r --> 0.01\% \cr
#'  + --> 0.5\% \cr
#'  1 --> 2.5\% \cr
#'  2a --> 8.8\% \cr
#'  2b --> 20\% \cr
#'  3 --> 37.5\% \cr
#'  4 --> 62.5\% \cr
#'  5 --> 87.5\% \cr
#'
#' @examples
#' NOT RUN
#'
#' @export
veg_trans <- function(veg){
  name <- row.names(veg)
  veg[] <- lapply(veg, factor)
  veg <- as.data.frame(lapply(veg, function(x) {
    levels(x)[levels(x) %in% c("r")] <- "0.01"
    x }))
  veg <- as.data.frame(lapply(veg, function(x) {
    levels(x)[levels(x) %in% c("+")] <- "0.5"
    x }))
  veg <- as.data.frame(lapply(veg, function(x) {
    levels(x)[levels(x) %in% c("1")] <- "2.5"
    x }))
  veg <- as.data.frame(lapply(veg, function(x) {
    levels(x)[levels(x) %in% c("2a")] <- "8.8"
    x }))
  veg <- as.data.frame(lapply(veg, function(x) {
    levels(x)[levels(x) %in% c("2b")] <- "20"
    x }))
  veg <- as.data.frame(lapply(veg, function(x) {
    levels(x)[levels(x) %in% c("3")] <- "37.5"
    x }))
  veg <- as.data.frame(lapply(veg, function(x) {
    levels(x)[levels(x) %in% c("4")] <- "62.5"
    x }))
  veg <- as.data.frame(lapply(veg, function(x) {
    levels(x)[levels(x) %in% c("5")] <- "87.5"
    x }))
  veg[] <- sapply(veg, as.character)
  veg[] <- sapply(veg, as.numeric)
  rownames(veg) <- name
  return(veg)
}
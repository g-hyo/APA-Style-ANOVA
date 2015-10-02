APAanova <- function(x, y) {
  result <- summary(aov(x~y))
  p <- result[[1]]$'Pr(>F)'[1]
  F <- result[[1]]$`F value`[1]
  df <- result[[1]]$Df[1:2]
  cat(sprintf("F(%.0f,%.0f)=%.3f, p=%.3f", df[1],df[2],F,p))
}

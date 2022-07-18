#" package
#' @param x the data set in xts format
#' @param T give a the title for the plot
#' @return a plot for garch model
#' @export

plotgarch <- function (x) {

library(xts)
alpha <- 0.1
beta <- 0.8
omega <- var(x)*(1-alpha-beta)
e <- x-mean(x)
e2 <- e^2
nobs <- length(x)
predvar <- rep(NA,nobs)
predvar[1] <- var(x)
for (t in 2:nobs) {
  predvar[t] <- omega + alpha * e2[t-1] +beta * predvar[t-1]
}
predvol <- sqrt(predvar)
predvol <- xts(predvol, order.by = index(x))
colnames(predvol)<- c("value")
predvol |>
as.data.frame() |>
  tibble::rownames_to_column("Date") |>
dplyr::mutate(Date = as.Date(Date, format = "%Y-%m-%d")) |>
  ggplot2::ggplot(aes(Date, value)) +
geom_line() +
scale_x_date(
date_breaks = "1 month",
labels = scales::date_format("%Y")) +
theme_minimal(base_size = 20) + labs(title = "",
                           subtitle = "Garch(1,1)")


}


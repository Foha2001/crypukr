
#'plot garch model
#'
#' @param x the data set in xts format
#' @param T give a the title for the plot
#'
#' @return
#' @export

plotgarch <- function (x,T) {

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
predvol <- xts(predvol, order.by = index(R_data))
#plot(predvol, type='l', main=" Litecoin Volatility")
colnames(predvol)<- c("value")
library(scales)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tibble)
predvol %>%
as.data.frame() %>%
rownames_to_column("Date") %>%
mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
ggplot(aes(Date, value)) +
geom_line() +
scale_x_date(
date_breaks = "1 month",
labels = date_format("%b\n%Y")) +
theme_minimal() + labs(title = T,
                           subtitle = "Garch(1,1)")









}


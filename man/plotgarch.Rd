% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_garch.R
\name{plotgarch}
\alias{plotgarch}
\title{plot Garch}
\usage{
plotgarch(x, T)
}
\arguments{
\item{x}{an xts format serie}

\item{T}{give a title for the plot within "" bracket}
}
\value{
Return a plot with Garch(1,1) model
}
\description{
plot Garch
}
\examples{
plotgarch(AA$AA.Adjusted,"Alcoa volatility")
}

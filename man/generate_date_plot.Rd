% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{generate_date_plot}
\alias{generate_date_plot}
\title{generate_date_plot}
\usage{
generate_date_plot(
  df,
  vars,
  colour = "black",
  scale_date,
  titles = c(x = "x", y = "y", main = "")
)
}
\arguments{
\item{df}{[[data.frame]] data}

\item{vars}{[[character]] named vector with variable shown on x axis and y axis}

\item{colour}{[[character]] colour name, by default `"black"`}

\item{scale_date}{[[character]] named vector with tick settings, breaks denote the time period and labels denote the date format}

\item{titles}{[[character]] name vector with labels for the x axis, y axis and plot title as main}
}
\value{
[[gg]]/[[ggplot]] ggplot object  1-row data.frame with the maximal value of the first chosen variable(and distant one in case of ties)
}
\description{
Function extracts the row with the maximal value
}

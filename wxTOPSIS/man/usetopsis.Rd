\name{usetopsis}
\alias{usetopsis}
\title{TOPSIS-based emergency plan optimization}
\usage{
usetopsis(content, weight, attribute)
}
\description{
Get the order of the plans and you can choose the best.
}
\examples{
content <- "datasrt.csv"
weight <- c(0.1,0.1,0.3,0.2,0.2,0.1)
attribute <- c(1,0,1,0,0,0)
usetopsis(content, weight, attribute)
}

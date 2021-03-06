% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_CostModelInfo.R
\name{get_CostModelInfo}
\alias{get_CostModelInfo}
\title{get_CostModelInfo}
\usage{
get_CostModelInfo(db_name, CostModelName = "PBB", db_con = NULL)
}
\arguments{
\item{db_name}{data base to use}

\item{CostModelName}{The costmodel name, FH or PBB. Typically PBB.}

\item{dbcon}{If a db con exists we can use it here.}
}
\description{
Get the CostModelInfo so we know the names of stuff
}
\examples{
CostModelInfo<-get_CostModelInfo(db_name='RX_Training5')
}

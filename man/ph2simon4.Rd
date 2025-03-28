% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/0S4.R, R/ph2simon4.R
\docType{class}
\name{ph2simon4}
\alias{ph2simon4}
\alias{ph2simon4-class}
\title{S4 Object \linkS4class{ph2simon4}}
\usage{
ph2simon4(object, type = "minimax", ...)
}
\arguments{
\item{object}{a \link[clinfun]{ph2simon} object}

\item{type}{\link[base]{character} scalar or \link[base]{vector}, see \strong{Slots}}

\item{...}{additional parameters, currently not in use}
}
\description{
One or more Simon's two-stage designs.
}
\section{Slots}{

\describe{
\item{\code{r1,r}}{non-negative \link[base]{integer} scalars or \link[base]{vector}s,
number of responses
in Stage-1 \eqn{r_1} and overall \eqn{r} required \emph{exclusively}, in other words
\itemize{
\item {pass Stage-1 if observed \eqn{>r_1} response;}
\item {reject \eqn{H_0} if observed \eqn{>r} responses.}
}}

\item{\code{n1,n}}{positive \link[base]{integer} scalars or \link[base]{vector}s,
Stage-1 sample size \eqn{n_1}
and total sample size \eqn{n}.}

\item{\code{pu,pa}}{\link[base]{double} scalars}

\item{\code{alpha,beta}}{\link[base]{double} scalars}

\item{\code{nmax}}{\link[base]{integer}}

\item{\code{type}}{\link[base]{character} scalars or \link[base]{vector}s,
type of Simon's two-stage design, one or more values among
\describe{
\item{\code{'minimax'}}{(default) minimum total sample size}
\item{\code{'optimal'}}{minimum expected total sample size \emph{under \eqn{p_0}}}
\item{\code{'n1'}}{minimum Stage-1 sample size}
\item{\code{'maximax'}}{to use up the user-provided maximum total sample size,
i.e., parameter \code{nmax} of function \link[clinfun]{ph2simon}}
}}
}}

\keyword{internal}

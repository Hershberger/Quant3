% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/potential_outcomes.R
\name{simulate_observational_study}
\alias{simulate_observational_study}
\title{Simulate an observational study}
\usage{
simulate_observational_study(n, seed = sample.int(.Machine$integer.max, 1))
}
\arguments{
\item{n}{number of observations}

\item{seed}{RNG seed}

\item{prob}{probability}
}
\value{
data.table with potential outcomes
}
\description{
Simulates potential outcomes and adds a treatment variable, assumes SUTVA,
defines the observed outcomes, deletes potential outcomes, and returns the
results
}


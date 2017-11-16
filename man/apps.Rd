\name{apps}
\alias{design}
\alias{conduct}
\title{
Shiny GUIs for model-based dose-escalation studies
}
\description{
A user-friendly tool to design and evaluate phase I cancer clinical trials, with the aim to estimate the maximum tolerated dose (MTD) of a novel drug. This is a point-and-click implementation of the dose-escalation study design proposed by Zhou & Whitehead (2003) that uses a Bayesian logistic regression method. The graphical user interfaces (GUIs) are based on R's Shiny system.
}
\usage{
design()
conduct()
}
\details{
This package contains two separate modules:

1) The \code{design} module allows to investigate different design options and parameters, and to simulate their operating characteristics under various scenarios. Type \code{design()} and the GUI will open in a browser window.

2) The \code{conduct} module provides guidance for dose selection throughout the study, and a recommendation for the MTD at the end. Type \code{conduct()} and the GUI will open in a browser window.

Both modules generate a variety of graphs to visualise data and design properties, and create downloadable PDF reports of simulation results and study data analyses.
}
\references{
Zhou Y, Whitehead J (2003) Practical implementation of Bayesian dose-escalation procedures. \emph{Drug Information Journal}, \strong{37}(1), 45--59.
}
\author{
Philip Pallmann (\email{pallmannp@cardiff.ac.uk})
}
\examples{\donttest{
design()
conduct()
}}
\keyword{misc}
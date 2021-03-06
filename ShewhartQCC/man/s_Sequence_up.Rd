% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/form_sequense.R
\name{s_Sequence_up}
\alias{s_Sequence_up}
\title{"1" and "0" sequense receiving}
\usage{
s_Sequence_up(sample, operand)
}
\arguments{
\item{sample}{- numeric vector}

\item{operand}{- constant numeric value}
}
\value{
numeric the estimated amount of good honey
}
\description{
Comparing inputting numeric sequense with constant operand.
Receiving logic sequense of comparation.
Then transforming into "1" and "0" sequense

Functions return "1" and "0" string, type char. Returns "1" if inputting
value greater than constant operand. Returns "0" if inputting
value lower than constant operand.
}
\examples{
const <- 5
seq <- c(1:10)
s_Sequence_up(seq, const)
#"0000011111"
}

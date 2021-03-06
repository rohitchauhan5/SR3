% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sr3.R
\name{sr3}
\alias{sr3}
\title{SR3}
\usage{
sr3(A, b, ...)
}
\arguments{
\item{A}{double precision real or complex matrix (dimension, say, MxN)}

\item{b}{double precision real or complex vector (length M)}

\item{...}{Optional arguments}
}
\description{
SR3 Relaxed pursuit method for regularized least squares problems
of the form:
   \code{0.5*norm(A*x-b,2)^2 + lam*R(w) + 0.5*kap*norm(C*x-w,2)^2}
over x and w. The output w represents a regularized solution of
the least squares problem described by A and b.
}

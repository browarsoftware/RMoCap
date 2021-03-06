% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bvhutils.R
\name{set.data.frame}
\alias{set.data.frame}
\title{Assign data frame to object of mocap class.}
\usage{
set.data.frame(skel, df)
}
\arguments{
\item{skel}{object of mocap class.}

\item{df}{data frame with columns names compatible to hierarchical model definition in skel object.}
}
\value{
object of mocap class with df assigned.
}
\description{
This function does not recalculate hierarchical kinematic model in skeleton.
}
\examples{
data("header.mocap")
data("heian.shodan")
print(header.mocap$skeleton$Frames)
original.bvh <- set.data.frame(header.mocap, heian.shodan)
print(original.bvh$skeleton$Frames)
}

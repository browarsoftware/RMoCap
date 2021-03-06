% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rotatedata.R
\name{rotatedata}
\alias{rotatedata}
\title{This function align two data frames that contains mocap data.}
\usage{
rotatedata(mydata, referencedata, v1, v2)
}
\arguments{
\item{mydata}{input data frame with mocap data to be algined to referencedata.}

\item{referencedata}{reference data frame with mocap data.}

\item{v1}{name of the first body joint.}

\item{v2}{name of the second body joint.}
}
\value{
mydata rotated by Y axis so that mydata and referencedata faces same direction.
}
\description{
Both data frames need to have two common column groups with names ending Dx and Dz. This function calculates vector vv = v1-v2
for both data frames (vv.m for mydata and vv.ref for referencedata) and rotates mydata around Y axis in order to minimize euclidean distance between
vv.m and vv.ref. Minimization is made with simplex method. After this procedure mydata face the same direction as referencdata.
This procedure works correctly only if root joint of mocap is stationary.
}
\examples{
data(mawashi.geri.left.1)
data(mawashi.geri.left.2)
refdata <- mawashi.geri.left.1$data.frame
inputdata <- mawashi.geri.left.2$data.frame
#after following function inputdata and refdata are alignined towards vector LeftThigh - RightThigh
inputdataalignment <- rotatedata(inputdata, refdata, "LeftThigh","RightThigh")


}

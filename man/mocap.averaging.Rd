% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/dba_quaternion_averaging_speed_up_all_techniques.R
\name{mocap.averaging}
\alias{mocap.averaging}
\title{This function averages a list of motion capture recordings.}
\usage{
mocap.averaging(myList, DBAIterationsCount = 50, eps = 1e-04,
  plot.me = TRUE)
}
\arguments{
\item{myList}{list of mocap data frames. Algorithm uses columns with names that has .Rx, .Ry and .Rz names. Rotation should be represented
by Euler angles in degrees.}

\item{DBAIterationsCount}{maximal number of iterations of DBA algorithm (default value is DBAIterationsCount = 50).}

\item{eps}{threshold value for DBA - iteration stops when absolute value of difference between normalized DTW distances on this and previous iteration is less than eps (default value is eps = 0.0001).}

\item{plot.me}{if TRUE, plots DTW distances for each averaged signal (default value is plot.me = 50).}
}
\value{
return object of class averaged.mocap.
}
\description{
Averaging is performed on each rotation channel of hierarchical model separately with Dynamic Time Warping barycentre averaging (DBA), see:
François Petitjean, Alain Ketterlin, PierreGançarski,
"A global averaging method for dynamic time warping, with applications to clustering",
Pattern Recognition, Volume 44, Issue 3, March 2011, Pages 678-693, https://doi.org/10.1016/j.patcog.2010.09.013
Each rotation signal holds rotation represented as quaternion. Quaternion averaging is performed with Quaternion Markley averaging algorithms, see: .
F. Landis Markley, Yang Cheng, John Lucas Crassidis, and Yaakov Oshman.  "Averaging Quaternions", Journal of Guidance, Control, and Dynamics, Vol. 30, No. 4 (2007), pp. 1193-1197. https://doi.org/10.2514/1.28949
Results are smoothed with Weighted Quaternion Markley averaging algorithms using Gaussian kernel.
}
\examples{
 #load list of objects of mocap class
 data("mawashi.geri.right.list")
 myList <- list()
 #assign data frames to list
 for (a in 1:length(mawashi.geri.right.list))
 {
   myList[[a]] <-mawashi.geri.right.list[[a]]$data.frame
 }
 #run compiled version of mocap.averaging function
 res.data <- mocap.averagingCmp(myList, 50, eps = 0.000001)
 plot(res.data)
 #write results to disc as bvh file
 skel <- set.data.frame(mawashi.geri.right.list[[1]], res.data$fullData)
 write.bvh(path = "avg.mawashi.geri.right.bvh", skeleton.helper = skel)
}

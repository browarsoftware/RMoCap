#helper function
generateroty <- function(angley)
{
  ry <- matrix(nrow=3, ncol=3)
  recangley <- angley * 2 * pi / 360
  ry[1,1] <- cos(recangley)
  ry[1,2] <- 0
  ry[1,3] <- sin(recangley)

  ry[2,1] <- 0
  ry[2,2] <- 1
  ry[2,3] <- 0

  ry[3,1] <- -sin(recangley)
  ry[3,2] <- 0
  ry[3,3] <- cos(recangley)

  return (ry)
}

#' This function align to data frames that contains mocap data.
#'
#' Both data frames need to have two common column groups with names ending Dx and Dz. This function calulates vector vv = v1-v2
#' for both data frames (vv.m for mydata and vv.ref for referencedata) and rotates mydata around Y axis in order to minimize euclidean distane between
#' vv.m and vv.ref. Minimization is perfomed with simplex method. After this procedure mydata face the same direction as referencdata.
#' This procedure works correctly only if root joint of mocap is stationary.
#'
#' @param mydata input data frame with mocap data to be algined to referencedata.
#' @param referencedata reference data frame with mocap data.
#' @param v1 name of the first body joint.
#' @param v2 name of the second body joint.
#'
#' @return mydata rotated by Y axis so that mydata and referencedata faces same direction.
#'
#' @examples
#' data(mawashi.geri.left.1)
#' data(mawashi.geri.left.2)
#' refdata <- mawashi.geri.left.1$data.frame
#' inputdata <- mawashi.geri.left.2$data.frame
#' #after following function inputdata and refdata are alignined towards vector LeftThigh - RightThigh
#' inputdataalignment <- rotatedata(inputdata, refdata, "LeftThigh","RightThigh")
#'
#'
rotatedata <- function(mydata, referencedata, v1, v2)
{
  require('subplex')
  v1.x <- paste(v1,".Dx", sep = "")
  v1.z <- paste(v1,".Dz", sep = "")

  v2.x <- paste(v2,".Dx", sep = "")
  v2.z <- paste(v2,".Dz", sep = "")

  columnsToFind <- colnames(mydata)[grep("[[:alnum:]]+\\.Dx", colnames(mydata), ignore.case = TRUE)]
  columnsToFind <- c(columnsToFind, colnames(mydata)[grep("[[:alnum:]]+\\.Dy", colnames(mydata), ignore.case = TRUE)])
  columnsToFind <- c(columnsToFind, colnames(mydata)[grep("[[:alnum:]]+\\.Dz", colnames(mydata), ignore.case = TRUE)])

  columnsToFindNoEnds <- list()
  for (a in 1:length(columnsToFind))
  {
    columnsToFindNoEnds[a] <- substring(text = columnsToFind[a], first = 1, last = nchar(columnsToFind[a]) - 3)
  }
  columnsToFindNoEnds <- unique(unlist(columnsToFindNoEnds))

  allResults <- list()
  allResultsColnames <- list()
  Time <- (1:length(mydata[,v2.x])) / 100

  df <- data.frame(Time = Time)


  #v1 <- c(mydata$LeftFoot.Dx[1] - mydata$Hips.Dx[1], 0, mydata$LeftFoot.Dz[1] - mydata$Hips.Dz[1])
  #v2 <- c(referencedata$LeftFoot.Dx[1] - referencedata$Hips.Dx[1], 0, referencedata$LeftFoot.Dz[1] - referencedata$Hips.Dz[1])

  v1 <- c(mydata[1,v1.x] - mydata[1,v2.x],
          0,
          mydata[1,v1.z] - mydata[1,v2.z])
  v2 <- c(referencedata[1,v1.x] - referencedata[1,v2.x],
          0,
          referencedata[1,v1.z] - referencedata[1,v2.z])


  euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

  optimizeangle <- function(x)
  {
    my <- generateroty(x)
    v11 <- my %*% v1
    #v11 <- rotres[1,]
    return (euc.dist(v11, v2))
  }
  response <- subplex(par=c(0),fn=optimizeangle)
  response$par

  #euc.dist(v1, v2)
  #euc.dist(my %*% matrix(v1), v2)

  #my <- generateroty(response$par)
  #v1
  #my %*% matrix(v1)
  #v2

  #vec1 <- rotres[1,]

  #v1
  #v2

  #columnsToFind

  xx <- grep("[[:alnum:]]+\\.Dx", colnames(mydata), ignore.case = TRUE)[1]
  yy <- grep("[[:alnum:]]+\\.Dy", colnames(mydata), ignore.case = TRUE)[1]
  zz <- grep("[[:alnum:]]+\\.Dz", colnames(mydata), ignore.case = TRUE)[1]

  tt <- colnames(mydata[xx])
  xx <- substring(tt,nchar(tt)-2,nchar(tt))
  tt <- colnames(mydata[yy])
  yy <- substring(tt,nchar(tt)-2,nchar(tt))
  tt <- colnames(mydata[zz])
  zz <- substring(tt,nchar(tt)-2,nchar(tt))

  for (a in 1:length(columnsToFindNoEnds))
  {
    #a = 2
    xx1 <- paste(columnsToFindNoEnds[a] , xx, sep = "")
    yy1 <- paste(columnsToFindNoEnds[a] , yy, sep = "")
    zz1 <- paste(columnsToFindNoEnds[a] , zz, sep = "")

    xxx <- list()
    yyy <- list()
    zzz <- list()

    for (b in 1:length(Time))
    {
      vec1 <- c(mydata[b,xx1], mydata[b,yy1], mydata[b,zz1])
      my <- generateroty(response$par)
      #rotres <- my * vec1
      vv <- as.vector(my %*% matrix(vec1))

      xxx[[b]] <- vv[1]
      yyy[[b]] <- vv[2]
      zzz[[b]] <- vv[3]
    }
    df[xx1] <- unlist(xxx)
    df[yy1] <- unlist(yyy)
    df[zz1] <- unlist(zzz)
  }
  return (df)
}

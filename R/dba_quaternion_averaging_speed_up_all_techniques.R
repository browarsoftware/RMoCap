############################################################
#Important functions for DBA and quaternions smoothing
############################################################

#' Quaternion Markley averaging algorithms.
#'
#' See: F. Landis Markley, Yang Cheng, John Lucas Crassidis, and Yaakov Oshman.  "Averaging Quaternions", Journal of Guidance, Control, and Dynamics, Vol. 30, No. 4 (2007), pp. 1193-1197. https://doi.org/10.2514/1.28949
#' @param Q a data frame of quaternions (four dimensional vectors) to be averaged. Each row of data frame holds one quaternion.
#'
#' @return 4D quaternion vector.
#'
#' @examples
#' Q <- data.frame(c(0.9999986, 0.9999986, 0.9999986, 0.9999986, 0.9999986, 0.9999986),
#' c(0.0008716584, 0.0008716584, 0.0009590162, 0.0009590162, 0.001046359, 0.001046359),
#' c(0.0009608439, 0.001048034, 0.0008736689, 0.001048034, 0.0009608439, 0.0008736689),
#' c(0.001046359, 0.0009590162, 0.001046359, 0.0008716584, 0.0008716584, 0.0009590162))
#' avg.quaternion.markley(Q)
avg.quaternion.markley <- function(Q)
{
  q <- t(as.matrix(Q[1,]))

  q %*% t(q)

  A <- rep(0, 16)
  A <- matrix(A, 4)
  M <- nrow(Q)
  for (a in 1:M)
  {
    q <- t(as.matrix(Q[a,]));
    A <- (q %*% t(q)) + A #rank 1 update
  }
  A <- (1.0 / M) * A

  return(eigen(A)$vectors[,1])
}

library(compiler)
avg.quaternion.markleyCmp <- cmpfun(avg.quaternion.markley)

#helper function
ArgMin3 <- function(a,b,c)
{
  if (a<b)
  {
    if (a<c)
    {
      return (0)
    }
    else
    {
    return (2)
    }
  }
  else
  {
    if (b<c)
    {
      return (1)
    }
    else
    {
      return (2);
    }
  }
  return (0)
}

library(compiler)
ArgMin3Cmp <- cmpfun(ArgMin3)

#helper function
distanceTo <- function(a,b)
{
  dist <- (a-b)*(a-b)
  return (dist)
}

#helper function
norm_vec <- function(x) sqrt(sum(x^2))
#helper function
dot_prod <- function(x, y) sum(x * y)
#helper function
quat_similarity2 <- function (x, y)
{
  dot = dot_prod(x, y)
  if(dot < 0.0)
  {
    dot = dot_prod(x, -y)
  }
  return (1 - dot)
}
#helper function
quat_similarity <- function (x, y)
{
  return (1 - abs(sum(x * y)))
}

library(compiler)
quat_similarityCmp <- cmpfun(quat_similarity)

#helper function
getSingleSIgnal <- function(all_quat, id)
{
  ss <- list()
  for (a in 1:length(all_quat))
  {
    ss[[a]] <- all_quat[[a]][id,]
  }
  return(ss)
}

#helper function
DBA_one_iteration <- function(averageS,sequences)
{
  tupleAssociation <- list();
  for (t in 1:length(averageS))
    tupleAssociation[[t]] <- data.frame(v1 = numeric(),
                                        v2 = numeric(),
                                        v3 = numeric(),
                                        v4 = numeric(),
                                        stringsAsFactors = FALSE);


  sl <- length(sequences) * length(sequences)
  seq1 <- rep(0, sl)

  costMatrix <- matrix(seq1, length(sequences))
  pathMatrix <- matrix(seq1, length(sequences))

  numberOfSignals <- nrow(as.data.frame(sequences[1]))

  for (k in 1:numberOfSignals)
  {
    sequence <- getSingleSIgnal(sequences, k)

    costMatrix[1,1] <- quat_similarityCmp(unlist(averageS[1]),unlist(sequence[1]))
    pathMatrix[1,1] <- -1;


    for (i in 2:length(averageS))
    {
      costMatrix[i,1] <- costMatrix[i-1,1] + quat_similarityCmp(unlist(averageS[i]),unlist(sequence[1]));
      pathMatrix[i,1] <- 2;
    }

    for (j in 2:length(sequence))
    {
      costMatrix[1,j] <- costMatrix[1,j-1] + quat_similarityCmp(unlist(sequence[j]),unlist(averageS[1]));
      pathMatrix[1,j] <- 1;
    }


    for (i in 2:length(averageS))
    {
      for (j in 2:length(sequence))
      {
        indiceRes <- ArgMin3Cmp(costMatrix[i-1,j-1],costMatrix[i,j-1],costMatrix[i-1,j]);
        pathMatrix[i,j] <- indiceRes;

        if (indiceRes==0)
        {
          res <- costMatrix[i-1,j-1];
        }
        else if (indiceRes==1)
        {
          res <- costMatrix[i,j-1]
        }
        else if (indiceRes==2)
        {
          res <- costMatrix[i-1,j]
        }
        costMatrix[i,j] <- res + quat_similarityCmp(unlist(averageS[i]),unlist(sequence[j]))

      }
    }
    i <- length(averageS)
    j <- length(sequence)


    while(TRUE)
    {
      ttt <- tupleAssociation[[i]]
      nr <- nrow(ttt) + 1
      ttt[nr,1:4] <- unlist(sequence[j])[1:4]

      tupleAssociation[[i]] <- ttt


      if (pathMatrix[i,j]==0)
      {
        i=i-1;
        j=j-1;
      } else if (pathMatrix[i,j]==1)
      {
        j=j-1;
      } else if (pathMatrix[i,j]==2)
      {
        i=i-1;
      } else
      {
        break
      }
    }
  }

  averageSR <- list()
  for (t in 1:length(averageS))
  {
    df <- tupleAssociation[[t]]
    averageSR[[t]] <- avg.quaternion.markleyCmp(df)
  }

  #message(paste("Normalized distance: ", costMatrix[length(averageS), length(sequence)] / (length(averageS) + length(sequence))))
  norm.distance[norm.distance.index, signal.id] <<- costMatrix[length(averageS), length(sequence)] / (length(averageS) + length(sequence))
  return(averageSR)
}


library(compiler)
DBA_one_iterationCmp <- cmpfun(DBA_one_iteration)

#helper function
my_dba <- function(sequences, iterationsCount, index = -1, eps)
{
  #sequences <- CharTrajResampled
  if (index < 0)
  {
    index <- round(runif(1, 1, nrow(as.data.frame(sequences[1]))))
  }
  #index <- 1
  average <- getSingleSIgnal(sequences, index)
  #average <- sequences[[index]]
  for (i in 1:iterationsCount)
  {
    norm.distance.index <<- i

    #print(paste('Iteration ', i, ' initial ', index))
    #average=DBA_one_iteration(average,sequences);
    average=DBA_one_iterationCmp(average,sequences)

    message(paste('Iteration: ', i, ", normalized distance:", norm.distance[norm.distance.index, signal.id], sep = ""))
    if (norm.distance[norm.distance.index, signal.id] < eps * 0.1)
    {
      message("Coverage")
      return(average)
    }
    if (i > 1)
    {
      if (abs(norm.distance[norm.distance.index - 1, signal.id] - norm.distance[norm.distance.index, signal.id]) < eps)
      {
        message("Coverage")
        return(average)
      }
    }
  }
  return (average)
}

library(compiler)
my_dbaCmp <- cmpfun(my_dba)


#' Weighted quaternion Markley averaging algorithms.
#'
#' See: F. Landis Markley, Yang Cheng, John Lucas Crassidis, and Yaakov Oshman.  "Averaging Quaternions", Journal of Guidance, Control, and Dynamics, Vol. 30, No. 4 (2007), pp. 1193-1197. https://doi.org/10.2514/1.28949
#'
#' @param Q a data frame of quaternions (four dimensional vectors) to be averaged. Each row of data frame holds one quaternion.
#' @param weights weights vector.
#'
#' @return 4D quaternion vector.
#'
#' @examples
#' Q <- data.frame(c(0.9999986, 0.9999986, 0.9999986, 0.9999986, 0.9999986, 0.9999986),
#' c(0.0008716584, 0.0008716584, 0.0009590162, 0.0009590162, 0.001046359, 0.001046359),
#' c(0.0009608439, 0.001048034, 0.0008736689, 0.001048034, 0.0009608439, 0.0008736689),
#' c(0.001046359, 0.0009590162, 0.001046359, 0.0008716584, 0.0008716584, 0.0009590162))
#'
#' x <- seq(-2,2,length=6)
#' y <- dnorm(x,mean=0, sd=1)
#' y <- y / sum(y)
#' wavg.quaternion.markley(Q, y)
wavg.quaternion.markley <- function(Q, weights)
{
  q <- t(as.matrix(Q[1,]))

  q %*% t(q)

  A <- rep(0, 16)
  A <- matrix(A, 4)
  M <- nrow(Q)
  wSum <- 0
  for (a in 1:M)
  {
    q <- t(as.matrix(Q[a,]));
    w_i <- weights[a]
    A <- w_i * (q %*% t(q)) + A #rank 1 update
    wSum <- wSum + w_i
  }
  A <- (1.0 / wSum) * A
  return(eigen(A)$vectors[,1])
}

#helper function
gaussianQuaternionSmoother <- function(quaternionSignal, windowSize)
{
  #quaternionSignal <- as
  #windowSize <- 10
  startInd <- floor(windowSize / 2)
  startLoop <- startInd
  endInd <- ceiling(windowSize / 2)
  startInd+endInd
  endLoop <- length(quaternionSignal)-endInd

  x <- seq(-2,2,length=windowSize)
  y <- dnorm(x,mean=0, sd=1)
  y <- y / sum(y)


  smoothedSignal <- list()
  for (a in 1:length(quaternionSignal))
  {
    smoothedSignal[[a]] <- quaternionSignal[[a]]
  }

  for (a in startLoop:endLoop)
  {
    ii <- 1
    sampleToSmooth <- data.frame(v1 = numeric(),
                                 v2 = numeric(),
                                 v3 = numeric(),
                                 v4 = numeric(),
                                 stringsAsFactors = FALSE);
    for (b in (-startInd+1):endInd)
    {
      sampleToSmooth[ii,] <- quaternionSignal[[a+b]]
      ii <- ii + 1
    }

    smoothedSignal[[a]] <- wavg.quaternion.markley(sampleToSmooth, y)
  }
  return (smoothedSignal)
}

############################################################
#
############################################################

#helper function
euler2quaternion <- function(xR, yR, zR)
{
  a1 <- c(zR, yR, xR) * (pi/180)
  q <- EA2Q(a1,'zyx')
  return(q)
}

#helper function
quaternion2euler <- function(q)
{
  ea <- Q2EA(q,'zyx') * (180/pi)
  return(c(ea[3], ea[2], ea[1]))
}

#helper function
resampleArray <- function(sig, newSignalLength) {
  xi <- seq(from = 1, to = length(sig), length.out = newSignalLength)
  retSig = interp1(1:length(sig), sig, xi, method = "nearest")#"linear", "nearest", "pchip", "cubic", "spline"
  return (retSig)
}



#' This function averages a list of motion capture recordings.
#'
#' Averaging is performed on each rotation channel of hierarchical model sepparetly with Dynami Time Warping barycenter averagin (DBA), see:
#' François Petitjean, Alain Ketterlin, PierreGançarski,
#' "A global averaging method for dynamic time warping, with applications to clustering",
#' Pattern Recognition, Volume 44, Issue 3, March 2011, Pages 678-693, https://doi.org/10.1016/j.patcog.2010.09.013
#' Each rotation signal holds rotation represented as quaternion. Quaternion averging is performed with Quaternion Markley averaging algorithms, see: .
#' F. Landis Markley, Yang Cheng, John Lucas Crassidis, and Yaakov Oshman.  "Averaging Quaternions", Journal of Guidance, Control, and Dynamics, Vol. 30, No. 4 (2007), pp. 1193-1197. https://doi.org/10.2514/1.28949
#' Results are smoothed with Weighted Quaternion Markley averaging algorithms using gaussian kernel.
#' @param myList list of mocap data frames. Algorithm uses columns with names that has .Rx, .Ry and .Rz names. Rotation should be represented
#' by Euler angles in degrees.
#' @param DBAIterationsCount maximal number of itereations of DBA algorithm (deafulat value is DBAIterationsCount = 50).
#' @param eps treshold value for DBA - iteration stops when absolute value of difference between normalized DTW distances on this and previous iteration is less than eps (default value is eps = 0.0001).
#' @param plot.me if TRUE, plots DTW distances for each averaged signal (deafulat value is plot.me = 50).
#'
#' @return return object of class averaged.mocap.
#'
#' @examples
#'  #load list of objects of mocap class
#'  data("mawashi.geri.right.list")
#'  myList <- list()
#'  #assign data frames to list
#'  for (a in 1:length(mawashi.geri.right.list))
#'  {
#'    myList[[a]] <-mawashi.geri.right.list[[a]]$data.frame
#'  }
#'  #run compiled version of mocap.averaging function
#'  res.data <- mocap.averagingCmp(myList, 50, eps = 0.000001)
#'  plot(res.data)
#'  #write results to disc as bvh file
#'  skel <- set.data.frame(mawashi.geri.right.list[[1]], res.data$fullData)
#'  write.bvh(path = "avg.mawashi.geri.right.bvh", skeleton.helper = skel)
mocap.averaging <- function(myList, DBAIterationsCount = 50, eps = 0.0001, plot.me=TRUE)
{
  library(RSpincalc)
  library('signal')
  if (DBAIterationsCount < 1)
  {
    DBAIterationsCount = 2
    message("DBAIterationsCount < 1 was changed to 2.")
  }

  #dirPathToData <- "e:\\mocap_data\\karate\\2016-10-26 ShorinRyu MP\\evaluation\\zenkutsu_dachi_right\\segmented\\"
  #dirPathToData <- "e:\\mocap_data\\karate\\2016-10-26 ShorinRyu MP\\evaluation\\yoko_geri_right\\segmented\\"

  #filesList <- list.files(dirPathToData)
  #myList <- list()
  #load all files
  #for (a in 1:length(filesList))
  #{
  #  fileToLoad <- paste (dirPathToData, filesList[a], sep = "", collapse = NULL)
  #  myData <- read.csv(file=fileToLoad,head=TRUE,sep=",")

    #myData <- rotateChannel(myData)
    #myData <- rotateChannelCmp(myData)

  #  myList[[a]] <-myData
  #}
  if (length(myList) < 1)
  {
    return (NULL)
  }
  columnsToFind <- colnames(myList[[1]])[grep("[[:alnum:]]+\\.Rx", colnames(myList[[1]]), ignore.case = TRUE)]
  columnsToFind <- c(columnsToFind, colnames(myList[[1]])[grep("[[:alnum:]]+\\.Ry", colnames(myList[[1]]), ignore.case = TRUE)])
  columnsToFind <- c(columnsToFind, colnames(myList[[1]])[grep("[[:alnum:]]+\\.Rz", colnames(myList[[1]]), ignore.case = TRUE)])

  maxL <- length(myList[[1]]$Hips.Rx)
  maxLIndex <- 1
  for (a in 2:length(myList))
  {
    if (length(myList[[a]]$Hips.Rx) > maxL)
    {
      maxL <- length(myList[[a]]$Hips.Rx)
      maxLIndex <- a
    }
  }

  #fileToLoad <- paste (dirPathToData, filesList[maxLIndex], sep = "", collapse = NULL)
  #fullData <- read.csv(file=fileToLoad,head=TRUE,sep=",")
  fullData <- myList[[maxLIndex]]


  for (a in 1:length(myList))
  {
    rescalledData <- list()
    for (b in 1:length(columnsToFind))
    {
      rescalledData[[b]] <- resampleArray(unlist(myList[[a]][columnsToFind[b]]), maxL)
    }
    df <- as.data.frame(rescalledData)
    colnames(df) <- columnsToFind
    myList[[a]] <- df
  }

  columnsToFindNoEnds <- list()


  for (a in 1:length(columnsToFind))
  {
    columnsToFindNoEnds[a] <- substring(text = columnsToFind[a], first = 1, last = nchar(columnsToFind[a]) - 3)
  }
  columnsToFindNoEnds <- unique(unlist(columnsToFindNoEnds))

  norm.distance <<- matrix(rep(NA,DBAIterationsCount * length(columnsToFindNoEnds)), nrow = DBAIterationsCount, ncol = length(columnsToFindNoEnds))

  allResults <- list()
  allResultsColnames <- list()
  Time <- (1:length(myList[[1]]$Hips.Rx)) / 100
  allResults[[1]] <- Time
  allResultsColnames[[1]] <- 'Time'

  #Iterate through all features
  for (colToITerate in 1:length(columnsToFindNoEnds))
  #for (colToITerate in 1:1)
  {
    #print(columnsToFindNoEnds[colToITerate])
    message(columnsToFindNoEnds[colToITerate])
    fx <- paste(columnsToFindNoEnds[colToITerate], '.Rx',sep = '')
    fy <- paste(columnsToFindNoEnds[colToITerate], '.Ry',sep = '')
    fz <- paste(columnsToFindNoEnds[colToITerate], '.Rz',sep = '')

    allResultsColnames[[length(allResultsColnames) + 1]] <- fx
    allResultsColnames[[length(allResultsColnames) + 1]] <- fy
    allResultsColnames[[length(allResultsColnames) + 1]] <- fz

    all_quat <- list()
    for (a in 1:length(myList))
    for (fposition in 1:length(myList[[1]][,1]))
    {
        helper_list <- list()
        for (a in 1:length(myList))
        {
          helper_list[[a]] <- euler2quaternion(myList[[a]][fx][fposition,],
                                              myList[[a]][fy][fposition,],
                                              myList[[a]][fz][fposition,])
        }
        df <- as.data.frame(matrix(unlist(helper_list), nrow=length(myList), byrow=T))
        all_quat[[fposition]] <- df
    }

    #add borders for smoothing puroposes
    kk <- append(all_quat[1], all_quat)
    for (a in 1:9)
    {
      kk <- append(all_quat[1], kk)
    }
    for (a in 1:10)
    {
      kk <- append(kk,all_quat[length(all_quat)])
    }

    #print('1')
    #ptm <- proc.time()
    #  as <- my_dba(kk, 2)
    #print(proc.time() - ptm)
    #print('2')
    #ptm <- proc.time()
    signal.id <<- colToITerate
    as <- my_dbaCmp(kk, DBAIterationsCount,-1, eps)


    #print(proc.time() - ptm)

    smoothedSignal <- gaussianQuaternionSmoother(as, 10)
    smoothedSignal <- smoothedSignal[11:(length(smoothedSignal)-10)]

    as <- as[11:(length(as)-10)]


    all_eulaer <- list()
    vx <- list()
    vy <- list()
    vz <- list()
    for (a in 1:length(as))
    {
      all_eulaer[[a]] <- quaternion2euler(as[[a]])
      vx[a] <- all_eulaer[[a]][1]
      vy[a] <- all_eulaer[[a]][2]
      vz[a] <- all_eulaer[[a]][3]
    }
    vx <- unlist(vx)
    vy <- unlist(vy)
    vz <- unlist(vz)

    #visualization
    all_eulaerS <- list()
    vxS <- list()
    vyS <- list()
    vzS <- list()
    for (a in 1:length(smoothedSignal))
    {
      all_eulaerS[[a]] <- quaternion2euler(smoothedSignal[[a]])
      vxS[a] <- all_eulaerS[[a]][1]
      vyS[a] <- all_eulaerS[[a]][2]
      vzS[a] <- all_eulaerS[[a]][3]
    }
    vxS <- unlist(vxS)
    vyS <- unlist(vyS)
    vzS <- unlist(vzS)

    allResults[[length(allResults) + 1]] <- vxS
    allResults[[length(allResults) + 1]] <- vyS
    allResults[[length(allResults) + 1]] <- vzS
  }



  allResultsDF <- as.data.frame(allResults)
  colnames(allResultsDF) <- unlist(allResultsColnames)



  for (a in 1:length(colnames(allResultsDF)))
  {
    #if (colnames(allResultsDF)[a] != 'Hips.Rx' && colnames(allResultsDF)[a] != 'Hips.Ry' && colnames(allResultsDF)[a] != 'Hips.Rz')
    {
      #print(colnames(allResultsDF)[a])
      fullData[colnames(allResultsDF)[a]] <- allResultsDF[colnames(allResultsDF)[a]]
    }
  }

  colnames(norm.distance) <- columnsToFindNoEnds
  rownames(norm.distance) <- 1:DBAIterationsCount


  res.data <- list(fullData = fullData, norm.distance = norm.distance)


  if (plot.me)
  {
    plot.data <- norm.distance
    for (j in 1:ncol(plot.data))
      for (i in 2:nrow(plot.data))
      {
        if (is.na(plot.data[i,j]))
          plot.data[i,j] <- plot.data[i-1,j]
      }

    color.rgb <- c()
    for (a in 1:length(plot.data[1,]))
    {
      color.rgb <- c(color.rgb,rgb(runif(1),runif(1),runif(1)))
    }

    plot(plot.data[,1], ylab = "Normalized distance", xlab = "Iterations", ylim = c(0, max(plot.data)), col = color.rgb[1], pch=1)
    lines(plot.data[,1], col = color.rgb[1], pch=1)

    for (a in 2:length(plot.data[1,]))
    {
      points(plot.data[,a], col = color.rgb[a], pch=(a%%11))
      lines(plot.data[,a], col = color.rgb[a], pch=(a%%11))
    }
    legend("topright", legend=colnames(plot.data), col=color.rgb, pch = (1:length(color.rgb) %% 11), ncol=4)
  }
  rm(norm.distance, envir = .GlobalEnv)
  class(res.data) <- "averaged.mocap"
  return (res.data)
  #write.csv(fullData, file=dirPathToSave, row.names = FALSE, quote = FALSE)
}

library(compiler)
mocap.averagingCmp <- cmpfun(mocap.averaging)

#' A class returned by mocap.averaging function.
#'
#' @docType class
#' @usage see documentation of mocap.averaging.
#' @format
#' a list containing data frame (fullData) and data frame (norm.distance) with normalized distance optimized during averaging.
#'
#' @keywords class
#' @examples
#' data("mawashi.geri.right.list")
#' myList <- list()
#' for (a in 1:length(mawashi.geri.right.list))
#' {
#'   myList[[a]] <-mawashi.geri.right.list[[a]]$data.frame
#' }
#' res.data <- mocap.averagingCmp(myList, 2, eps = 0.000001)
#' plot(res.data)
averaged.mocap <- setClass("averaged.mocap")


#' Plots normalized distance of all joints from averaged.mocap class.
#'
#' @param data.to.plot object of class averaged.mocap.
#'
#' @examples
#' data("mawashi.geri.right.list")
#' myList <- list()
#' for (a in 1:length(mawashi.geri.right.list))
#' {
#'   myList[[a]] <-mawashi.geri.right.list[[a]]$data.frame
#' }
#' res.data <- mocap.averagingCmp(myList, 2, eps = 0.000001)
#' plot(res.data)
plot.averaged.mocap <- function(data.to.plot)
{
  plot.data <- data.to.plot$norm.distance
  for (j in 1:ncol(plot.data))
    for (i in 2:nrow(plot.data))
    {
      if (is.na(plot.data[i,j]))
        plot.data[i,j] <- plot.data[i-1,j]
    }

  color.rgb <- c()
  for (a in 1:length(plot.data[1,]))
  {
    color.rgb <- c(color.rgb,rgb(runif(1),runif(1),runif(1)))
  }

  plot(plot.data[,1], ylab = "Normalized distance", xlab = "Iterations", ylim = c(0, max(plot.data)), col = color.rgb[1], pch=1)
  lines(plot.data[,1], col = color.rgb[1], pch=1)

  for (a in 2:length(plot.data[1,]))
  {
    points(plot.data[,a], col = color.rgb[a], pch=(a%%11))
    lines(plot.data[,a], col = color.rgb[a], pch=(a%%11))
  }
  legend("topright", legend=colnames(plot.data), col=color.rgb, pch = (1:length(color.rgb) %% 11), ncol=4)
}

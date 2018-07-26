#' This function returns absolute value of difference between x1 and x2.
#'
#' @param x1 first numeric value.
#' @param x2 second numeric value.
#'
#' @return abs(x1[1] - x2[1]).
#'
#' @examples
#' euc.dist1d(1,-5)
euc.dist1d <- function(x1, x2) abs(x1[1] - x2[1])
library(compiler)
euc.dist1dCmp <- cmpfun(euc.dist1d)

#helper function
vec.length <- function(x) sqrt(sum(x * x))

#helper function
euc.dist1dangle <- function(x1, x2)
{
  angle <- abs(x1[1] - x2[1])
  if (angle > pi)
    angle <- (2 * pi) - angle
  return (angle)
}
library(compiler)
euc.dist1dangleCmp <- cmpfun(euc.dist1dangle)

#helper function
calculatenormalizeddistance <- function(path1, path2, signal1, signal2, FUN)
{
  distanceHelper <- 0
  for (a in 1:length(path1))
  {
    distanceHelper <- distanceHelper + FUN(signal1[[path1[a]]], signal2[[path2[a]]])
  }
  return (distanceHelper / (length(signal1) + length(signal2)))
}

#' This function calulcates Euclidean distance between vectors x1 and x2.
#'
#' @param x1 first numeric vector.
#' @param x2 second numeric vector.
#'
#' @return asqrt(sum((x1 - x2) ^ 2)).
#'
#' @examples
#' euc.dist(c(1,2,3,4),c(-5,0,-6, 3))
euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
library(compiler)
euc.distCmp <- cmpfun(euc.dist)


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
myDTW <- function(FUN,averageS,sequence)
{
  #averageS <- rs
  #sequence <- s1

  tupleAssociation <- list();
  #for (t in 1:length(averageS))
  #  tupleAssociation[[t]] <- list();
  for (t in 1:length(averageS))
    tupleAssociation[[t]] <- data.frame(v1 = numeric(),
                                        v2 = numeric(),
                                        v3 = numeric(),
                                        v4 = numeric(),
                                        stringsAsFactors = FALSE);

  #for t=1:size(averageS,2)
  #tupleAssociation{t}=[];
  #end

  sl <- length(averageS) * length(sequence)
  seq1 <- rep(0, sl)
  #mat1 <- matrix(seq1, length(sequences[[1]]))

  costMatrix <- matrix(seq1, length(averageS))
  pathMatrix <- matrix(seq1, length(averageS))



  #costMatrix[1,1] <- quat_similarityCmp(unlist(averageS[1]),unlist(sequence[1]))
  costMatrix[1,1] <- FUN(unlist(averageS[1]),unlist(sequence[1]))


  pathMatrix[1,1] <- -1;


  for (i in 2:length(averageS))
  {
    #costMatrix[i,1] <- costMatrix[i-1,1] + quat_similarityCmp(unlist(averageS[i]),unlist(sequence[1]));
    costMatrix[i,1] <- costMatrix[i-1,1] + FUN(unlist(averageS[i]),unlist(sequence[1]));

    pathMatrix[i,1] <- 2;
  }

  for (j in 2:length(sequence))
  {
    #costMatrix[1,j] <- costMatrix[1,j-1] + quat_similarityCmp(unlist(sequence[j]),unlist(averageS[1]));
    costMatrix[1,j] <- costMatrix[1,j-1] + FUN(unlist(sequence[j]),unlist(averageS[1]));
    pathMatrix[1,j] <- 1;
  }

  for (i in 2:length(averageS))
  {
    for (j in 2:length(sequence))
    {
      #indiceRes <- ArgMin3(costMatrix[i-1,j-1],costMatrix[i,j-1],costMatrix[i-1,j]);
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
      #costMatrix[i,j] <- res + distanceTo(averageS[i],sequence[j])
      #costMatrix[i,j] <- res + quat_similarity(unlist(averageS[i]),unlist(sequence[j]))
      #costMatrix[i,j] <- res + quat_similarityCmp(unlist(averageS[i]),unlist(sequence[j]))
      costMatrix[i,j] <- res + FUN(unlist(averageS[i]),unlist(sequence[j]))

    }
  }

  i <- length(averageS)
  j <- length(sequence)

  distance <- 0

  path1 <- list()
  path2 <- list()

  a <- 1
  while(TRUE)
  {
    path1[[a]] <- i
    path2[[a]] <- j
    a <- a + 1

    ttt <- tupleAssociation[[i]]
    nr <- nrow(ttt) + 1
    ttt[nr,1:4] <- unlist(sequence[j])[1:4]

    tupleAssociation[[i]] <- ttt
    #print(costMatrix[i,j])
    distance <- costMatrix[i,j] + distance
    #print(costMatrix[i,j])
    if (pathMatrix[i,j]==0)
    {
      i=i-1;
      j=j-1;
      #print(paste('0)', i))
    } else if (pathMatrix[i,j]==1)
    {
      j=j-1;
      #print(paste('1)', i))
    } else if (pathMatrix[i,j]==2)
    {
      i=i-1;
      #print(paste('2)', i))
    } else
    {
      break
    }
  }

  path1 <- rev(unlist(path1))
  path2 <- rev(unlist(path2))

  #plot(path1, path2)
  #end

  #return(distance / (length(averageS) + length(sequence)))
  #return (costMatrix[length(averageS), length(sequence)] / (length(averageS) + length(sequence)))
  normalized_distance = costMatrix[length(averageS), length(sequence)] / (length(averageS) + length(sequence))
  newList <- list("path1" = path1, "path2" = path2, 'normalized_distance' = normalized_distance)
  return (newList)
}
myDTWCmp <- cmpfun(myDTW)


#helper function
plotsmoothingresults <- function(smoothingresults, plottitle, plotifnoextreams = TRUE, plotsmoothed = FALSE, ylab = "Distance [cm]", legenPosition = "topright")
{
  #smoothingresults <- footddf
  #plottitle <- "footddf"
  #plotifnoextreams <- TRUE
  #plotsmoothed <- TRUE

  #plot smoothed data
  if (plotsmoothed)
  {
    plot(smoothingresults$smoothdata , col = "black")
    title(main = plottitle)
    lines(smoothingresults$smoothdata , col = "black")

    for (a in 1:length(smoothingresults$extremumbool))
    {
      if (smoothingresults$extremumbool[a])
      {
        points(a, smoothingresults$smoothdata[a], col = "red",pch = 4)
        idhelp <- a - 1
        end <- FALSE
        #footddf$extremumtreshold
        while (smoothingresults$derivative[idhelp] > 0 && !end)
        {
          points(idhelp, smoothingresults$smoothdata[idhelp], col = "blue",pch = 4)
          if (idhelp > 1)
            idhelp <- idhelp - 1
          else
            end <- TRUE

        }
      }
    }
  }
  if (plotifnoextreams || length(smoothingresults$resultsList) > 0)
  {
    #plot(smoothingresults$data , col = "black")
    plot(smoothingresults$data, xlab = "Time [10^-100 s]", ylab = ylab, col = 'black', type='l',
	ylim = c(min(smoothingresults$data), max(smoothingresults$data) * 1.5))

    lines(smoothingresults$smoothdata, col = 'purple', lty = 2)
    title(main = plottitle)
    #lines(smoothingresults$data , col = "black")
    if (length(smoothingresults$resultsList) > 0)
      for (a in 1:length(smoothingresults$resultsList))
      {
        vec <- smoothingresults$resultsList[[a]]
        for (b in 1:length(vec))
        {
          points(vec[b], smoothingresults$data[vec[b]], col = "red",pch = 4, lwd=3)
          idhelp <- vec[b] - 1
          end <- FALSE
          #footddf$extremumtreshold
          while (smoothingresults$derivative[idhelp] > 0 && !end)
          {
            points(idhelp, smoothingresults$data[idhelp], col = "blue",pch = 4)
            if (idhelp > 1)
              idhelp <- idhelp - 1
            else
              end <- TRUE

          }
        }
      }
    legend(x= legenPosition, y=max(smoothingresults$data) * 1.5, legend=c("Original", "Smoothed","Maxima over treshold", "ROI"), col=c("black", "purple",'red', "blue"), lty=c(1,2,1), cex=0.8)
    #legend(x= "topright", y=max(ref.res$data) * 1.5, legend=c("Original", "Smoothed", "Maxima", "Maxima over treshold"), col=c("black", "purple", 'cyan','red'), lty=c(1,2,1), cex=0.8)

  }
}

#helper function
rglplotanalyzedata <- function(refdatakinematic, inputdataalignmentkinematic, xx1, xx2, path1, path2, resultdata, whattodraw = "LeftFoot", skeleton = NULL)
{
  #xx1 <- refdatakinematicf$dataRightKnee
  #xx2 <- inputdataalignmentkinematicf$dataRightKnee
  #path1 <- footddf$path1
  #path2 <- footddf$path2
  #resultdata <- kneeadf
  #whattodraw <- "LeftLeg"
  library("rgl")
  rgl.open() # Open a new RGL device
  #rgl.bg(col="white")

  alpha <- 0.1

  idref <- -1
  idinput <- -1

  #for (a in seq(1, length(path1), 20))
  #a = 1
  #for (a in length(path1):length(path1))
  for (a in 1:length(path1))
  {
    if (!(path2[a] %in% resultdata$extremumid))
    {
      if (idref != path1[a])
      {
        idref = path1[a]

        print.frame(skeleton, idref, my.color = "green", alpha = a / (4 * length(path1)), spheres = FALSE, df = refdatakinematic)

        #renderactor(refdatakinematic, idref, pointscolors, "green", "green", a / (4 * length(path1)), showspheres = FALSE)
        #renderactor(refdatakinematic, idref, "green", "green", 16 / length(path1), showspheres = FALSE,linewidth =  2)
      }
      if (idinput != path2[a])
      {
        idinput = path2[a]

        print.frame(skeleton, idinput, my.color = "red", alpha = a / (4 * length(path2)), spheres = FALSE, df = inputdataalignmentkinematic)

        #renderactor(inputdataalignmentkinematic, idinput, pointscolors, "red", "red", a / (4 * length(path2)), showspheres = FALSE)
        #renderactor(inputdataalignmentkinematic, idinput, "red", "red", 16 / length(path2), showspheres = FALSE, 2)
      }

      if (path2[a] %in% resultdata$extremumid)
      {
        lines3d(c(refdatakinematic[path1[a],paste(whattodraw, ".Dx",sep = "")],inputdataalignmentkinematic[path2[a],paste(whattodraw, ".Dx",sep = "")]),
                c(refdatakinematic[path1[a],paste(whattodraw, ".Dy",sep = "")],inputdataalignmentkinematic[path2[a],paste(whattodraw, ".Dy",sep = "")]),
                c(refdatakinematic[path1[a],paste(whattodraw, ".Dz",sep = "")],inputdataalignmentkinematic[path2[a],paste(whattodraw, ".Dz",sep = "")])
                , color = "yellow", alpha = 1, lwd=5)
        #rgl.texts((xx2[[path2[a]]][1] + xx1[[path1[a]]][1]) / 2,
        #          (xx2[[path2[a]]][2] + xx1[[path1[a]]][2]) / 2,
        #          (xx2[[path2[a]]][3] + xx1[[path1[a]]][3]) / 2,
        #          a, lwd = 5)
      }
      else
      {
        lines3d(c(refdatakinematic[path1[a],paste(whattodraw, ".Dx",sep = "")],inputdataalignmentkinematic[path2[a],paste(whattodraw, ".Dx",sep = "")]),
                c(refdatakinematic[path1[a],paste(whattodraw, ".Dy",sep = "")],inputdataalignmentkinematic[path2[a],paste(whattodraw, ".Dy",sep = "")]),
                c(refdatakinematic[path1[a],paste(whattodraw, ".Dz",sep = "")],inputdataalignmentkinematic[path2[a],paste(whattodraw, ".Dz",sep = "")])
                , color = "blue", alpha = 1, lwd=1)
        #rgl.texts((xx2[[path2[a]]][1] + xx1[[path1[a]]][1]) / 2,
        #          (xx2[[path2[a]]][2] + xx1[[path1[a]]][2]) / 2,
        #          (xx2[[path2[a]]][3] + xx1[[path1[a]]][3]) / 2,
        #        a, lwd = 1)
      }
    }
  }
  for (b in resultdata$extremumid)
  {
    for (a in 1:length(path2))
    {
      if (path2[a] == b)
      {
        #plot(original.bvh, frames.fraction = 0.1, my.color = "red", alpha = 0.1, spheres = FALSE)

        print.frame(skeleton, path1[a], my.color = "green", alpha = 1, spheres = FALSE, df = refdatakinematic)
        print.frame(skeleton, path2[a], my.color = "red", alpha = 1, spheres = FALSE, df = inputdataalignmentkinematic)


        #renderactor(refdatakinematic, path1[a], "green", "green", 1, showspheres = FALSE, 5)
        #renderactor(inputdataalignmentkinematic, path2[a], "red", "red", 1, showspheres = FALSE, 5)

        lines3d(c(refdatakinematic[path1[a],paste(whattodraw, ".Dx",sep = "")],inputdataalignmentkinematic[path2[a],paste(whattodraw, ".Dx",sep = "")]),
                c(refdatakinematic[path1[a],paste(whattodraw, ".Dy",sep = "")],inputdataalignmentkinematic[path2[a],paste(whattodraw, ".Dy",sep = "")]),
                c(refdatakinematic[path1[a],paste(whattodraw, ".Dz",sep = "")],inputdataalignmentkinematic[path2[a],paste(whattodraw, ".Dz",sep = "")])
                , color = "yellow", alpha = 1.0, lwd=50)
        #rgl.texts((xx2[[path2[a]]][1] + xx1[[path1[a]]][1]) / 2,
        #          (xx2[[path2[a]]][2] + xx1[[path1[a]]][2]) / 2,
        #          (xx2[[path2[a]]][3] + xx1[[path1[a]]][3]) / 2,
        #          a, lwd = 5)
      }
    }
  }

  borderx <- c(-100, -100, 100, 100, -100, -100, 100, 100) * 1
  bordery <- c(200, -20, 200, -20, 200, -20, 200, -20) * 1
  borderz <- c(-100, -100, -100, -100, 100, 100, 100, 100) * 1

  spheres3d(borderx, bordery, borderz, col = "red", r=0.01)


  y <- 0#min(refdata[1,featurename])
  planes3d(0,-1,0,y, col = 'orange', alpha = 0.3)
  #axes3d(col="black", alpha = 1, lwd = 1)
  axes3d(col="white", alpha = 1, lwd = 1)


}

#helper function
scatterplotanalyzedta <- function(x1, y1, z1, x2, y2, z2, path1, path2)
{

  library('scatterplot3d')
  color <- rep("green", length(x1))

  #s3d <- scatterplot3d(x1, z1, y1, color, type='l', pch=20, main="DTW signal mapping of RightThigh roation", angle=90, xlim=c(-1, 1), ylim=c(-1, 0), zlim=c(-1, -0.3),
  #s3d <- scatterplot3d(x1, z1, y1, color, type='l', pch=20, main="DTW signal mapping of Hips roation", angle=45, xlim=c(-1, 1), ylim=c(-1, 1), zlim=c(-1, 1),
  #                     s3d <- scatterplot3d(x1, z1, y1, color, type='l', pch=20, main=paste("DTW signal mapping of", "Hips", "roation"), angle=90,

  #at the begining it was angle=45
  s3d <- scatterplot3d(x1, z1, y1, color, type='l', pch=20, main=paste("DTW signal mapping of", "Hips", "rotation"),# angle=90,
                       #xlim=c(-0.5, 0.6), ylim=c(-1, 0.1), zlim=c(-.1, 0.9),
                       xlab = 'X',ylab = 'Z',zlab = 'Y')
  s3d$points3d(x1, z1, y1, col='green', type="p", pch=20)
  s3d$points3d(x2, z2, y2, col='blue', type="l", pch=20)
  s3d$points3d(x2, z2, y2, col='blue', type="p", pch=20)
  text(s3d$xyz.convert(x  = x1, y = z1, z = y1), labels = 1:length(x1), col='green')
  text(s3d$xyz.convert(x  = x2, y = z2, z = y2), labels = 1:length(x2), col='blue')

  for (a in 1:length(path1))
  {
    x3 <- c(x1[path1[a]], x2[path2[a]])
    y3 <- c(y1[path1[a]], y2[path2[a]])
    z3 <- c(z1[path1[a]], z2[path2[a]])
    s3d$points3d(x3, z3, y3, col='red', type="l", pch=20)
  }

}

#helper function
smothdata <- function(dd, smoothSize = 0.2, smoothSizeHelper = 0.1)
{
  library(smoother)
  #smoothSize <- 0.1
  #smoothSize <- 0.2
  #smoothSizeHelper <- 0.1

  options('smoother.window' = smoothSize)
  ddsmooth <- smth.gaussian(dd, tails = TRUE)
  minvalue = min(ddsmooth)
  maxvalue = max(ddsmooth)
  diffvalue <- maxvalue - minvalue

  #findAllMinimums(x)

  derivative <- rep(0, length(ddsmooth))
  for (a in 2:(length(ddsmooth)-1))
  {
    derivative[a] <- (ddsmooth[a+1] - ddsmooth[a-1]) / 2
  }

  extremum <- rep(0, length(ddsmooth))


  for (a in 2:(length(derivative)-1))
  {
    if (derivative[a] < 0 &&  derivative[a+1] > 0)
      extremum[a] <- -1
    else if (derivative[a] > 0 &&  derivative[a+1] < 0)
      extremum[a] <- 1
  }
  for (a in 1:(length(derivative)*smoothSizeHelper))
  {
    extremum[a] <- 0
  }

  for (a in (length(derivative)*(1-smoothSizeHelper)):length(derivative))
  {
    extremum[a] <- 0
  }

  if (FALSE)
  {
    results <- rep(FALSE, length(extremum))
    for (a in 1:length(extremum))
    {
      if (extremum[a] == 1)
      {
        for (b in a:1)
        {
          if (ddsmooth[b] > ddsmooth[1] && derivative[b] > 0)
            results[b] <- TRUE
          else
            b <- -1
        }
        for (b in a:length(extremum))
        {
          if (ddsmooth[b] > ddsmooth[1] && derivative[b] < 0)
            results[b] <- TRUE
          else
            b <- length(extremum) + 1
        }
      }
    }
  } else
  {
    results <- rep(FALSE, length(extremum))
    for (a in 1:length(extremum))
    {
      if (extremum[a] == 1)
      {
        results[a] = TRUE
      }
    }
  }

  vec <- -1
  resultsList <- list()
  countList <- 1
  for (a in 1:length(results))
  {
    if (results[a] == TRUE)
    {
      if (vec[1] == -1)
        vec <- a
      else
        vec <- c(vec,a)
    }
    else
    {
      if (vec[1] != -1)
      {
        resultsList[[countList]] <- vec
        countList <- countList + 1
        vec <- -1
      }
    }
  }

  extremumid <- which(extremum %in% 1)
  resultdata<-list(data = dd, smoothdata = ddsmooth, derivative = derivative, extremum = extremum, extremumbool = results, resultsList = resultsList, extremumid = extremumid,
                   minvalue = minvalue, maxvalue = maxvalue, diffvalue = diffvalue)
  return(resultdata)
}


#helper function
analyzedta <- function(xx1, xx2, FUN=euc.dist, smoothSize = 0.2,
                       path1 = NA, path2 = NA, extremumtreshold = -1, smoothSizeHelper = 0.1)
{
  dd <- list()
  if (is.na(path1) || is.na(path2))
  {
    ff <- myDTWCmp(FUN,xx1, xx2)
    path1 <- ff$path1
    path2 <- ff$path2
  }

  x1 <- list()
  y1 <- list()
  z1 <- list()
  for (a in 1:length(xx1))
  {
    x1[[a]] <- xx1[[a]][1]
    y1[[a]] <- xx1[[a]][2]
    z1[[a]] <- xx1[[a]][3]
  }
  x1 <- unlist(x1)
  y1 <- unlist(y1)
  z1 <- unlist(z1)

  x2 <- list()
  y2 <- list()
  z2 <- list()
  for (a in 1:length(xx2))
  {
    x2[[a]] <- xx2[[a]][1]
    y2[[a]] <- xx2[[a]][2]
    z2[[a]] <- xx2[[a]][3]
  }
  x2 <- unlist(x2)
  y2 <- unlist(y2)
  z2 <- unlist(z2)


  for (a in 1:length(path1))
  {
    xx <- c(x1[path1[a]], y1[path1[a]], z1[path1[a]])
    yy <- c(x2[path2[a]], y2[path2[a]], z2[path2[a]])
    dd[[a]] <- FUN(xx, yy)
  }

  dd2 <- generatedtwalignment(FUN, x1, y1, z1, x2, y2, z2, path1, path2)

  resultdata <- smothdata(dd2, smoothSize = smoothSize, smoothSizeHelper = smoothSizeHelper)
  if (exists("ff"))
  {
    ff <- c(ff, resultdata)
    return(ff)
  }
  return (resultdata)
}

#helper function
generatedtwalignment <- function(FUN, x1, y1, z1, x2, y2, z2, path1, path2)
{
  dd2 <- list()
  idd2 <- 1
  idd2prev <- -1
  for (a in 1:length(path1))
  {
    idd2 <- path2[a]
    xx <- c(x1[path1[a]], y1[path1[a]], z1[path1[a]])
    yy <- c(x2[path2[a]], y2[path2[a]], z2[path2[a]])

    if (idd2 != idd2prev)
    {
      dd2[[idd2]] <- FUN(xx, yy)
    }
    else
    {
      dd2[[idd2]] <- max(dd2[[idd2prev]], FUN(xx, yy))
    }
    idd2prev <- idd2
  }

  dd2 <- unlist(dd2)
  return(dd2)
}

#' This function translate inputdata so that it has common values of limbname Dx, Dy, Dz columns with refdata.
#'
#' @param inputdata input motion capture data frame.
#' @param refdata reference motion capture data frame.
#' @param limbname name of the column which is used to perform alignment.
#'
#' @return data frame aligned as it is described above.
#'
#' @examples
#' #See example from analyze.mocap function
aligninputandrefdata <- function(inputdata, refdata, limbname)
{
  dx3 <- grep("[[:alnum:]]+\\.Dx", colnames(inputdata), ignore.case = TRUE)
  dy3 <- grep("[[:alnum:]]+\\.Dy", colnames(inputdata), ignore.case = TRUE)
  dz3 <- grep("[[:alnum:]]+\\.Dz", colnames(inputdata), ignore.case = TRUE)



  dx <- inputdata[1,paste(limbname, ".Dx", sep = "")] - refdata[1,paste(limbname, ".Dx", sep = "")]
  dy <- inputdata[1,paste(limbname, ".Dy", sep = "")] - refdata[1,paste(limbname, ".Dy", sep = "")]
  dz <- inputdata[1,paste(limbname, ".Dz", sep = "")] - refdata[1,paste(limbname, ".Dz", sep = "")]

  for (a in 1:length(dx3))
  {
    inputdata[dx3[a]] <- inputdata[dx3[a]] - dx
    inputdata[dy3[a]] <- inputdata[dy3[a]] - dy
    inputdata[dz3[a]] <- inputdata[dz3[a]] - dz
  }
  return (inputdata)
}


#helper function
alignextremum <- function(footddf, kneeadf, analyzerange = 10)
{
  if (length(kneeadf$extremumid) == 0)
    return (kneeadf)
  if (length(footddf$extremumid) == 0)
    return (footddf)
  #analyzerange <- 10
  for (a in 1:length(kneeadf$extremumid))
  {
    found <- FALSE
    for (b in 1:length(footddf$extremumid))
    {
      if (footddf$extremumid[b] - analyzerange <= kneeadf$extremumid[a] &&  kneeadf$extremumid[a] <= footddf$extremumid[b] + analyzerange)
        found <- TRUE
    }
    if (!found)
    {
      kneeadf$resultsList[[a]] <- NA
      kneeadf$extremumbool[kneeadf$extremumid[a]] <- FALSE
      kneeadf$extremumid[a] <- NA
      kneeadf$extremum[a] <- 0
    }
    #kneeadf$extremumbool[a] <- TRUE
  }
  kneeadf$extremumid <- kneeadf$extremumid[!is.na(kneeadf$extremumid)]
  a <- 1
  while (a <= length(kneeadf$resultsList))
  {
    if (is.na((kneeadf$resultsList[[a]])))
    {
      kneeadf$resultsList <- kneeadf$resultsList[-a]
    }
    else {
      a <- a + 1
    }
  }
  return (kneeadf)
}

#helper function
tresholdresults <- function(extreamdf, extremumtreshold = -1)
{
  if (extremumtreshold < 0)
  {
    extremumtreshold <- mean(extreamdf$smoothdata) + sd(extreamdf$smoothdata)
    for (a in 1:length(extreamdf$extremum))
    {
      if (extreamdf$extremum[a] == 1 &&  extreamdf$smoothdata[a] >= extremumtreshold) #&& (ddsmooth[a] - minvalue) >= (diffvalue * extremumtreshold))
      {
        extreamdf$extremumbool[a] = TRUE
      }
      else
      {
        extreamdf$extremumbool[a] = FALSE
        extreamdf$extremum[a] = 0
      }
    }
  }
  else {
    for (a in 1:length(extreamdf$extremum))
    {
      if (extreamdf$extremum[a] == 1 &&  (extreamdf$smoothdata[a] - extreamdf$minvalue) >= (extreamdf$diffvalue * extremumtreshold))
      {
        extreamdf$extremumbool[a] = TRUE
      }
      else
      {
        extreamdf$extremumbool[a] = FALSE
        extreamdf$extremum[a] = 0
      }
    }
  }

  vec <- -1
  resultsList <- list()
  countList <- 1
  for (a in 1:length(extreamdf$extremumbool))
  {
    if (extreamdf$extremumbool[a] == TRUE)
    {
      if (vec[1] == -1)
        vec <- a
      else
        vec <- c(vec,a)
    }
    else
    {
      if (vec[1] != -1)
      {
        resultsList[[countList]] <- vec
        countList <- countList + 1
        vec <- -1
      }
    }
  }

  extreamdf$extremumid <- unlist(resultsList)

  extreamdf$resultsList <- resultsList
  return (extreamdf)

}



#' This function perfomrs motion capture dat analysis based on Dynamic Time Warping.
#'
#' This procedure detects highest differences between reference and input mocap recording. Analysis goes as follows:
#' \itemize{
#' \item Perform DTW on x1 and x2 from list at index 1. Plot DTW alignment This alignment will be used in aligning all other signals. Plot distance between x1 and x2 after alignment and find local maxima in this plot.
#' In following analysis only maxima with relative value above treshold will be used.
#' \item perform DTW alignment for each other element from the data.configuration list, however use the alignment function from first step. Maxima are detected in the same procedure as above, however we take into account only those
#' of them, that are close enough to maxims from first step of analysis.
#' }
#' @param data.configuration a list contaning configuration for the algorithm. Eachelement of the list is a list with following elements (all elements are obligatory):
#' \itemize{
#' \item x1 - reference signal for DTW (vectors list),
#' \item x2 - second signal for DTW (vectors list),
#' \item FUN - distance function for DTW, use euc.dist or euc.dist1d, however you can define any function that can opperates on x1 and x2,
#' \item ylab - label on Y axis of the results plot,
#' \item legend - part of the legend sting over plots,
#' \item plotRGL - name of the body joint for which DTW alignment of x1 and x2 will be drawn. It will be 3D rgl plot. If plotRGL = NULL plot will not be drawn.
#' \item skeleton - object of class mocap. It is used to get joints relations while plotting RGL plot.
#' }
#' @param ref.d data frame with reference mocap data (default is ref.d=NULL). It is used to get joints relations while plotting RGL plot.
#' @param in.d  data frame with reference mocap data (default is in.d=NULL). It is used to get joints relations while plotting RGL plot.
#' @param extremumtreshold treshold from range [0,1], that is used to remove local extreme, which realtive value is below extremumtreshold (default value is extremumtreshold=0.66).
#' @param smoothSize size of the gaussian smoothing window. Deafult value is smoothSize = 0.1.
#'
#' @return a list containing data.configuration parameters plus algorihm results
#'
#' @examples
#' ########################
#' #analyze upper body data
#' ########################
#' data(right.arm.motion.1)
#' data(right.arm.motion.2)
#'
#' refdata <- right.arm.motion.1$data.frame
#' inputdata <- right.arm.motion.2$data.frame
#'
#' extremumtreshold <- 0.66
#' smoothSize <- 0.1
#'
#' inputdataalignment <- rotatedata(inputdata, refdata, "LeftShoulder","RightShoulder")
#' inputdataalignmentkinematic <- calculate.kinematic(inputdataalignment, bodypartname = "LeftShoulder")
#' refdatakinematic <- calculate.kinematic(refdata, bodypartname = "LeftShoulder")
#' inputdataalignmentkinematic <- aligninputandrefdata(inputdataalignmentkinematic, refdatakinematic, limbname = "LeftShoulder")
#'
#' data.configuration <- list()
#' data.configuration[[1]] <- list(x1 = vector.to.list(refdatakinematic, "RightHand"),
#'    x2 = vector.to.list(inputdataalignmentkinematic, "RightHand"),
#'    FUN = euc.dist,
#'    ylab = "Distance [cm]",
#'    legend = "RightHand",
#'    plotRGL = "RightHand",
#'    skeleton = right.arm.motion.1)
#'
#' data.configuration[[2]] <- list(x1 = vector.to.angles.list(refdatakinematic, "RightShoulder", "RightArm", "RightForearm"),
#'    x2 = vector.to.angles.list(inputdataalignmentkinematic, "RightShoulder", "RightArm", "RightForearm"),
#'    FUN = euc.dist1d,
#'    ylab = "Angle [rad]",
#'    legend = "Right elbow",
#'    plotRGL = NULL,
#'    skeleton = NULL)
#'
#' x1 <- vector.to.angles.frame.list(refdatakinematic, "RightArm", "RightForearm", "RightShoulder", "LeftShoulder")
#' x2 <- vector.to.angles.frame.list(inputdataalignmentkinematic, "RightArm", "RightForearm", "RightShoulder", "LeftShoulder")
#'
#' data.configuration[[3]] <- list(x1 = x1[[1]],
#'    x2 = x2[[1]],
#'    FUN = euc.dist1d,
#'    ylab = "Angle [rad]",
#'    legend = "X angle between RightArm and RightForearm",
#'    plotRGL = NULL,
#'    skeleton = NULL)
#'
#' data.configuration[[4]] <- list(x1 = x1[[2]],
#'    x2 = x2[[2]],
#'    FUN = euc.dist1d,
#'    ylab = "Angle [rad]",
#'    legend = "Y angle between RightArm and RightForearm",
#'    plotRGL = NULL,
#'    skeleton = NULL)
#'
#' data.configuration[[5]] <- list(x1 = x1[[3]],
#'    x2 = x2[[3]],
#'    FUN = euc.dist1d,
#'    ylab = "Angle [rad]",
#'    legend = "Z angle between RightArm and RightForearm",
#'    plotRGL = NULL,
#'    skeleton = NULL)
#'
#' res <- analyze.mocap(data.configuration,
#' refdatakinematic,
#' inputdataalignmentkinematic,
#' extremumtreshold,
#' smoothSize)
#'
#' ########################
#' #analyze lower body data
#' ########################
#' data(mawashi.geri.left.1)
#' data(mawashi.geri.left.2)
#'
#' refdata <- mawashi.geri.left.1$data.frame
#' inputdata <- mawashi.geri.left.2$data.frame
#' extremumtreshold <- 0.66
#' smoothSize <- 0.1
#'
#' inputdataalignment <- rotatedata(inputdata, refdata, "LeftThigh","RightThigh")
#' inputdataalignmentkinematic <- calculate.kinematic(inputdataalignment, bodypartname = "RightFoot")
#' refdatakinematic <- calculate.kinematic(refdata, bodypartname = "RightFoot")
#' inputdataalignmentkinematic <- aligninputandrefdata(inputdataalignmentkinematic, refdatakinematic, limbname = "RightFoot")
#'
#' data.configuration <- list()
#' data.configuration[[1]] <- list(x1 = vector.to.list(refdatakinematic, "LeftFoot"),
#'  x2 = vector.to.list(inputdataalignmentkinematic, "LeftFoot"),
#'  FUN = euc.dist,
#'  ylab = "Distance [cm]",
#'   legend = "LeftFoot",
#'   plotRGL = "LeftFoot",
#'   skeleton = mawashi.geri.left.1)
#'
#' data.configuration[[2]] <- list(x1 = vector.to.angles.list(refdatakinematic, "LeftThigh", "LeftLeg", "LeftFoot"),
#'   x2 = vector.to.angles.list(inputdataalignmentkinematic, "LeftThigh", "LeftLeg", "LeftFoot"),
#'   FUN = euc.dist1d,
#'   ylab = "Angle [rad]",
#'   legend = "Left knee",
#'   plotRGL = NULL)
#'
#' x1 <- vector.to.angles.frame.list(refdatakinematic, "LeftThigh", "LeftLeg", "LeftThigh","RightThigh")
#' x2 <- vector.to.angles.frame.list(inputdataalignmentkinematic, "LeftThigh", "LeftLeg", "LeftThigh","RightThigh")
#'
#' data.configuration[[3]] <- list(x1 = x1[[1]],
#'   x2 = x2[[1]],
#'   FUN = euc.dist1d,
#'   ylab = "Angle [rad]",
#'   legend = "X angle between LeftThigh and LeftLeg",
#'   plotRGL = NULL)
#'
#' data.configuration[[4]] <- list(x1 = x1[[2]],
#'   x2 = x2[[2]],
#'   FUN = euc.dist1d,
#'   ylab = "Angle [rad]",
#'   legend = "Y angle between LeftThigh and LeftLeg",
#'   plotRGL = NULL)
#'
#' data.configuration[[5]] <- list(x1 = x1[[3]],
#'   x2 = x2[[3]],
#'   FUN = euc.dist1d,
#'   ylab = "Angle [rad]",
#'   legend = "Z angle between LeftThigh and LeftLeg",
#'   plotRGL = NULL)
#'
#' res <- analyze.mocap(data.configuration,
#' refdatakinematic,
#' inputdataalignmentkinematic,
#' extremumtreshold,
#' smoothSize)

analyze.mocap <- function(data.configuration, ref.d = NULL, in.d = NULL, extremumtreshold = 0.66, smoothSize = 0.1)
{

  data.configuration.ret <- data.configuration
  for (a in 1:length(data.configuration))
  {
    dc <- data.configuration[[a]]
    x1 <- dc$x1
    x2 <- dc$x2

    analyzerange <- ceiling(length(dc$x2) * smoothSize * 1)

    FUN <- dc$FUN
    ylab <- dc$ylab
    legend <- dc$legend
    plotRGL <- dc$plotRGL
    skeleton <- dc$skeleton

    if (a == 1)
    {
      ref.res <- analyzedta(x1, x2, FUN=FUN, smoothSize = smoothSize,
                            extremumtreshold = extremumtreshold, smoothSizeHelper = 0.1)

      dftoanalyze <- tresholdresults(ref.res, extremumtreshold)
      plot(ref.res$path1, ref.res$path2, xlab = "Reference signal [sample id]", ylab = "Input signal [sample id]", main ="DTW alignment", type='l')


      plot(ref.res$data, xlab = "Time [10^-100 s]", ylab =ylab, main =paste("DTWaf of ", legend, " trajectory analysis", sep = ""), col = 'black', type='l', ylim = c(min(ref.res$data), max(ref.res$data) * 1.5))
      lines(ref.res$smoothdata, col = 'purple', lty = 2)
      #points(footddf$smoothdata, col = 'blue')


      dc$results <- ref.res
      dc$tresholded.results <- dftoanalyze

      for (b in 1:length(ref.res$extremumid))
      {
        points(ref.res$extremumid[b], ref.res$smoothdata[ref.res$extremumid[b]], col = "cyan",pch = 1, cex = 2, lwd=3)
      }
      for (b in 1:length(dftoanalyze$extremumid))
      {
        points(dftoanalyze$extremumid[b], dftoanalyze$smoothdata[dftoanalyze$extremumid[b]], col = "red",pch = 4, cex = 2, lwd=3)
      }
      legend(x= "topright", y=max(ref.res$data) * 1.5, legend=c("Original", "Smoothed", "Maxima", "Maxima over treshold"), col=c("black", "purple", 'cyan','red'), lty=c(1,2,1), cex=0.8)

      #this might render quite long, but looks nice :-)
      if (!(is.null(plotRGL) || is.null(in.d) || is.null(ref.d)))
      {
        rglplotanalyzedata(ref.d, in.d, x1, x2, ref.res$path1, ref.res$path2,resultdata =   dftoanalyze, whattodraw = plotRGL, skeleton = skeleton)
      }

      #plotsmoothingresults(dftoanalyze,paste("DTWaf of", legend, "trajectory analysis"), plotifnoextreams = TRUE, plotsmoothed = FALSE)

      data.configuration.ret[[a]] <- dc
    } else
    {
      res.original <-analyzedta(x1, x2, FUN=FUN, smoothSize = smoothSize,
                       path1 = ref.res$path1, path2 = ref.res$path2, extremumtreshold = extremumtreshold)

      res <- tresholdresults(res.original, extremumtreshold)
      plotsmoothingresults(alignextremum(dftoanalyze, res, analyzerange),paste("DTWaf of ", legend, " trajectory analysis", sep = ""), plotifnoextreams = TRUE, plotsmoothed = FALSE, ylab = ylab)

      dc$results <- res.original
      dc$tresholded.results <- res
      data.configuration.ret[[a]] <- dc
    }
  }
  return(data.configuration.ret)
}


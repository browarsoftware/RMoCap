#' This function recalculates direct to hierarchical kinematic model.
#'
#' Procedure implements iterative algebraic procedure with additional initial optimization, that is required to align root body joints.
#' Optimization is done using simplex method. The rotation order in hierarchical model is automatically set to ZYX, even if input.skeleton has different order.
#'
#' @param input.skeleton object of mocap class that defines hierarchical kinematic model.
#' @param df.to.save data frame with column names compatible with input.skeleton. Data that is used for calculation has to be placed in columns with names ending .Dx, .Dy and .Dz.
#' @param plot.me if TRUE plot steps of skeleton aligning of frame with index frame.id. Default value is plot.me = FALSE.
#' @param frame.id if frame.id > 0 and plot.me = TRUE plot steps of skeleton aligning of frame with index frame.id. Default value is frame.id = -1.
#' @param debug.messages print additional messages informing about calculation progress.
#'
#' @return object of class mocap.
#'
#' @examples
#'   data("header.mocap")
#'   data("heian.yondan")
#'
#'   input.skeleton <- header.mocap
#'
#'   df.to.save <- heian.yondan[1:300,]
#'   first.frame <- df.to.bvh(input.skeleton, df.to.save, plot.me = FALSE, debug.messages = TRUE)
#'   write.bvh(first.frame, "e:\\bvh in r\\gotowy_kod\\output\\heian.yondan.frames300.bvh")
#'
#'   plot(first.frame$skeleton$Joints[[1]]$Rxyz[,1], type = "l", col = "black", xlab = "sample", ylab = "angle (degrees)")
#'   lines(first.frame$skeleton$Joints[[1]]$Rxyz[,2], type = "l", col = "red")
#'   lines(first.frame$skeleton$Joints[[1]]$Rxyz[,3], type = "l", col = "blue")
#'   legend("bottomright", legend=c("X axis rotation", "Y axis rotation", "Z axis rotation"), col=c("black", "red", "blue"), lty = 1)
#'   title("Hips rotation data")
#'
#'   plot(df.to.save[,2], ylab = "Displacement [cm]", xlab = "Time [10^-2 sec]", pch = 1)
#'   for (a in 1:ncol(df.to.save))
#'   {
#'      df.to.save[,a] <- jitter(df.to.save[,a], factor = 500)
#'   }
#'   points(df.to.save[,2],col="red", pch = 2)
#'   legend("bottomright", legend=c("Original", "Jitter"), col=c("black", "red"), pch = c(1,2))
#'   title("Example channel of MoCap data")
#'
#'   first.frame <- df.to.bvh(input.skeleton, df.to.save, plot.me = FALSE, debug.messages = TRUE)
#'
#'   #plot rotation data
#'   plot(first.frame$skeleton$Joints[[1]]$Rxyz[,1], type = "l", col = "black", xlab = "sample", ylab = "angle (degrees)")
#'   lines(first.frame$skeleton$Joints[[1]]$Rxyz[,2], type = "l", col = "red")
#'   lines(first.frame$skeleton$Joints[[1]]$Rxyz[,3], type = "l", col = "blue")
#'   legend("bottomright", legend=c("X axis rotation", "Y axis rotation", "Z axis rotation"), col=c("black", "red", "blue"), lty = 1)
#'   title("Hips rotation data")
#'
#'   write.bvh(first.frame, "e:\\bvh in r\\gotowy_kod\\output\\jitter.heian.yondan.frames300.bvh")
#'
#'   df.to.save <- heian.yondan[1000:1001,]
#'   foo <- df.to.bvh(input.skeleton, df.to.save, plot.me = TRUE, debug.messages = FALSE, frame.id = 1)
df.to.bvh <-function(input.skeleton, df.to.save, plot.me = FALSE, frame.id = -1, debug.messages = FALSE)
{
  for (a in 1:length(input.skeleton$skeleton$Joints))
  {
    input.skeleton$skeleton$Joints[[a]]$Order <- c(3,2,1)
  }



  if (frame.id != -1 && frame.id > nrow(df.to.save))
  {
    frame.id <- nrow(df.to.save)
  }
  first.frame <- generate.first.frame(input.skeleton, nrow(df.to.save))
  for (index in 1:nrow(df.to.save))
  {

    exclusion.vector <- c()
    list.to.visit <- list()


    find.child <- function(skeleton, parent.id = -1, exclusion.vector = c())
    {
      allchildren <- c()
      for (a in 1:length(skeleton$Joints))
      {
        if (skeleton$Joints[[a]]$Parent == parent.id)
        {
          if (!(a %in% exclusion.vector))
          {
            allchildren <- c(allchildren, a)
          }
        }
      }
      return (allchildren)
    }
    #parent - different processing
    #find all parent joints, works only if there are at least two children
    skeleton <- input.skeleton$skeleton

    parent.id <- find.child(skeleton)


    first.frame$skeleton$Joints[[parent.id]]$RawDxyz[index,1] <- df.to.save[index,paste(skeleton$Joints[[parent.id]]$Name, ".Dx", sep = "")] - first.frame$skeleton$Joints[[parent.id]]$Offset[1]
    first.frame$skeleton$Joints[[parent.id]]$RawDxyz[index,2] <- df.to.save[index,paste(skeleton$Joints[[parent.id]]$Name, ".Dy", sep = "")] - first.frame$skeleton$Joints[[parent.id]]$Offset[2]
    first.frame$skeleton$Joints[[parent.id]]$RawDxyz[index,3] <- df.to.save[index,paste(skeleton$Joints[[parent.id]]$Name, ".Dz", sep = "")] - first.frame$skeleton$Joints[[parent.id]]$Offset[3]

    first.frame <- generate.single.frame(first.frame, index)


    exclusion.vector <- c(exclusion.vector, parent.id)

    if (plot.me && frame.id == index)
    {
      plot(first.frame, my.color = rgb(runif(5),runif(5),runif(5)), frame = index, spheres = FALSE, alpha = (10/(length(skeleton$Joints)+2)))
    }



    #find all children of root joint
    ChildId <- find.child(skeleton, parent.id, exclusion.vector)
    #they are excluded from further finding
    exclusion.vector <- c(exclusion.vector, ChildId)

    for (a in 1:length(ChildId))
    {
      list.to.visit[[length(list.to.visit) + 1]] <- ChildId[a]
    }

    SecondChildId <- ChildId[2]
    ChildId <- ChildId[1]



    parent.dxyz <- c(df.to.save[index,paste(skeleton$Joints[[parent.id]]$Name, ".Dx", sep = "")],
                     df.to.save[index,paste(skeleton$Joints[[parent.id]]$Name, ".Dy", sep = "")],
                     df.to.save[index,paste(skeleton$Joints[[parent.id]]$Name, ".Dz", sep = "")])

    child.dxyz <- c(df.to.save[index,paste(skeleton$Joints[[ChildId]]$Name, ".Dx", sep = "")],
                    df.to.save[index,paste(skeleton$Joints[[ChildId]]$Name, ".Dy", sep = "")],
                    df.to.save[index,paste(skeleton$Joints[[ChildId]]$Name, ".Dz", sep = "")])

    map <- child.dxyz - parent.dxyz
    tomapowac <- first.frame$skeleton$Joints[[ChildId]]$Offset

    mapu <- vector.to.unit(map)
    tomapowacu <- vector.to.unit(tomapowac)


    Rx2y = rotation.matrix.between.vectors(tomapowacu, mapu)
    if (anyNA(Rx2y))
    {
      Rx2y <- matrix(c(1,0,0,0,1,0,0,0,1),nrow = 3, ncol = 3)
    }
    library(RSpincalc)

    ea <- DCM2EA(Rx2y, 'zyx') * 180 / pi


    eeaa <- ea * (pi / 180)
    q.lewe.biodro <- EA2Q(eeaa, 'zyx')
    Q2DCM(q.lewe.biodro)

    vectQrot(q.lewe.biodro, tomapowacu)

    input.skeleton$skeleton$Joints[[1]]$Trans[[1]]

    c(ea[3],ea[2],ea[1])
    input.skeleton$skeleton$Joints[[1]]$Rxyz[1,]

    first.frame$skeleton$Joints[[parent.id]]$Rxyz[index,1] <- ea[3]
    first.frame$skeleton$Joints[[parent.id]]$Rxyz[index,2] <- ea[2]
    first.frame$skeleton$Joints[[parent.id]]$Rxyz[index,3] <- ea[1]


    first.frame <- generate.single.frame(first.frame, index)
    axis <- first.frame$skeleton$Joints[[ChildId]]$Dxyz[index,] - first.frame$skeleton$Joints[[parent.id]]$Dxyz[index,]
    axisu <- vector.to.unit(axis)

    #########################################

    library('subplex')
    euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

    optimizeangle <- function(x)
    {
      q.prawe.biodro <- myEV2Q(axisu, x)
      q.oba.biodra <- q.prawe.biodro %Q*% q.lewe.biodro

      ea.oba.biodra <- Q2EA(q.oba.biodra, 'zyx', ignoreAllChk = TRUE) * 180 / pi

      first.frame$skeleton$Joints[[parent.id]]$Rxyz[index,1] <- ea.oba.biodra[3]
      first.frame$skeleton$Joints[[parent.id]]$Rxyz[index,2] <- ea.oba.biodra[2]
      first.frame$skeleton$Joints[[parent.id]]$Rxyz[index,3] <- ea.oba.biodra[1]

      first.frame <- generate.single.frame(first.frame, index = index)

      v11 <- c(df.to.save[index,paste(skeleton$Joints[[SecondChildId]]$Name, ".Dx", sep = "")],
               df.to.save[index,paste(skeleton$Joints[[SecondChildId]]$Name, ".Dy", sep = "")],
               df.to.save[index,paste(skeleton$Joints[[SecondChildId]]$Name, ".Dz", sep = "")])
      v2 <- first.frame$skeleton$Joints[[SecondChildId]]$Dxyz[index,]

      return (euc.dist(v11, v2))
    }
    response <- subplex(par=c(0),fn=optimizeangle)


    q.prawe.biodro <- myEV2Q(axisu, response$par)

    q.oba.biodra <- q.prawe.biodro %Q*% q.lewe.biodro

    ea.oba.biodra <- Q2EA(q.oba.biodra, 'zyx', ignoreAllChk = FALSE) * 180 / pi

    first.frame$skeleton$Joints[[parent.id]]$Rxyz[index,1] <- ea.oba.biodra[3]
    first.frame$skeleton$Joints[[parent.id]]$Rxyz[index,2] <- ea.oba.biodra[2]
    first.frame$skeleton$Joints[[parent.id]]$Rxyz[index,3] <- ea.oba.biodra[1]

    first.frame <- generate.single.frame(first.frame, index)

    if (plot.me && frame.id == index)
    {
      plot(first.frame, my.color = rgb(runif(5),runif(5),runif(5)), frame = index, append = TRUE, spheres = FALSE, alpha = (11/(length(skeleton$Joints)+2)))
    }

    while (length(list.to.visit) > 0)
    {
      parent.id <- list.to.visit[[1]]

      parent.id <- list.to.visit[[1]]

      list.to.visit <- list.to.visit[-1]

      ChildId <- find.child(skeleton, parent.id, exclusion.vector)
      if (length(ChildId) < 1)
      {
        next
      }
      exclusion.vector <- c(exclusion.vector, ChildId)

      for (a in 1:length(ChildId))
      {
        list.to.visit[[length(list.to.visit) + 1]] <- ChildId[a]
      }
      ChildId <- ChildId[1]

      parent.dxyz <- c(df.to.save[index,paste(skeleton$Joints[[parent.id]]$Name, ".Dx", sep = "")],
                       df.to.save[index,paste(skeleton$Joints[[parent.id]]$Name, ".Dy", sep = "")],
                       df.to.save[index,paste(skeleton$Joints[[parent.id]]$Name, ".Dz", sep = "")])

      child.dxyz <- c(df.to.save[index,paste(skeleton$Joints[[ChildId]]$Name, ".Dx", sep = "")],
                      df.to.save[index,paste(skeleton$Joints[[ChildId]]$Name, ".Dy", sep = "")],
                      df.to.save[index,paste(skeleton$Joints[[ChildId]]$Name, ".Dz", sep = "")])

      map <- child.dxyz - parent.dxyz

      rp <- solve(a=first.frame$skeleton$Joints[[skeleton$Joints[[parent.id]]$Parent]]$Trans[[index]][1:3,1:3])

      map <- as.vector(rp %*% map)

      tomapowac <- first.frame$skeleton$Joints[[ChildId]]$Offset

      mapu <- vector.to.unit(map)
      tomapowacu <- vector.to.unit(tomapowac)

      Rx2y = rotation.matrix.between.vectors(tomapowacu, mapu)
      if (anyNA(Rx2y))
      {
        Rx2y <- matrix(c(1,0,0,0,1,0,0,0,1),nrow = 3, ncol = 3)
      }
      tomapowacu %*% Rx2y


      library(RSpincalc)

      ea <- DCM2EA(Rx2y, 'zyx') * 180 / pi

      eeaa <- ea * (pi / 180)
      q.lewe.biodro <- EA2Q(eeaa, 'zyx')
      Q2DCM(q.lewe.biodro)



      first.frame$skeleton$Joints[[parent.id]]$Rxyz[index,1] <- ea[3]
      first.frame$skeleton$Joints[[parent.id]]$Rxyz[index,2] <- ea[2]
      first.frame$skeleton$Joints[[parent.id]]$Rxyz[index,3] <- ea[1]
      first.frame <- generate.single.frame(first.frame, index)

      if (plot.me && frame.id == index)
      {
        plot(first.frame, my.color = rgb(runif(5),runif(5),runif(5)), frame = index, append = TRUE,  spheres = FALSE, alpha = ((11+index)/(length(skeleton$Joints)+2)))
      }

    }
    if (debug.messages)
    {
      message(paste("Processed frame index:",index))
    }

  }
  return (first.frame)
}

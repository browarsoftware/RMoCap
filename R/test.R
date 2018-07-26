# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

test <- function() {


  if (!require(RMoCap))
  {
    if (!require(RMocap))
    {
      install.packages("devtools") # if you have not installed "devtools" package
    }
    devtools::install_github("browarsoftware/RMoCap")
  }

  ######################################
  #read BVH file
  ######################################
  #an example BVH file
  data("heian.nidan.bvh")
  f <- file("e:\\bvh in r\\gotowy_kod\\output\\heian.nidan.bvh")
  writeChar(con = f, object = heian.nidan.bvh)
  close(f)
  #read hierarchical model stored in hierarchical BVH file
  heian.nidan <- read.mocap("e:\\bvh in r\\gotowy_kod\\output\\heian.nidan.bvh")
  summary(heian.nidan)
  #plot kinematic data
  plot(x = heian.nidan$data.frame$Hips.Dx, y = heian.nidan$data.frame$Hips.Dz, type = "l", ylab = "Displacement X [cm]", xlab = "Displacement Z [cm]")
  title("Hips displacement during motion")

  #generate kinematic from hierarchical model - same results as above
  df <- hierarchical.to.direct.kinematic(heian.nidan$skeleton)
  plot(x = df$Hips.Dx, y = df$Hips.Dz, type = "l", ylab = "Displacement X [cm]", xlab = "Displacement Z [cm]")
  title("Hips displacement during motion")

  #plot
  plot(heian.nidan, frame = 1, my.color = "white", alpha = 1, spheres = TRUE, print.text = TRUE)
  plot(heian.nidan, frames.fraction = 0.1, my.color = "white", alpha = 0.1, spheres = FALSE)

  ######################################
  #recalculate direct to hierarchical kinematic model
  ######################################
  data("header.mocap")
  data("heian.yondan")

  input.skeleton <- header.mocap

  df.to.save <- heian.yondan[1:250,]
  first.frame <- df.to.bvh(input.skeleton, df.to.save, plot.me = FALSE, debug.messages = TRUE)
  write.bvh(first.frame, "e:\\bvh in r\\gotowy_kod\\output\\heian.yondan.frames250.bvh")

  plot(df.to.save[,2], ylab = "Displacement [cm]", xlab = "Time [10^-2 sec]", pch = 1)
  for (a in 1:ncol(df.to.save))
  {
    df.to.save[,a] <- jitter(df.to.save[,a], factor = 500)
  }
  points(df.to.save[,2],col="red", pch = 2)
  legend("bottomright", legend=c("Original", "Jitter"), col=c("black", "red"), pch = c(1,2))
  title("Example channel of MoCap data")

  first.frame <- df.to.bvh(input.skeleton, df.to.save, plot.me = FALSE, debug.messages = TRUE)

  #plot rotation data
  plot(first.frame$skeleton$Joints[[1]]$Rxyz[,1], type = "l", col = "black")
  lines(first.frame$skeleton$Joints[[1]]$Rxyz[,2], type = "l", col = "red")
  lines(first.frame$skeleton$Joints[[1]]$Rxyz[,3], type = "l", col = "blue")
  legend("bottomright", legend=c("X axis rotation", "Y axis rotation", "Z axis rotation"), col=c("black", "red", "blue"), lty = 1)
  title("Hips rotation data")

  write.bvh(first.frame, "e:\\bvh in r\\gotowy_kod\\output\\jitter.heian.yondan.frames300.bvh")

  df.to.save <- heian.yondan[1000:1001,]
  foo <- df.to.bvh(input.skeleton, df.to.save, plot.me = TRUE, debug.messages = FALSE, frame.id = 1)


  ######################################
  #motion direction correction
  ######################################
  data("header.mocap")
  data("heian.shodan")

  heian.shodan.corrected <- calculate.kinematic(heian.shodan, show.plot = "TRUE", plot.title = "Heian Shodan")
  original.bvh <- set.data.frame(header.mocap, heian.shodan)
  corrected.bvh <- set.data.frame(header.mocap, heian.shodan.corrected)
  #plotting BVH
  plot(original.bvh, frames.fraction = 0.1, my.color = "red", alpha = 0.1, spheres = FALSE)
  plot(corrected.bvh, frames.fraction = 0.1, my.color = "green", alpha = 0.1, spheres = FALSE, append = TRUE)

  #writting BVH to disk
  write.bvh(original.bvh, "e:\\bvh in r\\gotowy_kod\\output\\original.bvh")
  write.bvh(corrected.bvh, "e:\\bvh in r\\gotowy_kod\\output\\corrected.bvh")

  ###############################
  #Motion data averaging
  ###############################

  data("mawashi.geri.right.list")
  myList <- list()
  #Use only data frames
  for (a in 1:length(mawashi.geri.right.list))
  {
    myList[[a]] <-mawashi.geri.right.list[[a]]$data.frame
  }
  #set seed for repeatable results
  set.seed(123)
  res.data <- mocap.averagingCmp(myList, 50, eps = 0.000001)
  plot(res.data)
  #save results in BVH file
  skel <- set.data.frame(mawashi.geri.right.list[[1]], res.data$fullData)
  write.bvh(path = "e:\\bvh in r\\gotowy_kod\\output\\mawashi_50.bvh", skeleton.helper = skel)


  ######################################
  #motion capture analysis - hands
  ######################################
  data(right.arm.motion.1)
  data(right.arm.motion.2)

  refdata <- right.arm.motion.1$data.frame
  inputdata <- right.arm.motion.2$data.frame

  extremumtreshold <- 0.66
  smoothSize <- 0.1


  inputdataalignment <- rotatedata(inputdata, refdata, "LeftShoulder","RightShoulder")
  inputdataalignmentkinematic <- calculate.kinematic(inputdataalignment, bodypartname = "LeftShoulder")
  refdatakinematic <- calculate.kinematic(refdata, bodypartname = "LeftShoulder")

  inputdataalignmentkinematic <- aligninputandrefdata(inputdataalignmentkinematic, refdatakinematic, limbname = "LeftShoulder")


  #setup kinematic chain
  data.configuration <- list()
  data.configuration[[1]] <- list(x1 = vector.to.list(refdatakinematic, "RightHand"),
                    x2 = vector.to.list(inputdataalignmentkinematic, "RightHand"),
                    FUN = euc.dist,
                    ylab = "Distance [cm]",
                    legend = "RightHand",
                    plotRGL = "RightHand",
                    skeleton = right.arm.motion.1)

  data.configuration[[2]] <- list(x1 = vector.to.angles.list(refdatakinematic, "RightShoulder", "RightArm", "RightForearm"),
                    x2 = vector.to.angles.list(inputdataalignmentkinematic, "RightShoulder", "RightArm", "RightForearm"),
                    FUN = euc.dist1d,
                    ylab = "Angle [rad]",
                    legend = "Right elbow",
                    plotRGL = NULL,
                    skeleton = NULL)


  x1 <- vector.to.angles.frame.list(refdatakinematic, "RightArm", "RightForearm", "RightShoulder", "LeftShoulder")
  x2 <- vector.to.angles.frame.list(inputdataalignmentkinematic, "RightArm", "RightForearm", "RightShoulder", "LeftShoulder")

  data.configuration[[3]] <- list(x1 = x1[[1]],
                    x2 = x2[[1]],
                    FUN = euc.dist1d,
                    ylab = "Angle [rad]",
                    legend = "X angle between RightArm and RightForearm",
                    plotRGL = NULL,
                    skeleton = NULL)

  data.configuration[[4]] <- list(x1 = x1[[2]],
                                  x2 = x2[[2]],
                                  FUN = euc.dist1d,
                                  ylab = "Angle [rad]",
                                  legend = "Y angle between RightArm and RightForearm",
                                  plotRGL = NULL,
                                  skeleton = NULL)

  data.configuration[[5]] <- list(x1 = x1[[3]],
                                  x2 = x2[[3]],
                                  FUN = euc.dist1d,
                                  ylab = "Angle [rad]",
                                  legend = "Z angle between RightArm and RightForearm",
                                  plotRGL = NULL,
                                  skeleton = NULL)


  res <- analyze.mocap(data.configuration,
                refdatakinematic,
                inputdataalignmentkinematic,
                extremumtreshold,
                smoothSize)


  ######################################
  #motion capture analysis - legs
  ######################################

  data(mawashi.geri.left.1)
  data(mawashi.geri.left.2)

  refdata <- mawashi.geri.left.1$data.frame
  inputdata <- mawashi.geri.left.2$data.frame

  extremumtreshold <- 0.66
  smoothSize <- 0.1


  inputdataalignment <- rotatedata(inputdata, refdata, "LeftThigh","RightThigh")
  inputdataalignmentkinematic <- calculate.kinematic(inputdataalignment, bodypartname = "RightFoot")
  refdatakinematic <- calculate.kinematic(refdata, bodypartname = "RightFoot")

  inputdataalignmentkinematic <- aligninputandrefdata(inputdataalignmentkinematic, refdatakinematic, limbname = "RightFoot")


  #setup kinematic chain
  data.configuration <- list()
  data.configuration[[1]] <- list(x1 = vector.to.list(refdatakinematic, "LeftFoot"),
                                  x2 = vector.to.list(inputdataalignmentkinematic, "LeftFoot"),
                                  FUN = euc.dist,
                                  ylab = "Distance [cm]",
                                  legend = "LeftFoot",
                                  plotRGL = "LeftFoot",
                                  skeleton = mawashi.geri.left.1)

  data.configuration[[2]] <- list(x1 = vector.to.angles.list(refdatakinematic, "LeftThigh", "LeftLeg", "LeftFoot"),
                                  x2 = vector.to.angles.list(inputdataalignmentkinematic, "LeftThigh", "LeftLeg", "LeftFoot"),
                                  FUN = euc.dist1d,
                                  ylab = "Angle [rad]",
                                  legend = "Left knee",
                                  plotRGL = NULL)


  x1 <- vector.to.angles.frame.list(refdatakinematic, "LeftThigh", "LeftLeg", "LeftThigh","RightThigh")
  x2 <- vector.to.angles.frame.list(inputdataalignmentkinematic, "LeftThigh", "LeftLeg", "LeftThigh","RightThigh")

  data.configuration[[3]] <- list(x1 = x1[[1]],
                                  x2 = x2[[1]],
                                  FUN = euc.dist1d,
                                  ylab = "Angle [rad]",
                                  legend = "X angle between LeftThigh and LeftLeg",
                                  plotRGL = NULL)

  data.configuration[[4]] <- list(x1 = x1[[2]],
                                  x2 = x2[[2]],
                                  FUN = euc.dist1d,
                                  ylab = "Angle [rad]",
                                  legend = "Y angle between LeftThigh and LeftLeg",
                                  plotRGL = NULL)

  data.configuration[[5]] <- list(x1 = x1[[3]],
                                  x2 = x2[[3]],
                                  FUN = euc.dist1d,
                                  ylab = "Angle [rad]",
                                  legend = "Z angle between LeftThigh and LeftLeg",
                                  plotRGL = NULL)


  res <- analyze.mocap(data.configuration,
                refdatakinematic,
                inputdataalignmentkinematic,
                extremumtreshold,
                smoothSize)


}

#' A class returned by read.mocap function.
#'
#' @docType class
#' @usage read.mocap("file.name.bvh")
#' @format
#' a list containing:
#' \itemize{
#'   \item Joints - list of joints,
#'   \item Time - vector with time series,
#'   \item FrameTime - value of time interval between samples
#'   \item Frame - and samples count.
#' }
#' Each joint is a list that contains:
#' \itemize{
#'   \item Nestdepth - level of joint in hierarchy,
#'   \item Name - name of the joint,
#'   \item Parent - id of the parent on the list, root joint has parent = -1,
#'   \item Offset - 3D vector with offset from parent joint,
#'   \item Nchannels - number of data channels (6 from root, 3 for other or 0 for end point),
#'   \item Order - rotation order (accepted orders are XYZ, XZY, YXZ, YZX, ZXY or ZYX),
#'   \item Dxyz - matrix with direct kinematic displacement (calculated from original data),
#'   \item RawDxyz - matrix with direct kinematic displacement, present only in root joint,
#'   \item Rxyz - matrix with rotation in degrees of hierarchical kinematic model
#'   \item Trans - list of rotation - translation matrices that are used to recalculates hierarchical to direct kinematic model.
#' }
#'
#' @keywords class
#' @examples
#' #an example BVH file
#' data("heian.nidan.bvh")
#' #write file to the disc
#' f <- file("heian.nidan.bvh")
#' writeChar(con = f, object = heian.nidan.bvh)
#' close(f)
#' #read hierarchical model stored in BVH file
#' heian.nidan <- read.mocap("heian.nidan.bvh")
#' summary(heian.nidan)
mocap <- setClass("mocap")

#' Plots information about number of frames, joints count and joints chierarchy of mocap class.
#'
#' @param mocap.data an object of mocap class.
#'
#' @examples
#' data("heian.nidan.bvh")
#' f <- file("e:\\bvh in r\\gotowy_kod\\output\\heian.nidan.bvh")
#' writeChar(con = f, object = heian.nidan.bvh)
#' close(f)
#' #read hierarchical model stored in hierarchical BVH file
#' heian.nidan <- read.mocap("e:\\bvh in r\\gotowy_kod\\output\\heian.nidan.bvh")
#' summary(heian.nidan)
summary.mocap <-function(mocap.data)
{
  message(paste("Frames count:", heian.nidan$skeleton$Frames))
  message(paste("Joints count:", length(heian.nidan$skeleton$Joints)))
  message("Joints hierarchy:")
  for (a in 1:length(heian.nidan$skeleton$Joints))
  {
    tabs <- ""
    if (heian.nidan$skeleton$Joints[[a]]$Nestdepth - 1 > 0)
    {
      tabs <- ""
      for (b in 1:(heian.nidan$skeleton$Joints[[a]]$Nestdepth - 1))
           tabs <- paste(tabs,"  ", sep="")
      tabs <- paste(tabs,"+-> ", sep="")
    }
    message(paste(tabs, heian.nidan$skeleton$Joints[[a]]$Name, sep = ""))
  }
}


#' A raw bvh file to be saved on disc (Shotokan Karate kata Heian Nidan).
#'
#' @docType data
#'
#' @usage data(heian.nidan.bvh)
#'
#' @format A raw file.
#'
#' @keywords datasets
#'
#' @examples
#' data("heian.nidan.bvh")
#' f <- file("e:\\bvh in r\\gotowy_kod\\output\\heian.nidan.bvh")
#' writeChar(con = f, object = heian.nidan.bvh)
#' close(f)
"heian.nidan.bvh"


#' An example object of class mocap that does not have any motion data.
#'
#' @docType data
#'
#' @usage data("header.mocap")
#'
#' @format An object of mocap class.
#'
#' @keywords datasets
"header.mocap"

#' A data frame with mocap data (Shotokan Karate kata Heian Yondan).
#'
#' @docType data
#'
#' @usage data("heian.yondan")
#'
#' @format A data frame.
#'
#' @keywords datasets
"heian.yondan"


#' A data frame with mocap data (Shotokan Karate kata Heian Shodan). Also acceleration channels are included.
#'
#' @docType data
#'
#' @usage data("heian.shodan")
#'
#' @format A data frame.
#'
#' @keywords datasets
"heian.shodan"



#' An object of class mocap that contains motion of right arm.
#'
#' @docType data
#'
#' @usage data("right.arm.motion.2")
#'
#' @format An object of mocap class.
#'
#' @keywords datasets
"right.arm.motion.2"


#' An object of class mocap that contains karate kick nawashi geri.
#'
#' @docType data
#'
#' @usage data("mawashi.geri.left.1")
#'
#' @format An object of mocap class.
#'
#' @keywords datasets
"mawashi.geri.left.1"


#' An object of class mocap that contains karate kick nawashi geri.
#'
#' @docType data
#'
#' @usage data("mawashi.geri.left.2")
#'
#' @format An object of mocap class.
#'
#' @keywords datasets
"mawashi.geri.left.2"


#' A list of ten object of class mocap that contains karate kicks mawashi geri.
#'
#' @docType data
#'
#' @usage data("mawashi.geri.right.list")
#'
#' @format A list of objects of mocap class.
#'
#' @keywords datasets
"mawashi.geri.right.list"





#' This function returns length (norm) of the vector.
#'
#' @param x a vector.
#'
#' @return vector norm.
#'
#' @examples
#' vector.norm(c(1,2,3))
vector.norm <- function(x) sqrt(sum(x^2))


#' This function performs vector normalizing.
#'
#' @param x a vector.
#'
#' @return normalized vector x.
#'
#' @examples
#' vector.to.unit(c(1,2,3,4,5))
vector.to.unit <- function(x) {x / sqrt(sum(x^2))}

#' This function calculates cross product.
#'
#' Cross product is only defined for 3D vectors.
#' @param a first vector.
#' @param b second vector.
#'
#' @return cross product.
#'
#' @examples
#' vector.cross(c(1,2,3),c(3,4,5))
vector.cross <- function(a, b) {
  if(length(a)!=3 || length(b)!=3){
    stop("Cross product is only defined for 3D vectors.");
  }
  i1 <- c(2,3,1)
  i2 <- c(3,1,2)
  return (a[i1]*b[i2] - a[i2]*b[i1])
}

#' This function calculates dot product.
#'
#' @param a first vector.
#' @param b second vector.
#'
#' @return dot product.
#'
#' @examples
#' vector.dot(c(1,2,3,4),c(5,6,7,8))
vector.dot <- function(a,b) sum(a*b)



#' This function calculates angle between two vectors on the plane designed by those vectors.
#'
#' The return value is in the range [0,pi].
#'
#' @param a first vector.
#' @param b second vector.
#'
#' @return angle between a and b (in radians).
#'
#' @examples
#' vector.angle(c(1,2,3),c(4,5,6))
vector.angle <- function(a,b)
{
  a.unit <- vector.to.unit(a)
  b.unit <- vector.to.unit(b)
  return (acos(vector.dot(a.unit, b.unit)))
}


#Undocumented helper function, that calculates rotation matrix
transformation_matrix <- function(disp, rxyz, order)
{

  MX <- matrix(c(1,0,0,
                 0,cos(rxyz[1] * pi / 180.0),-sin(rxyz[1] * pi / 180.0),
                 0,sin(rxyz[1] * pi / 180.0),cos(rxyz[1] * pi / 180.0)),nrow = 3, ncol = 3, byrow = TRUE)


  MY <- matrix(c(cos(rxyz[2] * pi / 180.0),0,sin(rxyz[2] * pi / 180.0),
                 0,1,0,
                 -sin(rxyz[2] * pi / 180.0),0,cos(rxyz[2] * pi / 180.0)),nrow = 3, ncol = 3, byrow = TRUE)

  MZ <- matrix(c(cos(rxyz[3] * pi / 180.0),-sin(rxyz[3] * pi / 180.0),0,
                 sin(rxyz[3] * pi / 180.0),cos(rxyz[3] * pi / 180.0),0,
                 0,0,1),nrow = 3, ncol = 3, byrow = TRUE)


  if (order[1] == 1)
  {
    rotM <- MX
  } else if (order[1] == 2)
  {
    rotM <- MY
  } else if (order[1] == 3)
  {
    rotM <- MZ
  }

  if (order[2] == 1)
  {
    rotM <- rotM %*% MX #ultiplyByMatrix(rotM,MX)
  } else if (order[2] == 2)
  {
    rotM <- rotM %*%  MY #multiplyByMatrix(rotM,MY)
  } else if (order[2] == 3)
  {
    rotM <- rotM %*% MZ #multiplyByMatrix(rotM,MZ)
  }

  if (order[3] == 1)
  {
    rotM <- rotM %*% MX #multiplyByMatrix(rotM,MX)
  } else if (order[3] == 2)
  {
    rotM <- rotM %*% MY #multiplyByMatrix(rotM,MY)
  } else if (order[3] == 3)
  {
    rotM <- rotM %*% MZ #multiplyByMatrix(rotM,MZ)
  }

  transM <- matrix(c(rotM[1,1],rotM[1,2],rotM[1,3],disp[1],
                     rotM[2,1],rotM[2,2],rotM[2,3],disp[2],
                     rotM[3,1],rotM[3,2],rotM[3,3],disp[3],
                     0,0,0,1), nrow = 4, ncol = 4, byrow = TRUE)

  return (transM)
}

#' This function read file in BVH (Biovision Hierarchy) format and recalculates it to direct kinematic model.
#'
#' BVH file contains a hierarchical kinematic model definition (called a skeleton) and motion data. For more information see:
#' "Motion Capture File Formats Explained" by M. Meredith S. Maddock, doi: 10.1.1.103.2097.
#' @param filepath A path to BVH file.
#'
#' @return an object of mocap class.
#'
#' @examples
#' #an example BVH file
#' data("heian.nidan.bvh")
#' #write file to the disc
#' f <- file("heian.nidan.bvh")
#' writeChar(con = f, object = heian.nidan.bvh)
#' close(f)
#' #read hierarchical model stored in BVH file
# heian.nidan <- read.bvh("heian.nidan.bvh")

read.bvh <- function(filepath)
{
  all_tokens <- c()

  con = file(filepath, "r")
  while ( TRUE ) {
    line = readLines(con, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    #replace \t by " "
    line <- gsub("\t", " ", line)
    #split by  " "
    splitted_line <- unlist(strsplit(line, " "))
    #remove empty "" strings
    splitted_line <- splitted_line[splitted_line != ""]

    all_tokens <- c(all_tokens, splitted_line)

    #print(line)
  }
  close(con)

  ii = 1
  nn = 0
  brace_count = 1

  skeleton <- list()
  skeleton$Joints <- list()
  length(skeleton$Joints)

  while (all_tokens[ii] != "MOTION")
  {
    ii = ii+1
    token = all_tokens[ii]
    if (token == "{")
    {
      brace_count = brace_count + 1
    } else if (token == "}")
    {
      brace_count = brace_count - 1
    } else if (token == "}")
    {
      brace_count = brace_count - 1
    } else if (token == "OFFSET")
    {
      if (nn -1 >= length(skeleton$Joints))
      {
        skeleton$Joints[[length(skeleton$Joints) + 1]] <- list()
        skeleton$Joints[[nn]]$Nestdepth <- 0
      }
      skeleton$Joints[[nn]]$Offset <- c(as.numeric(all_tokens[ii + 1]),
                                        as.numeric(all_tokens[ii + 2]),
                                        as.numeric(all_tokens[ii + 3]))
      ii <- ii+3
    } else if (token == "CHANNELS")
    {
      skeleton$Joints[[nn]]$Nchannels <- as.numeric(all_tokens[ii + 1])
      #The 'order' field is an index corresponding to the order of 'X' 'Y' 'Z'.
      #Subtract 87 because char numbers "X" == 88, "Y" == 89, "Z" == 90.
      if (skeleton$Joints[[nn]]$Nchannels == 3)
      {
        skeleton$Joints[[nn]]$Order <- c( strtoi(charToRaw(substr(all_tokens[ii + 2], 1, 1)), base = 16L) - 87,
                                          strtoi(charToRaw(substr(all_tokens[ii + 3], 1, 1)), base = 16L) - 87,
                                          strtoi(charToRaw(substr(all_tokens[ii + 4], 1, 1)), base = 16L) - 87)

      } else if (skeleton$Joints[[nn]]$Nchannels == 6)
      {
        skeleton$Joints[[nn]]$Order <- c( strtoi(charToRaw(substr(all_tokens[ii + 5], 1, 1)), base = 16L) - 87,
                                          strtoi(charToRaw(substr(all_tokens[ii + 6], 1, 1)), base = 16L) - 87,
                                          strtoi(charToRaw(substr(all_tokens[ii + 7], 1, 1)), base = 16L) - 87)
      } else
      {
        #throw new BVHLoaderException("Not sure how to handle not (3 or 6) number of channels.")
      }

      #if ~all(sort(skeleton(nn).order)==[1 2 3])
      #error('Cannot read channels order correctly. Should be some permutation of [''X'' ''Y'' ''Z''].')
      #end
      ii <- ii + skeleton$Joints[[nn]]$Nchannels + 1
    } else if (token == "JOINT" || token == "ROOT")
    {

      nn <- nn+1
      if (nn -1 >= length(skeleton$Joints))
      {
        #print(token)
        skeleton$Joints[[length(skeleton$Joints) + 1]] <- list()
        skeleton$Joints[[nn]]$Nestdepth <- 0
      }
      skeleton$Joints[[nn]]$Name <- all_tokens[ii + 1]
      skeleton$Joints[[nn]]$Nestdepth <- brace_count
      if (brace_count == 1)
      {
        #root node
        skeleton$Joints[[nn]]$Parent = -1
      } else if (skeleton$Joints[[nn-1]]$Nestdepth + 1 == brace_count)
      {
        #if I am a child, the previous node is my parent
        skeleton$Joints[[nn]]$Parent <- nn - 1
      } else
      {
        #if not, what is the node corresponding to this brace count?
        prev_parent <- skeleton$Joints[[nn - 1]]$Parent
        while (skeleton$Joints[[prev_parent]]$Nestdepth + 1 != brace_count)
        {
          prev_parent <- skeleton$Joints[[prev_parent]]$Parent
        }
        skeleton$Joints[[nn]]$Parent = prev_parent
      }
      ii <- ii+1
    } else if (all_tokens[ii] == "End" && all_tokens[ii+1] == "Site")
    {
      #End effector; unnamed terminating joint
      #N.B. The "two word" token here is why we don't use a switch statement
      #for this code.
      nn <- nn+1
      if (nn -1 >= length(skeleton$Joints))
      {
        skeleton$Joints[[length(skeleton$Joints) + 1]] <- list()
        skeleton$Joints[[nn]]$Nestdepth <- 0
      }
      skeleton$Joints[[nn]]$Name = paste("EndSite", nn, sep = "")
      skeleton$Joints[[nn]]$Offset = c( as.numeric(all_tokens[ii+4]),
                                        as.numeric(all_tokens[ii+5]),
                                        as.numeric(all_tokens[ii+6]))
      skeleton$Joints[[nn]]$Parent <- nn - 1#always the direct child
      skeleton$Joints[[nn]]$Nestdepth <- brace_count
      skeleton$Joints[[nn]]$Nchannels <- 0
    }
  }#while (!all_tokens.get(ii).equals("MOTION"))


  #/Initial processing and error checking
  Nnodes <- length(skeleton$Joints)
  Nchannels <- 0
  Nchainends <- 0
  for (a in 1:length(skeleton$Joints))
  {
    Nchannels <- Nchannels + skeleton$Joints[[a]]$Nchannels
    if (skeleton$Joints[[a]]$Nchannels == 0)
    {
      Nchainends <- Nchainends + 1
    }
  }
  #Calculate number of header lines:
  #  - 5 lines per joint
  #  - 4 lines per chain end
  #  - 4 additional lines (first one and last three)
  Nheaderlines <- (Nnodes-Nchainends)*5 + Nchainends*4 + 4
  ii <- ii +1 #/MOTION
  ii <- ii +1 #Frames:

  Nframes <- as.numeric(all_tokens[ii])
  ii <- ii + 3 #Frame time:
  frame_time = as.numeric(all_tokens[ii])

  skeleton$Time <- rep(0, Nframes)
  skeleton$FrameTime <- frame_time
  skeleton$Frames <- Nframes
  if (length(skeleton$Time) > 1)
  {
    for (a in 2:length(skeleton$Time))
    {
      skeleton$Time[a] = skeleton$Time[a - 1] + frame_time
    }
  }
  ii <- ii + 1
  raw_data_size <- length(all_tokens) - ii + 1
  raw_data_size_help <- Nchannels * Nframes

  if (raw_data_size != raw_data_size_help)
  {
    #throw new BVHLoaderException("Error reading BVH file: channels count or frames count does not match.");
  }

  rawdatavector <- rep(0,Nframes * Nchannels)
  rawdata <- matrix(rawdatavector, nrow = Nframes, ncol = Nchannels)
  for (a in 1:Nframes)
  {
    for (b in 1:Nchannels)
    {
      rawdata[a,b] <- as.numeric(all_tokens[ii])
      ii <- ii + 1
    }
  }

  ###############
  channel_count <- 0
  for (nn in 1:Nnodes)
  {
    if (skeleton$Joints[[nn]]$Nchannels == 6)#root node
    {
      #assume translational data is always ordered XYZ
      skeleton$Joints[[nn]]$Dxyz <- matrix(rep(0, Nframes * 3), nrow = Nframes, ncol = 3)
      skeleton$Joints[[nn]]$RawDxyz <- matrix(rep(0, Nframes * 3), nrow = Nframes, ncol = 3)
      for (b in 1:Nframes)
      {
        skeleton$Joints[[nn]]$RawDxyz[b,1] <- rawdata[b,channel_count + 1]
        skeleton$Joints[[nn]]$RawDxyz[b,2] <- rawdata[b,channel_count + 2]
        skeleton$Joints[[nn]]$RawDxyz[b,3] <- rawdata[b,channel_count + 3]

        skeleton$Joints[[nn]]$Dxyz[b,1] <- skeleton$Joints[[nn]]$Offset[1] + rawdata[b,channel_count + 1]
        skeleton$Joints[[nn]]$Dxyz[b,2] <- skeleton$Joints[[nn]]$Offset[2] + rawdata[b,channel_count + 2]
        skeleton$Joints[[nn]]$Dxyz[b,3] <- skeleton$Joints[[nn]]$Offset[3] + rawdata[b,channel_count + 3]
      }
      skeleton$Joints[[nn]]$Rxyz = matrix(rep(0, Nframes * 3), nrow = Nframes, ncol = 3)
      for (b in 1:Nframes)
      {
        skeleton$Joints[[nn]]$Rxyz[b,skeleton$Joints[[nn]]$Order[1]] <- rawdata[b,channel_count + 4]
        skeleton$Joints[[nn]]$Rxyz[b,skeleton$Joints[[nn]]$Order[2]] <- rawdata[b,channel_count + 5]
        skeleton$Joints[[nn]]$Rxyz[b,skeleton$Joints[[nn]]$Order[3]] <- rawdata[b,channel_count + 6]
      }
      #Kinematics of the root element:
      skeleton$Joints[[nn]]$Trans <- list()
      for (b in 1:Nframes)
      {
        skeleton$Joints[[nn]]$Trans[[b]] = transformation_matrix(
          skeleton$Joints[[nn]]$Dxyz[b,],
          skeleton$Joints[[nn]]$Rxyz[b,],
          skeleton$Joints[[nn]]$Order
        )
      }
    }
    if (skeleton$Joints[[nn]]$Nchannels == 3)#joint node
    {
      skeleton$Joints[[nn]]$Rxyz = matrix(rep(0, Nframes * 3), nrow = Nframes, ncol = 3)
      for (b in 1:Nframes)
      {
        skeleton$Joints[[nn]]$Rxyz[b,skeleton$Joints[[nn]]$Order[1]] <- rawdata[b,channel_count + 1]
        skeleton$Joints[[nn]]$Rxyz[b,skeleton$Joints[[nn]]$Order[2]] <- rawdata[b,channel_count + 2]
        skeleton$Joints[[nn]]$Rxyz[b,skeleton$Joints[[nn]]$Order[3]] <- rawdata[b,channel_count + 3]
      }
      skeleton$Joints[[nn]]$Dxyz = matrix(rep(0, Nframes * 3), nrow = Nframes, ncol = 3)


      skeleton$Joints[[nn]]$Trans <- list()
      for (a in 1:Nframes)
      {
        skeleton$Joints[[nn]]$Trans[[a]] = matrix(rep(0,16), nrow = 4, ncol = 4)
      }

    }
    if (skeleton$Joints[[nn]]$Nchannels == 0)#end node
    {
      skeleton$Joints[[nn]]$Dxyz <- matrix(rep(0, Nframes * 3), nrow = Nframes, ncol = 3)
    }
    channel_count <- channel_count + skeleton$Joints[[nn]]$Nchannels
  }


  #skeleton$Joints[[1]]$Dxyz[1,]
  #skeleton$Joints[[1]]$Rxyz[1,]
  #skeleton$Joints[[1]]$Trans[[1]]

  #Calculate kinematics
  #No calculations are required for the root nodes.
  #For each joint, calculate the transformation matrix and for convenience
  #extract each position in a separate vector.
  for ( nn in 1:length(skeleton$Joints))
  {
    if (skeleton$Joints[[nn]]$Parent != -1 && skeleton$Joints[[nn]]$Nchannels != 0)
    {
      parent <- skeleton$Joints[[nn]]$Parent
      for (ff in 1:Nframes)
      {
        transM <- transformation_matrix(
          skeleton$Joints[[nn]]$Offset,
          skeleton$Joints[[nn]]$Rxyz[ff,],
          skeleton$Joints[[nn]]$Order)

        skeleton$Joints[[nn]]$Trans[[ff]] <-skeleton$Joints[[parent]]$Trans[[ff]] %*% transM# multiplyByMatrix(
        #skeleton$Joints[[parent]]$Trans[[ff]],
        #transM)

        skeleton$Joints[[nn]]$Dxyz[ff,1] <- skeleton$Joints[[nn]]$Trans[[ff]][1,4]
        skeleton$Joints[[nn]]$Dxyz[ff,2] <- skeleton$Joints[[nn]]$Trans[[ff]][2,4]
        skeleton$Joints[[nn]]$Dxyz[ff,3] <- skeleton$Joints[[nn]]$Trans[[ff]][3,4]
      }
    }
  }

  #For an end effector we don't have rotation data;
  #just need to calculate the final position.
  for (nn in 1:length(skeleton$Joints))
  {
    if (skeleton$Joints[[nn]]$Nchannels == 0)
    {
      mm <- matrix(c(1,0,0,skeleton$Joints[[nn]]$Offset[1],
                     0,1,0,skeleton$Joints[[nn]]$Offset[2],
                     0,0,1,skeleton$Joints[[nn]]$Offset[3],
                     0,0,0,1), nrow = 4, ncol = 4, byrow = TRUE)

      parent <- skeleton$Joints[[nn]]$Parent
      for (ff in 1:Nframes)
      {
        transM <- skeleton$Joints[[parent]]$Trans[[ff]] %*% mm
        skeleton$Joints[[nn]]$Dxyz[ff,1] = transM[1,4]
        skeleton$Joints[[nn]]$Dxyz[ff,2] = transM[2,4]
        skeleton$Joints[[nn]]$Dxyz[ff,3] = transM[3,4]
      }
    }
  }
  return (skeleton)
}



#' Generates direct kinematic model from hierarchical kinematic model.
#'
#' This function makes calculations based on hierarchical kinematic data in input list (input.skeleton). It does not use Dxyz from input.skeleton.
#'
#' @param input.skeleton list in the same format as one generated with read.mocap function.
#'
#' @return data frame with direct kinematic model. Names of the columns are the same as in input.skeleton.
#'
#' @examples
#' #an example BVH file
#' data("heian.nidan.bvh")
#' f <- file("heian.nidan.bvh")
#' writeChar(con = f, object = heian.nidan.bvh)
#' close(f)
#' #read hierarchical model stored in hierarchical BVH file
#' heian.nidan <- read.mocap("heian.nidan.bvh")
#' #plot kinematic data
#' plot(x = heian.nidan$data.frame$Hips.Dx, y = heian.nidan$data.frame$Hips.Dz, type = "l", ylab = "Displacement X [cm]", xlab = "Displacement Z [cm]")
#' title("Hips displacement during motion")
#'
#' #generate kinematic from hierarchical model - same results as above
#' df <- hierarchical.to.direct.kinematic(heian.nidan$skeleton)
#' plot(x = df$Hips.Dx, y = df$Hips.Dz, type = "l", ylab = "Displacement X [cm]", xlab = "Displacement Z [cm]")
#' title("Hips displacement during motion")
hierarchical.to.direct.kinematic <- function(input.skeleton)
{
  skeleton <- input.skeleton
  Nframes <- skeleton$Frames
  Nnodes <- length(skeleton$Joints)

  for (nn in 1:Nnodes)
  {
    if (skeleton$Joints[[nn]]$Nchannels == 6)#root node
    {
      #assume translational data is always ordered XYZ
      skeleton$Joints[[nn]]$Dxyz <- matrix(rep(0, Nframes * 3), nrow = Nframes, ncol = 3)
      for (b in 1:Nframes)
      {
        skeleton$Joints[[nn]]$Dxyz[b,1] <- skeleton$Joints[[nn]]$Offset[1] + skeleton$Joints[[nn]]$RawDxyz[b,1]
        skeleton$Joints[[nn]]$Dxyz[b,2] <- skeleton$Joints[[nn]]$Offset[2] + skeleton$Joints[[nn]]$RawDxyz[b,2]
        skeleton$Joints[[nn]]$Dxyz[b,3] <- skeleton$Joints[[nn]]$Offset[3] + skeleton$Joints[[nn]]$RawDxyz[b,3]
      }
      #Kinematics of the root element:
      skeleton$Joints[[nn]]$Trans <- list()
      for (b in 1:Nframes)
      {
        skeleton$Joints[[nn]]$Trans[[b]] = transformation_matrix(
          skeleton$Joints[[nn]]$Dxyz[b,],
          skeleton$Joints[[nn]]$Rxyz[b,],
          skeleton$Joints[[nn]]$Order
        )
      }
    }
    if (skeleton$Joints[[nn]]$Nchannels == 3)#joint node
    {
      skeleton$Joints[[nn]]$Dxyz = matrix(rep(0, Nframes * 3), nrow = Nframes, ncol = 3)


      skeleton$Joints[[nn]]$Trans <- list()
      for (a in 1:Nframes)
      {
        skeleton$Joints[[nn]]$Trans[[a]] = matrix(rep(0,16), nrow = 4, ncol = 4)
      }

    }
    if (skeleton$Joints[[nn]]$Nchannels == 0)#end node
    {
      skeleton$Joints[[nn]]$Dxyz <- matrix(rep(0, Nframes * 3), nrow = Nframes, ncol = 3)
    }

  }


  #skeleton$Joints[[1]]$Dxyz[1,]
  #skeleton$Joints[[1]]$Rxyz[1,]
  #skeleton$Joints[[1]]$Trans[[1]]

  #Calculate kinematics
  #No calculations are required for the root nodes.
  #For each joint, calculate the transformation matrix and for convenience
  #extract each position in a separate vector.
  for ( nn in 1:length(skeleton$Joints))
  {
    if (skeleton$Joints[[nn]]$Parent != -1 && skeleton$Joints[[nn]]$Nchannels != 0)
    {
      parent <- skeleton$Joints[[nn]]$Parent
      for (ff in 1:Nframes)
      {
        transM <- transformation_matrix(
          skeleton$Joints[[nn]]$Offset,
          skeleton$Joints[[nn]]$Rxyz[ff,],
          skeleton$Joints[[nn]]$Order)

        skeleton$Joints[[nn]]$Trans[[ff]] <-skeleton$Joints[[parent]]$Trans[[ff]] %*% transM# multiplyByMatrix(
        #skeleton$Joints[[parent]]$Trans[[ff]],
        #transM)

        skeleton$Joints[[nn]]$Dxyz[ff,1] <- skeleton$Joints[[nn]]$Trans[[ff]][1,4]
        skeleton$Joints[[nn]]$Dxyz[ff,2] <- skeleton$Joints[[nn]]$Trans[[ff]][2,4]
        skeleton$Joints[[nn]]$Dxyz[ff,3] <- skeleton$Joints[[nn]]$Trans[[ff]][3,4]
      }
    }
  }

  #For an end effector we don't have rotation data;
  #just need to calculate the final position.
  for (nn in 1:length(skeleton$Joints))
  {
    if (skeleton$Joints[[nn]]$Nchannels == 0)
    {
      mm <- matrix(c(1,0,0,skeleton$Joints[[nn]]$Offset[1],
                     0,1,0,skeleton$Joints[[nn]]$Offset[2],
                     0,0,1,skeleton$Joints[[nn]]$Offset[3],
                     0,0,0,1), nrow = 4, ncol = 4, byrow = TRUE)

      parent <- skeleton$Joints[[nn]]$Parent
      for (ff in 1:Nframes)
      {
        transM <- skeleton$Joints[[parent]]$Trans[[ff]] %*% mm
        skeleton$Joints[[nn]]$Dxyz[ff,1] = transM[1,4]
        skeleton$Joints[[nn]]$Dxyz[ff,2] = transM[2,4]
        skeleton$Joints[[nn]]$Dxyz[ff,3] = transM[3,4]
      }
    }
  }
  df <- bvh.to.df(skeleton, sd = FALSE)
  return(df)
}


#' Calculates second derivative
#'
#' Calculates second derivative using Central Difference Operator.
#' @param signal input vecotr.
#'
#' @return vector containing second derivative calculated from the input vector.
#'
#' @examples
#' second.derivative(sin(seq(from=0,to=2*pi,by=pi/50)))
second.derivative <- function(signal)
{
  if (length(signal) < 3)
  {
    return (NULL)
  }
  derivative <- rep(0, length(signal))
  for (a in 2:(length(signal)-1))
  {
    derivative[a] <- signal[a - 1] - 2 * signal[a] + signal[a + 1]
  }
  derivative[1] <- -2 * signal[a] + 2 * signal[a + 1]
  derivative[length(signal)] <- -2 * signal[length(signal)] + 2 * signal[length(signal) - 1]
  return (derivative)
}


#' This function get direct kinematic from list generated with function read.bvh or read.mocap function and returns it in the form of data frame.
#'
#' This function does not perform any algebraic calculation - it just takes data from Dxyz and Rxyz columns. Function can additionally calculate second derivative of Dxyz.
#'
#' @param skeleton input hierarchical kinematic model.
#' @param sd does function should calculate second derivative? Default = TRUE.
#'
#' @return data frame with direct kinematic.
#'
#' @examples
#' #an example BVH file
#' data("heian.nidan.bvh")
#' #write file to the disc
#' f <- file("heian.nidan.bvh")
#' writeChar(con = f, object = heian.nidan.bvh)
#' close(f)
#' #read hierarchical model stored in BVH file
#' heian.nidan <- read.bvh("heian.nidan.bvh")
#' df <- bvh.to.df(heian.nidan)
bvh.to.df <- function(skeleton, sd = TRUE)
{
  dataframe <- data.frame(Time = skeleton$Time)
  for (b in 1:length(skeleton$Joints))
  {
    #print(skeleton$Joints[[b]]$Name)
    Dx <- rep(0,skeleton$Frames)
    Dy <- rep(0,skeleton$Frames)
    Dz <- rep(0,skeleton$Frames)

    Rx <- rep(0,skeleton$Frames)
    Ry <- rep(0,skeleton$Frames)
    Rz <- rep(0,skeleton$Frames)
    for (a in 1:skeleton$Frames)
    {
      Dx[a] <- skeleton$Joints[[b]]$Dxyz[a,1]
      Dy[a] <- skeleton$Joints[[b]]$Dxyz[a,2]
      Dz[a] <- skeleton$Joints[[b]]$Dxyz[a,3]

      if (!is.null(skeleton$Joints[[b]]$Rxyz))
      {
        Rx[a] <- skeleton$Joints[[b]]$Rxyz[a,1]
        Ry[a] <- skeleton$Joints[[b]]$Rxyz[a,2]
        Rz[a] <- skeleton$Joints[[b]]$Rxyz[a,3]
      }
    }
    dataframe[paste(skeleton$Joints[[b]]$Name, ".Dx", sep = "")] <- Dx
    dataframe[paste(skeleton$Joints[[b]]$Name, ".Dy", sep = "")] <- Dy
    dataframe[paste(skeleton$Joints[[b]]$Name, ".Dz", sep = "")] <- Dz

    dataframe[paste(skeleton$Joints[[b]]$Name, ".Rx", sep = "")] <- Rx
    dataframe[paste(skeleton$Joints[[b]]$Name, ".Ry", sep = "")] <- Ry
    dataframe[paste(skeleton$Joints[[b]]$Name, ".Rz", sep = "")] <- Rz

    if (sd)
    {
      dataframe[paste(skeleton$Joints[[b]]$Name, ".ax", sep = "")] <- second.derivative(Dx)
      dataframe[paste(skeleton$Joints[[b]]$Name, ".ay", sep = "")] <- second.derivative(Dy)
      dataframe[paste(skeleton$Joints[[b]]$Name, ".az", sep = "")] <- second.derivative(Dz)
    }
  }
  return(dataframe)
}

#' Assign data frame to objcet of mocap class.
#'
#' This function does not recalculate chierarchical kinematic model in skeleton.
#'
#' @param skel object of mocap class.
#' @param df data frame with columns names compatible to hierarchical model definition in skel object.
#'
#' @return object of mocap class with df assigned.
#'
#' @examples
#' data("header.mocap")
#' data("heian.shodan")
#' print(header.mocap$skeleton$Frames)
#' original.bvh <- set.data.frame(header.mocap, heian.shodan)
#' print(original.bvh$skeleton$Frames)
set.data.frame <- function(skel, df)
{
  skel$skeleton$Frames <- nrow(df)
  skel$data.frame <- df
  return (skel)
}

#' This function reads motion capture file on BVH format.
#'
#' It also caluclates direct kinematic from hierarchical kinematic. This function calls read.bvh and bvh.to.df to generate single object of mocap class. See documentation for those two functions.
#'
#' @param filepath path to a file.
#'
#' @return object of mocap class.
#'
#' @examples
#' #an example BVH file
#' data("heian.nidan.bvh")
#' f <- file("eian.nidan.bvh")
#' writeChar(con = f, object = heian.nidan.bvh)
#' close(f)
#' #read hierarchical model stored in hierarchical BVH file
#' heian.nidan <- read.mocap("heian.nidan.bvh")
#' summary(heian.nidan)
read.mocap <- function(filepath)
{
  ll <- read.bvh(filepath)
  df <- bvh.to.df(ll)
  returndata <- list(skeleton = ll, data.frame = df)
  class(returndata) <- "mocap"
  return (returndata)
}

#Helper function for drawing spheres.
sphere.f <- function(x0 = 0, y0 = 0, z0 = 0, r = 1, n = 101, alpha = 1, ...){
  f <- function(s, t) cbind(r * cos(s) * cos(t) + x0,
                            r * sin(s) * cos(t) + y0,
                            r * sin(t) + z0)
  persp3d(f, slim = c(0, pi), tlim = c(0, 2*pi), n = n, add = T, alpha = alpha, ...)
}

#helper function for printing mocap data
print.frame <- function(obj, frame = 1, my.color = "green", alpha = 0.05, spheres = FALSE, df = NULL, print.text = FALSE)
{
  if (frame > nrow(obj$data.frame))
  {
    frame <- nrow(obj$data.frame)
  }
  if (!is.null(df))
  {
    if (frame > nrow(df))
    {
      frame <- nrow(df)
    }
  }
  for (a in 1:length(obj$skeleton$Joints))
  {
    parent <- obj$skeleton$Joints[[a]]$Parent

    if (obj$skeleton$Joints[[a]]$Parent != -1)
    {
      if (is.null(df))
      {
        x.a <- obj$data.frame[frame,paste(obj$skeleton$Joints[[a]]$Name, ".Dx", sep = "")]
        y.a <- obj$data.frame[frame,paste(obj$skeleton$Joints[[a]]$Name, ".Dy", sep = "")]
        z.a <- obj$data.frame[frame,paste(obj$skeleton$Joints[[a]]$Name, ".Dz", sep = "")]

        x.parent <- obj$data.frame[frame,paste(obj$skeleton$Joints[[parent]]$Name, ".Dx", sep = "")]
        y.parent <- obj$data.frame[frame,paste(obj$skeleton$Joints[[parent]]$Name, ".Dy", sep = "")]
        z.parent <- obj$data.frame[frame,paste(obj$skeleton$Joints[[parent]]$Name, ".Dz", sep = "")]

        #segments3d(x = c(x.a, x.parent),
        #        y = c(y.a, y.parent),
        #        z = c(z.a, z.parent),  color = my.color, lwd=3, alpha = 1)
      } else
      {
        x.a <- df[frame,paste(obj$skeleton$Joints[[a]]$Name, ".Dx", sep = "")]
        y.a <- df[frame,paste(obj$skeleton$Joints[[a]]$Name, ".Dy", sep = "")]
        z.a <- df[frame,paste(obj$skeleton$Joints[[a]]$Name, ".Dz", sep = "")]

        x.parent <- df[frame,paste(obj$skeleton$Joints[[parent]]$Name, ".Dx", sep = "")]
        y.parent <- df[frame,paste(obj$skeleton$Joints[[parent]]$Name, ".Dy", sep = "")]
        z.parent <- df[frame,paste(obj$skeleton$Joints[[parent]]$Name, ".Dz", sep = "")]
      }
      lines3d(x = c(x.a, x.parent),
              y = c(y.a, y.parent),
              z = c(z.a, z.parent),  color = my.color, alpha = alpha)

      #parent <- obj$skeleton$Joints[[a]]$Parent
      #lines3d(x = c(obj$skeleton$Joints[[a]]$Dxyz[frame,1],obj$skeleton$Joints[[parent]]$Dxyz[frame,1]),
      #        y = c(obj$skeleton$Joints[[a]]$Dxyz[frame,2],obj$skeleton$Joints[[parent]]$Dxyz[frame,2]),
      #        z = c(obj$skeleton$Joints[[a]]$Dxyz[frame,3],obj$skeleton$Joints[[parent]]$Dxyz[frame,3]),  color ="green")
      if (spheres)
      {
        sphere.f(x = x.a, y = y.a, z = z.a, r=5, n=31, radius = 1, color = my.color, alpha = alpha)
      }
      if (print.text)
      {
        rgl.texts(x = (x.a + 5), y = (y.a + 5), z = (z.a + 5), obj$skeleton$Joints[[a]]$Name)
      }
    }

  }
  if (spheres)
  {
    x.a <- obj$data.frame[frame,paste(obj$skeleton$Joints[[1]]$Name, ".Dx", sep = "")]
    y.a <- obj$data.frame[frame,paste(obj$skeleton$Joints[[1]]$Name, ".Dy", sep = "")]
    z.a <- obj$data.frame[frame,paste(obj$skeleton$Joints[[1]]$Name, ".Dz", sep = "")]

    sphere.f(x = x.a, y = y.a, z = z.a, r=5, n=31, radius = 1, color = my.color, alpha = alpha)
  }
  if (print.text)
  {
    x.a <- obj$data.frame[frame,paste(obj$skeleton$Joints[[1]]$Name, ".Dx", sep = "")]
    y.a <- obj$data.frame[frame,paste(obj$skeleton$Joints[[1]]$Name, ".Dy", sep = "")]
    z.a <- obj$data.frame[frame,paste(obj$skeleton$Joints[[1]]$Name, ".Dz", sep = "")]
    rgl.texts(x = (x.a + 5), y = (y.a + 5), z = (z.a + 5), obj$skeleton$Joints[[1]]$Name)
  }
}

#' Overrides plot function to work with mocap class.
#'
#' This function uses rgl package for 3D visualizations. It creates interactive 3D plots that enables rotation and scalling.
#'
#' @param obj mocap object.
#' @param frame index of frame to be show. If default (frame = 0) all frames are drawn.
#' @param my.color color of the plot.
#' @param frames.fraction if frame = 0 this parameter indicates what fraction of frames should be drawn (default frames.fraction = 0.1 means that 10\% of frames will be ploted).
#' @param alpha value of alpha channel of the plot (0 is 100\% transparency, 1 is no transparency, default is alpha = 0.05).
#' @param spheres if TRUE, position of body joints will be marked as spheres (default is spheres = FALSE).
#' @param append if FALSE (default is append = FALSE) a new plot is generated, if TRUE a plot is drawn over open rgl window).
#' @param print.text if TRUE (default is append = FALSE) plots name of the joints.
#'
#' @examples
#' data("right.arm.motion.1")
#' plot(right.arm.motion.1, frame = 1, my.color = "white", alpha = 1, spheres = TRUE)
#' plot(right.arm.motion.1, frames.fraction = 0.5, my.color = "white", alpha = 1, spheres = FALSE, print.text = TRUE)
plot.mocap <- function(obj, frame = 0, my.color = "green", frames.fraction = 0.1, alpha = 0.05, spheres = FALSE, append = FALSE, print.text = FALSE){
  library(rgl)
  if (!append)
  {
    rgl.open() # Open a new RGL device
  }
  if (frame > 0)
  {
    print.frame(obj, frame, my.color = my.color, alpha = alpha, spheres = spheres, print.text = print.text)
  }
  else {

    step <- ceiling(1.0 / frames.fraction)
    ind <- seq(from = 1, to = obj$skeleton$Frames, by = step)

    #for (a in 1:obj$skeleton$Frames)
    for (a in ind)
      print.frame(obj, a, my.color = my.color, alpha = alpha, spheres = spheres, print.text = print.text)
  }
  axes3d(col="white", alpha = 1)
}

########################
########################
#helper function
correctMe3D <- function(startC, stopC, signal1, signal2, signal3, all)
{
  diff2 <- all[stopC, signal1] - all[startC, signal1]
  all[stopC:length(all$Hips.Dy),dz_vector] = all[stopC:length(all$Hips.Dy),dz_vector] - diff2

  diff2 <- all[stopC, signal2] - all[startC, signal2]
  all[stopC:length(all$Hips.Dy),dx_vector] = all[stopC:length(all$Hips.Dy),dx_vector] - diff2


  diff2 <- all[stopC, signal3] - all[startC, signal3]
  all[stopC:length(all$Hips.Dy),dy_vector] = all[stopC:length(all$Hips.Dy),dy_vector] - diff2

  return (all)
}

#' This function corrects translation of the direct kinematic model.
#'
#' This heuristic method is especially usable while dealing with data acquired by IMU (Inertial measurement unit) sensors. There are two
#' possible calls of this method. The first one utilize acceleration data of body joints (.ax, .ay and .az columns). If this data is avilable
#' method can calculate displacement of long motion, during which an actor uses both left and right leg. If this call is used,
#' one should left bodypartname as NULL. The second call is used when we are dealing with data without acceleration data and actor has
#' one stationary limb. In this case bodypartname shoule have value of the limb which does not translate during motion.
#'
#'
#' @param dd data frame with motion capture data.
#' @param LeftFoot name of the column with data that holds coordinates of left foot (joint that touch the ground). Default value is LeftFoot = "LeftFoot". Used only for the first type of call.
#' @param RightFoot name of the column with data that holds coordinates of right foot (joint that touch the ground). Default value is RightFoot = "RightFoot". Used only for the first type of call.
#' @param show.plot if TRUE (default is show.plot = FALSE) plots results of an algorithm.
#' @param plot.title part of the title over plots.
#' @param bodypartname if not NULL value the stationary joint of the motion is column with bodypartname. Default value is bodypartname = NULL.
#' @param dyEps Treshold value for translation calculation (used only for the first type of call). Default value is dyEps = 5.
#'
#' @return Data frame, which has Dxzy columns updated. All other columns are not updated.
#'
#' @examples
#'   #This example uses acceleration data to calculates correct displacement.
#'   #header of mocap file
#'   data("header.mocap")
#'   #data frame with displacements and acceleration data
#'   data("heian.shodan")
#'
#'   heian.shodan.corrected <- calculate.kinematic(heian.shodan, show.plot = "TRUE", plot.title = "Heian Shodan")
#'   original.bvh <- set.data.frame(header.mocap, heian.shodan)
#'   corrected.bvh <- set.data.frame(header.mocap, heian.shodan.corrected)
#'   #plot BVH, red is original data, green is corrected
#'   plot(original.bvh, frames.fraction = 0.1, my.color = "red", alpha = 0.1, spheres = FALSE)
#'   plot(corrected.bvh, frames.fraction = 0.1, my.color = "green", alpha = 0.1, spheres = FALSE, append = TRUE)
#'
#'   #writting BVH to disk
#'   write.bvh(original.bvh, "original.bvh")
#'   write.bvh(corrected.bvh, "corrected.bvh")
#'
#'   #This example uses single body joint which coordinates should be constant during motion
#'   data(mawashi.geri.left.1)
#'   plot(mawashi.geri.left.1, frames.fraction = 0.1, my.color = "red", alpha = 0.5, spheres = FALSE)
#'   mawashi.geri.left.1$data.frame <- calculate.kinematic(mawashi.geri.left.1$data.frame, bodypartname = "RightFoot")
#'   plot(mawashi.geri.left.1, frames.fraction = 0.1, my.color = "green", alpha = 0.5, spheres = FALSE, append = TRUE)

calculate.kinematic <- function(dd, LeftFoot = "LeftFoot", RightFoot = "RightFoot", show.plot = FALSE, plot.title = "", bodypartname = NULL, dyEps = 5)
{
  dx_vector <<- names(dd)[grepl(".Dx",names(dd))]
  dy_vector <<- names(dd)[grepl(".Dy",names(dd))]
  dz_vector <<- names(dd)[grepl(".Dz",names(dd))]
  if (!is.null(bodypartname))
  {
    window_size <- 0.05
    library(smoother)

    for (a in 1:(length(dd$RightFoot.Dx) - 1))
    {

      dd <- correctMe3D(a, a + 1,
                        paste(bodypartname, ".Dz", sep = ""),
                        paste(bodypartname, ".Dx", sep = ""),
                        paste(bodypartname, ".Dy", sep = ""), dd)
    }

    rm(dx_vector, envir = .GlobalEnv)
    rm(dy_vector, envir = .GlobalEnv)
    rm(dz_vector, envir = .GlobalEnv)

    return (dd)
  }

  window_size <- 100 / length(dd[,paste(RightFoot,".ax", sep ="")])

  library(smoother)

  zzR <- smth(sqrt(dd[,paste(RightFoot,".ax", sep ="")] ^ 2 + dd[,paste(RightFoot,".ay", sep ="")] ^ 2 + dd[,paste(RightFoot,".az", sep ="")] ^ 2),window = window_size,method = "gaussian") #SMOOTHING
  zzL <- smth(sqrt(dd[,paste(LeftFoot,".ax", sep ="")] ^ 2 + dd[,paste(LeftFoot,".ay", sep ="")] ^ 2 + dd[,paste(LeftFoot,".az", sep ="")] ^ 2),window = window_size,method = "gaussian") #SMOOTHING

  #zzR <- smth(sqrt(dd$RightFoot.ax ^ 2 + dd$RightFoot.ay ^ 2 + dd$RightFoot.az ^ 2),window = window_size,method = "gaussian") #SMOOTHING
  #zzL <- smth(sqrt(dd$LeftFoot.ax ^ 2 + dd$LeftFoot.ay ^ 2 + dd$LeftFoot.az ^ 2),window = window_size,method = "gaussian") #SMOOTHING

  if (show.plot)
  {
    plot(zzR, type = 'n', xlab = "Time [ms]", ylab="Acceleration magnitude [g]")
    title(paste("Acceleration magnitude plot of", plot.title))
    lines(zzR, col="red")
    lines(zzL, col="blue")
    legend("topright", legend=c("Right foot", "Left foot"), col=c("red", "blue"), lty=c(1,1))
  }

  zzR[is.na(zzR)] <- 0
  zzL[is.na(zzL)] <- 0

  for (a in 1:(length(dd[,paste(RightFoot,".ax", sep ="")]) - 1))
  {
    if (zzR[a] > zzL[a] && zzR[a + 1] > zzL[a + 1])
    {
      dd <- correctMe3D(a, a + 1, paste(LeftFoot,".Dz", sep =""), paste(LeftFoot,".Dx", sep =""), paste(LeftFoot,".Dy", sep =""), dd)
    }
    else if (zzR[a] < zzL[a] && zzR[a + 1] < zzL[a + 1])
    {
      dd <- correctMe3D(a, a + 1, paste(RightFoot,".Dz", sep =""), paste(RightFoot,".Dx", sep =""), paste(RightFoot,".Dy", sep =""), dd)
    }
  }

  zzR <- smth(dd[,paste(RightFoot,".Dy", sep ="")],window = window_size,method = "gaussian") #SMOOTHING
  if (show.plot)
  {
    plot(zzR, type = 'n', xlab = "Time [ms]", ylab="Y [cm]")
    title(paste("Vertical coordinates of feet of", plot.title))
    lines(zzR, col="red")
  }
  zzL <- smth(dd[,paste(LeftFoot,".Dy", sep ="")],window = window_size,method = "gaussian") #SMOOTHING
  if (show.plot)
  {
    lines(zzL, col="blue")
    legend("topright", legend=c("Right foot", "Left foot"), col=c("red", "blue"), lty=c(1,1))
  }

  zzR[is.na(zzR)] <- 0
  zzL[is.na(zzL)] <- 0

  for (a in 1:(length(dd[,paste(RightFoot,".ax", sep ="")]) - 1))
  {
    if (abs(zzR[a] - zzL[a]) > dyEps)
    {
      if (zzR[a] > zzL[a] && zzR[a + 1] > zzL[a + 1])
      {
        dd <- correctMe3D(a, a + 1, paste(LeftFoot, ".Dz", sep=""), paste(LeftFoot, ".Dx", sep =""), paste(LeftFoot, ".Dy", sep=""), dd)
      }
      else if (zzR[a] < zzL[a] && zzR[a + 1] < zzL[a + 1])
      {
        dd <- correctMe3D(a, a + 1, paste(RightFoot,".Dz", sep=""), paste(RightFoot,".Dx", sep=""), paste(RightFoot,".Dy",sep =""), dd)
      }
    }
  }

  groundPosition <- dd$RightFoot.Dy[1]
  for (a in 1:(length(dd$RightFoot.ax)))
  {
    if (zzR[a] >= zzL[a])
    {
      #przesu? ca?? sylwetk?, aby Y stopy dotyka?o ziemi
      dd <- MoveToTheGround(dd, a, groundPosition, paste(LeftFoot,".Dy",sep=""))
    }
    else if (zzR[a] < zzL[a])
    {
      dd <- MoveToTheGround(dd, a, groundPosition, paste(RightFoot,".Dy",sep=""))
    }
  }


  zzR <- smth(dd[,paste(RightFoot,".Dy", sep ="")],window = window_size,method = "gaussian") #SMOOTHING
  zzL <- smth(dd[,paste(LeftFoot,".Dy", sep ="")],window = window_size,method = "gaussian") #SMOOTHING
  if (show.plot)
  {
    plot(zzL, type = 'n', xlab = "Time [ms]", ylab="Y [cm]")
    title(paste("Vertical coordinates of feet after positioning\r\nwith the ground of", plot.title))
    lines(zzR, col="red")
    zzL <- smth(dd[,paste(LeftFoot,".Dy", sep ="")],window = window_size,method = "gaussian") #SMOOTHING
    lines(zzL, col="blue")
  }
  rm(dx_vector, envir = .GlobalEnv)
  rm(dy_vector, envir = .GlobalEnv)
  rm(dz_vector, envir = .GlobalEnv)

  return (dd)
}

#helper function
MoveToTheGround <- function (all, a, groundPosition,  signal1)
{
  diff2 <- all[a, signal1] - groundPosition
  all[a,dy_vector] = all[a,dy_vector] - diff2
  return (all)
}

#helper function
maketabs <- function(level)
{
  if (level == 1)
  {
    return ("")
  } else
  {
    return (paste(rep("\t",level -1), collapse=""))
  }
}

#' This function saves object of mocap class to the file.
#'
#' Function uses hierarchical model definition from hierarchy list however data of channels are taken from data frame.
#'
#' @param skeleton.helper mocap object to be save.
#' @param path path to the file.
#'
#' @examples
#'   data("header.mocap")
#'   data("heian.shodan")
#'   heian.shodan.corrected <- calculate.kinematic(heian.shodan, show.plot = "TRUE", plot.title = "Heian Shodan")
#'   original.bvh <- set.data.frame(header.mocap, heian.shodan)
#'   corrected.bvh <- set.data.frame(header.mocap, heian.shodan.corrected)
#'   #plotting BVH
#'   plot(original.bvh, frames.fraction = 0.1, my.color = "red", alpha = 0.1, spheres = FALSE)
#'   plot(corrected.bvh, frames.fraction = 0.1, my.color = "green", alpha = 0.1, spheres = FALSE, append = TRUE)
#'   #writting BVH to disk
#'   write.bvh(original.bvh, "original.bvh")
#'   write.bvh(corrected.bvh, "corrected.bvh")
write.bvh <- function(skeleton.helper, path)
{
  fileConn<-file(path,"w")
  writeLines(c("HIERARCHY"), con=fileConn)
  jointLevelName <- "ROOT"
  tabs <- ""
  prev.Nestdepth <- 1
  for (a in 1:length(skeleton.helper$skeleton$Joints))
  {
    if (a == 1)
    {
      jointLevelName <- "ROOT"
    } else
    {
      jointLevelName <- "JOINT"
    }
    jj <- skeleton.helper$skeleton$Joints[[a]]
    if (jj$Nchannels != 0)
      jointLevelName <- paste(jointLevelName, jj$Name)
    else
      jointLevelName <- "End Site"
    tabs <- maketabs(jj$Nestdepth)
    Nest.diff <- jj$Nestdepth - prev.Nestdepth
    if (Nest.diff == 0)
    {
      writeLines(jointLevelName, con=fileConn)
      writeLines("{", con=fileConn)
    } else if (Nest.diff > 0)
    {
      tabs <- maketabs(jj$Nestdepth)
      writeLines(paste(tabs, jointLevelName, sep = ""), con=fileConn)
      writeLines(paste(tabs,"{",sep = ""), con=fileConn)
    }
    else if (Nest.diff < 0)
    {
      for (a in prev.Nestdepth:jj$Nestdepth)
      {
        tabs <- maketabs(a)
        writeLines(paste(tabs,"}",sep = ""), con=fileConn)
      }
      writeLines(paste(tabs, jointLevelName,sep = ""), con=fileConn)
      writeLines(paste(tabs,"{",sep = ""), con=fileConn)
    }
    tabs <- maketabs(jj$Nestdepth + 1)

    writeLines(paste(tabs, "OFFSET ", paste(jj$Offset, collapse=" "), sep = ""), con=fileConn)
    if (jj$Nchannels > 0)
    {
      if (jj$Nchannels == 3)
      {
        line <- "CHANNELS 3"
        line <- paste(tabs, line, " ", intToUtf8(jj$Order[1] + 87), "rotation", sep = "")
        line <- paste(line, " ", intToUtf8(jj$Order[2] + 87), "rotation", sep = "")
        line <- paste(line, " ", intToUtf8(jj$Order[3] + 87), "rotation", sep = "")
      }
      if (jj$Nchannels == 6)
      {
        line <- paste(tabs, "CHANNELS 6 ", "Xposition Yposition Zposition", sep = "")
        line <- paste(line, " ", intToUtf8(jj$Order[1] + 87), "rotation", sep = "")
        line <- paste(line, " ", intToUtf8(jj$Order[2] + 87), "rotation", sep = "")
        line <- paste(line, " ", intToUtf8(jj$Order[3] + 87), "rotation", sep = "")
      }
      writeLines(line, con=fileConn)
    }
    prev.Nestdepth <- jj$Nestdepth
  }

  for (a in jj$Nestdepth:1)
  {
    tabs <- maketabs(a)
    writeLines(paste(tabs,"}",sep = ""), con=fileConn)
  }
  writeLines("MOTION", con=fileConn)
  writeLines(paste("Frames:", nrow(skeleton.helper$data.frame)), con=fileConn)
  writeLines(paste("Frame Time:", skeleton.helper$skeleton$FrameTime), con=fileConn)
  writeLines("", con=fileConn)

  columns.helper <- c()
  for (a in 1:length(skeleton.helper$skeleton$Joints))
  {
    jj <- skeleton.helper$skeleton$Joints[[a]]


    #odj?? OFFSET od Dx, Dy, Dz
    if (jj$Nchannels == 6)
    {
      columns.helper <- c(columns.helper, paste(jj$Name, ".Dx", sep = ""))
      columns.helper <- c(columns.helper, paste(jj$Name, ".Dy", sep = ""))
      columns.helper <- c(columns.helper, paste(jj$Name, ".Dz", sep = ""))

      columns.helper <- c(columns.helper, paste(jj$Name, ".R", intToUtf8(jj$Order[1] + 119), sep = ""))
      columns.helper <- c(columns.helper, paste(jj$Name, ".R", intToUtf8(jj$Order[2] + 119), sep = ""))
      columns.helper <- c(columns.helper, paste(jj$Name, ".R", intToUtf8(jj$Order[3] + 119), sep = ""))
    }
    if (jj$Nchannels == 3)
    {
      columns.helper <- c(columns.helper, paste(jj$Name, ".R", intToUtf8(jj$Order[1] + 119), sep = ""))
      columns.helper <- c(columns.helper, paste(jj$Name, ".R", intToUtf8(jj$Order[2] + 119), sep = ""))
      columns.helper <- c(columns.helper, paste(jj$Name, ".R", intToUtf8(jj$Order[3] + 119), sep = ""))
    }
  }

  idx <- match(columns.helper, names(skeleton.helper$data.frame))
  mydf <- skeleton.helper$data.frame[,idx]
  for (a in 1:nrow(mydf))
  {
    writeLines(paste(as.vector(mydf[a,]), collapse=" "), con=fileConn)
  }
  close(fileConn)
}


#' This function returns n by n rotation matrix that align vector x onto y.
#'
#' Bth vector x and y has to be normalized and has to be same length.
#'
#' @param x vector to be rotated (has to be normalized).
#' @param y reference vector (has to be normalized).
#'
#' @return n x n rotation matrix.
#'
#' @examples
#' x <- vector.to.unit(c(1,0,0))
#' y <- vector.to.unit(c(0,1,0))
#' Rx2y <- rotation.matrix.between.vectors(x, y)
#' x %*% Rx2y
#' y
#' x <- vector.to.unit(c(5,0,2,4,6))
#' y <- vector.to.unit(c(0,1,0,8,-5))
#' Rx2y <- rotation.matrix.between.vectors(x, y)
#' x %*% Rx2y
#' y
#' x <- vector.to.unit(c(8,0,0))
#' y <- vector.to.unit(c(2,0,0))
#' Rx2y <- rotation.matrix.between.vectors(x, y)
#' x %*% Rx2y
#' y
rotation.matrix.between.vectors <- function(x,y){
  a=x/sqrt(sum(x^2))

  b=y-sum(a*y)*a
  b=b/sqrt(sum(b^2))

  cost=sum(x*y)/sqrt(sum(x^2))/sqrt(sum(y^2))

  sint=sqrt(1-cost^2);

  r.mat<- diag(length(x)) - a %*% t(a) - b %*% t(b) +
    cbind(a,b) %*% matrix(c(cost,-sint,sint,cost), 2) %*% t(cbind(a,b))
  if (anyNA(r.mat))
  {
    r.mat <- diag(length(x))
  }
 return(r.mat)
}




#' This function generates object of mocap class with zero rotation data.
#'
#' @param input.skeleton object of mocap class which defines hierarchical model.
#' @param Nframes number of frames to be generated.
#' @param FrameTime intervals between acquisitions.
#'
#' @return object of mocap class.
#'
#' @examples
#' data("heian.nidan.bvh")
#' f <- file("heian.nidan.bvh")
#' writeChar(con = f, object = heian.nidan.bvh)
#' close(f)
#' #read hierarchical model stored in hierarchical BVH file
#' heian.nidan <- read.mocap("heian.nidan.bvh")
#' first.frame <- generate.first.frame(heian.nidan)
#' plot(heian.nidan, frame = 1, alpha = 1, spheres = TRUE)
#' plot(first.frame, my.color = "red", frame = 1, alpha = 1, spheres = TRUE, append = TRUE)
generate.first.frame <- function(input.skeleton, Nframes = 1, FrameTime = 0.01)
{
  #Nframes = nrow(df.to.save)
  #FrameTime = 0.01

  skeleton <- input.skeleton$skeleton
  #skeleton$Time <- seq(from=0,
  #      to=(FrameTime * Nframes + (FrameTime / 2)),
  #      by=FrameTime)
  skeleton$Time <- rep(0, Nframes)

  if (Nframes > 1)
  {
    for (a in 2:Nframes)
    {
      skeleton$Time[a] <- skeleton$Time[a -1] + FrameTime
    }
  }
  Nnodes <- length(skeleton$Joints)
  #print(Nnodes)
  ###############
  channel_count <- 0
  Nframes <- Nframes
  for (nn in 1:Nnodes)
  {
    if (skeleton$Joints[[nn]]$Nchannels == 6)#root node
    {
      #assume translational data is always ordered XYZ
      skeleton$Joints[[nn]]$Dxyz <- matrix(rep(0, Nframes * 3), nrow = Nframes, ncol = 3)
	  skeleton$Joints[[nn]]$RawDxyz <- matrix(rep(0, Nframes * 3), nrow = Nframes, ncol = 3)
      for (b in 1:Nframes)
      {
        skeleton$Joints[[nn]]$Dxyz[b,1] <- skeleton$Joints[[nn]]$Offset[1] + 0
        skeleton$Joints[[nn]]$Dxyz[b,2] <- skeleton$Joints[[nn]]$Offset[2] + 0
        skeleton$Joints[[nn]]$Dxyz[b,3] <- skeleton$Joints[[nn]]$Offset[3] + 0
      }
      skeleton$Joints[[nn]]$Rxyz = matrix(rep(0, Nframes * 3), nrow = Nframes, ncol = 3)
      for (b in 1:Nframes)
      {
        skeleton$Joints[[nn]]$Rxyz[b,skeleton$Joints[[nn]]$Order[1]] <- 0
        skeleton$Joints[[nn]]$Rxyz[b,skeleton$Joints[[nn]]$Order[2]] <- 0
        skeleton$Joints[[nn]]$Rxyz[b,skeleton$Joints[[nn]]$Order[3]] <- 0
      }
      #Kinematics of the root element:
      skeleton$Joints[[nn]]$Trans <- list()
      for (b in 1:Nframes)
      {
        skeleton$Joints[[nn]]$Trans[[b]] = transformation_matrix(
          skeleton$Joints[[nn]]$Dxyz[b,],
          skeleton$Joints[[nn]]$Rxyz[b,],
          skeleton$Joints[[nn]]$Order
        )
      }
    }
    if (skeleton$Joints[[nn]]$Nchannels == 3)#joint node
    {
      skeleton$Joints[[nn]]$Rxyz = matrix(rep(0, Nframes * 3), nrow = Nframes, ncol = 3)
      for (b in 1:Nframes)
      {
        skeleton$Joints[[nn]]$Rxyz[b,skeleton$Joints[[nn]]$Order[1]] <- 0
        skeleton$Joints[[nn]]$Rxyz[b,skeleton$Joints[[nn]]$Order[2]] <- 0
        skeleton$Joints[[nn]]$Rxyz[b,skeleton$Joints[[nn]]$Order[3]] <- 0
      }
      skeleton$Joints[[nn]]$Dxyz = matrix(rep(0, Nframes * 3), nrow = Nframes, ncol = 3)


      skeleton$Joints[[nn]]$Trans <- list()
      for (a in 1:Nframes)
      {
        skeleton$Joints[[nn]]$Trans[[a]] = matrix(rep(0,16), nrow = 4, ncol = 4)
      }

    }
    if (skeleton$Joints[[nn]]$Nchannels == 0)#end node
    {
      skeleton$Joints[[nn]]$Dxyz <- matrix(rep(0, Nframes * 3), nrow = Nframes, ncol = 3)
    }
    channel_count <- channel_count + skeleton$Joints[[nn]]$Nchannels
  }

  #skeleton$Joints[[1]]$Dxyz[1,]
  #skeleton$Joints[[1]]$Rxyz[1,]
  #skeleton$Joints[[1]]$Trans[[1]]

  #Calculate kinematics
  #No calculations are required for the root nodes.
  #For each joint, calculate the transformation matrix and for convenience
  #extract each position in a separate vector.
  for ( nn in 1:length(skeleton$Joints))
  {
    if (skeleton$Joints[[nn]]$Parent != -1 && skeleton$Joints[[nn]]$Nchannels != 0)
    {
      parent <- skeleton$Joints[[nn]]$Parent
      for (ff in 1:Nframes)
      {
        transM <- transformation_matrix(
          skeleton$Joints[[nn]]$Offset,
          skeleton$Joints[[nn]]$Rxyz[ff,],
          skeleton$Joints[[nn]]$Order)

        skeleton$Joints[[nn]]$Trans[[ff]] <-skeleton$Joints[[parent]]$Trans[[ff]] %*% transM# multiplyByMatrix(
        #skeleton$Joints[[parent]]$Trans[[ff]],
        #transM)

        skeleton$Joints[[nn]]$Dxyz[ff,1] <- skeleton$Joints[[nn]]$Trans[[ff]][1,4]
        skeleton$Joints[[nn]]$Dxyz[ff,2] <- skeleton$Joints[[nn]]$Trans[[ff]][2,4]
        skeleton$Joints[[nn]]$Dxyz[ff,3] <- skeleton$Joints[[nn]]$Trans[[ff]][3,4]
      }
    }
  }

  #For an end effector we don't have rotation data;
  #just need to calculate the final position.
  for (nn in 1:length(skeleton$Joints))
  {
    if (skeleton$Joints[[nn]]$Nchannels == 0)
    {
      mm <- matrix(c(1,0,0,skeleton$Joints[[nn]]$Offset[1],
                     0,1,0,skeleton$Joints[[nn]]$Offset[2],
                     0,0,1,skeleton$Joints[[nn]]$Offset[3],
                     0,0,0,1), nrow = 4, ncol = 4, byrow = TRUE)

      parent <- skeleton$Joints[[nn]]$Parent
      for (ff in 1:Nframes)
      {
        transM <- skeleton$Joints[[parent]]$Trans[[ff]] %*% mm
        skeleton$Joints[[nn]]$Dxyz[ff,1] = transM[1,4]
        skeleton$Joints[[nn]]$Dxyz[ff,2] = transM[2,4]
        skeleton$Joints[[nn]]$Dxyz[ff,3] = transM[3,4]
      }
    }
  }


  skeleton$Frames <- Nframes
  df <- bvh.to.df(skeleton)
  returndata <- list(skeleton = skeleton, data.frame = df)
  class(returndata) <- "mocap"

  return (returndata)
}#generate.first.frame

#helper function
generate.single.frame <- function(input.skeleton, index =1)
{
  skeleton <- input.skeleton$skeleton
  Nnodes <- length(skeleton$Joints)
  ###############
  channel_count <- 0
  Nframes <- 1
  for (nn in 1:Nnodes)
  {
    if (skeleton$Joints[[nn]]$Nchannels == 6)#root node
    {
      #assume translational data is always ordered XYZ
      #skeleton$Joints[[nn]]$Dxyz <- matrix(rep(0, Nframes * 3), nrow = Nframes, ncol = 3)
      for (b in 1:Nframes)
      {
        skeleton$Joints[[nn]]$Dxyz[index,1] <- skeleton$Joints[[nn]]$Offset[1] + skeleton$Joints[[nn]]$RawDxyz[index,1]
        skeleton$Joints[[nn]]$Dxyz[index,2] <- skeleton$Joints[[nn]]$Offset[2] + skeleton$Joints[[nn]]$RawDxyz[index,2]
        skeleton$Joints[[nn]]$Dxyz[index,3] <- skeleton$Joints[[nn]]$Offset[3] + skeleton$Joints[[nn]]$RawDxyz[index,3]
      }

      #Kinematics of the root element:
      #skeleton$Joints[[nn]]$Trans <- list()
      for (b in 1:Nframes)
      {
        skeleton$Joints[[nn]]$Trans[[index]] = transformation_matrix(
          skeleton$Joints[[nn]]$Dxyz[index,],
          skeleton$Joints[[nn]]$Rxyz[index,],
          skeleton$Joints[[nn]]$Order
        )
      }
    }
    if (skeleton$Joints[[nn]]$Nchannels == 3)#joint node
    {
      #skeleton$Joints[[nn]]$Dxyz = matrix(rep(0, Nframes * 3), nrow = Nframes, ncol = 3)


      #skeleton$Joints[[nn]]$Trans <- list()
      #for (a in 1:Nframes)
      #{
      #  skeleton$Joints[[nn]]$Trans[[a]] = matrix(rep(0,16), nrow = 4, ncol = 4)
      #}

    }
    #if (skeleton$Joints[[nn]]$Nchannels == 0)#end node
    #{
    #  skeleton$Joints[[nn]]$Dxyz <- matrix(rep(0, Nframes * 3), nrow = Nframes, ncol = 3)
    #}
    channel_count <- channel_count + skeleton$Joints[[nn]]$Nchannels
  }

  #skeleton$Joints[[1]]$Dxyz[1,]
  #skeleton$Joints[[1]]$Rxyz[1,]
  #skeleton$Joints[[1]]$Trans[[1]]

  #Calculate kinematics
  #No calculations are required for the root nodes.
  #For each joint, calculate the transformation matrix and for convenience
  #extract each position in a separate vector.
  for ( nn in 1:length(skeleton$Joints))
  {
    if (skeleton$Joints[[nn]]$Parent != -1 && skeleton$Joints[[nn]]$Nchannels != 0)
    {
      parent <- skeleton$Joints[[nn]]$Parent
      for (ff in 1:Nframes)
      {
        transM <- transformation_matrix(
          skeleton$Joints[[nn]]$Offset,
          skeleton$Joints[[nn]]$Rxyz[index,],
          skeleton$Joints[[nn]]$Order)

        skeleton$Joints[[nn]]$Trans[[index]] <-skeleton$Joints[[parent]]$Trans[[index]] %*% transM# multiplyByMatrix(
        #skeleton$Joints[[parent]]$Trans[[ff]],
        #transM)

        skeleton$Joints[[nn]]$Dxyz[index,1] <- skeleton$Joints[[nn]]$Trans[[index]][1,4]
        skeleton$Joints[[nn]]$Dxyz[index,2] <- skeleton$Joints[[nn]]$Trans[[index]][2,4]
        skeleton$Joints[[nn]]$Dxyz[index,3] <- skeleton$Joints[[nn]]$Trans[[index]][3,4]
      }
    }
  }

  #For an end effector we don't have rotation data;
  #just need to calculate the final position.
  for (nn in 1:length(skeleton$Joints))
  {
    if (skeleton$Joints[[nn]]$Nchannels == 0)
    {
      mm <- matrix(c(1,0,0,skeleton$Joints[[nn]]$Offset[1],
                     0,1,0,skeleton$Joints[[nn]]$Offset[2],
                     0,0,1,skeleton$Joints[[nn]]$Offset[3],
                     0,0,0,1), nrow = 4, ncol = 4, byrow = TRUE)

      parent <- skeleton$Joints[[nn]]$Parent
      for (ff in 1:Nframes)
      {
        transM <- skeleton$Joints[[parent]]$Trans[[index]] %*% mm
        skeleton$Joints[[nn]]$Dxyz[index,1] = transM[1,4]
        skeleton$Joints[[nn]]$Dxyz[index,2] = transM[2,4]
        skeleton$Joints[[nn]]$Dxyz[index,3] = transM[3,4]
      }
    }
  }


  #skeleton$Frames <- 1
  df <- bvh.to.df(skeleton)
  returndata <- list(skeleton = skeleton, data.frame = df)
  class(returndata) <- "mocap"

  return (returndata)
}#generate.single.frame


#' Calculates quaternion from axis angle.
#'
#' All other quternion function are impoted from package RSpincalc.
#' @param axis 3D vector.
#' @param angle in radians.
#'
#' @return 4D quaternon vector.
#'
#' @examples
#' myEV2Q(vector.to.unit(c(1,2,3)),pi/4)
myEV2Q <- function(axis, angle)
{
  qx = axis[1] * sin(angle/2)
  qy = axis[2] * sin(angle/2)
  qz = axis[3] * sin(angle/2)
  qw = cos(angle/2)
  Q = c(qw,qx,qy,qz)
  Q
}

##################

#' This function genereates list of vectors.
#'
#' Each list position equals c(ABC.Dx, ABC.Dy, ABC.Dz) where featuresName = ABC is a name of the column of data frame dataToCalculate.
#'
#'
#' @param dataToCalculate data frame with motion capture data.
#' @param featuresName column name of the motion capture feature. Column names should have names endings .Dx, .Dy, .Dz.
#'
#' @return list of vectors defined as above.
#'
#' @examples
#' data("heian.yondan")
#' vector.to.list(heian.yondan[1:3,], "RightHand")
vector.to.list <- function(dataToCalculate, featuresName)
{
  listhelper <- list()
  for (b in 1:length(dataToCalculate[,paste(featuresName,".Dx",sep = "")]))
    listhelper[[b]] <- c(dataToCalculate[b,paste(featuresName,".Dx",sep = "")],
                         dataToCalculate[b,paste(featuresName,".Dy",sep = "")],
                         dataToCalculate[b,paste(featuresName,".Dz",sep = "")])
  return(listhelper)
}



#' This function generates list of angles.
#'
#' Angles are calculated on the plane between vectors v1=(f2-f1) and v2=(f2-f3).
#'
#' @param dataToCalculate data frame with motion capture data.
#' @param f1 name of the first joint.
#' @param f2 name of the second joint.
#' @param f3 name of the third joint.
#'
#' @return list of angles (in radians) defined as above.
#'
#' @examples
#' data("heian.yondan")
#' vector.to.angles.list(heian.yondan[1:3,], "RightShoulder", "RightArm", "RightForearm")
vector.to.angles.list <- function(dataToCalculate, f1, f2, f3)
{
  listhelper <- list()
  for (b in 1:length(dataToCalculate[,paste(f1,".Dx",sep = "")]))
    listhelper[[b]] <- vector.angle(c(dataToCalculate[b,paste(f2,".Dx",sep = "")] - dataToCalculate[b,paste(f1,".Dx",sep = "")],
                                       dataToCalculate[b,paste(f2,".Dy",sep = "")] - dataToCalculate[b,paste(f1,".Dy",sep = "")],
                                       dataToCalculate[b,paste(f2,".Dz",sep = "")] - dataToCalculate[b,paste(f1,".Dz",sep = "")]),
                                     c(dataToCalculate[b,paste(f2,".Dx",sep = "")] - dataToCalculate[b,paste(f3,".Dx",sep = "")],
                                       dataToCalculate[b,paste(f2,".Dy",sep = "")] - dataToCalculate[b,paste(f3,".Dy",sep = "")],
                                       dataToCalculate[b,paste(f2,".Dz",sep = "")] - dataToCalculate[b,paste(f3,".Dz",sep = "")]))
  return(listhelper)
}

#helper function
generate.frame <- function(xt)
{
  xxt <- xt / vector.norm(xt)
  yt <- c(0,1,0)
  zt <- vector.cross(xxt, yt)
  zzt <-  zt / vector.norm(zt)
  yyt <- vector.cross(xt, zzt)
  yyt <- yyt / vector.norm(yyt)
  return(list(x = xxt, y = yyt, z = zzt))
}

#' This function genereates list of angles between vector and coordinates frame.
#'
#' Vector is defined as v1=(f1-f2). The cordinate frame is:
#' \itemize{
#'   \item X=(xt1-xt2),
#'   \item Z=X x (0,1,0),
#'   \item Y=X x Z
#' }
#' All vectors are normalized.
#'
#' @param dataToCalculate data frame with motion capture data.
#' @param f1 name of the first joint of vector.
#' @param f2 name of the second joint of vector.
#' @param xt1 name of the first joint of horizontal axis of coordinate frame.
#' @param xt2 name of the second joint of horizontal axis of coordinate frame.
#'
#' @return
#' Results are list of vectors defined as follow: (angle(Y, v1), angle(Z, v1), angle(X, v1)).
#'
#' @examples
#' data("heian.yondan")
#' vector.to.angles.frame.list(heian.yondan[1:3,], "RightArm", "RightForearm", "RightShoulder", "LeftShoulder")
vector.to.angles.frame.list <- function(dataToCalculate, f1, f2, xt1, xt2)
{
  #dataToCalculate = refdatakinematic
  #f1 = "RightArm"
  #f2 = "RightForearm"
  #xt1 = "RightShoulder"
  #xt2 = "LeftShoulder"


  listhelper <- list()

  x <- c()
  y <- c()
  z <- c()

  for (b in 1:length(dataToCalculate[,paste(f1,".Dx",sep = "")]))
  {
    vector.helper <- c(dataToCalculate[b,paste(f1,".Dx",sep = "")] - dataToCalculate[b,paste(f2,".Dx",sep = "")],
                     dataToCalculate[b,paste(f1,".Dy",sep = "")] - dataToCalculate[b,paste(f2,".Dy",sep = "")],
                     dataToCalculate[b,paste(f1,".Dz",sep = "")] - dataToCalculate[b,paste(f2,".Dz",sep = "")])

    xt <- c(dataToCalculate[b,paste(xt1,".Dx",sep = "")] - dataToCalculate[b,paste(xt2,".Dx",sep = "")],
         dataToCalculate[b,paste(xt1,".Dy",sep = "")] - dataToCalculate[b,paste(xt2,".Dy",sep = "")],
         dataToCalculate[b,paste(xt1,".Dz",sep = "")] - dataToCalculate[b,paste(xt2,".Dz",sep = "")])

    xyz <- generate.frame(xt)

    x <- c(x, vector.angle(xyz[[2]],vector.helper))
    y <- c(y, vector.angle(xyz[[3]],vector.helper))
    z <- c(z, vector.angle(xyz[[1]],vector.helper))
  }
  #listhelper[[b]] <- c(vector.angle(xyz[2],vector.helper),
  #                     vector.angle(xyz[3],vector.helper),
  #                     vector.angle(xyz[1],vector.helper))
  listhelper[[1]] <- x
  listhelper[[2]] <- y
  listhelper[[3]] <- z

  return(listhelper)
}

##################

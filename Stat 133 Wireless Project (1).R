library(plyr)

################################################################################
############################### PART I #########################################
################################################################################

# Read in our file
txt = readLines("~/Desktop/offline.final.trace.txt")


# Get rid of all the data that we don't need
txt = txt[-(grep("#", txt))]


### Task 1

# Read data into R and save it as a data frame
processLine = function (x) {
  index = strsplit(x, ";")
  index = index[[1]]
  timestamp.index = grep("t=", index)
  time = gsub("t=","",index[timestamp.index])
  id.index = grep("id=", index)
  scanMac = gsub("id=","",index[id.index])
  pos.index = grep("pos=", index)
  position = gsub("pos=","",index[pos.index])
  position = strsplit(position, ",")
  position = position[[1]]
  posX = position[1]
  posY = position[2]
  posZ = position[3]
  deg.index = grep("degree=", index)
  orientation = gsub("degree=","",index[deg.index])
  
  mac = vector(mode = "character", length = length(index))
  signal = vector(mode = "character", length = length(index))
  channel = vector(mode = "character", length = length(index))
  type = vector(mode = "character", length = length(index))
  
  for (i in 5:length(index)) {
    device = strsplit(index[i], "=|,")
    device = device[[1]]
    
    mac[i] = device[1]
    signal[i] = device[2]
    channel[i] = device[3]
    type[i] = device[4]
    
  }
  
  mac = mac[-(1:4)]
  signal = signal[-(1:4)]
  channel = channel[-(1:4)]
  type = type[-(1:4)]
  
  time = rep(time, length(mac))
  scanMac = rep(scanMac, length(mac))
  posX = rep(posX, length(mac))
  posY = rep(posY, length(mac))
  posZ = rep(posZ, length(mac))
  orientation = rep(orientation, length(mac))
  
  
  data = matrix( data = c(time, scanMac, posX, posY, posZ, orientation,
                          mac, signal, channel, type), ncol = 10, nrow = length(time))
  colnames(data) = c("time","scanMac","posX", "posY", "posZ",
                     "orientation", "mac", "signal", "channel", "type")
  
  return(data)
}



# Process our data and return as a data frame
tmp = lapply(txt, processLine)
offline = as.data.frame(do.call("rbind", tmp), stringsAsFactors = FALSE)
names(offline) = c("time", "scanMac", "posX", "posY", "posZ",
                   "orientation", "mac", "signal", "channel", "type")

# Run this line only if there are NAs in our data frame
offline = offline[-(which(is.na(offline$mac))),]


### Task 2

# Write a function that cleans data from processLine and returns a data frame
cleanData = function(data, keepMacs = c("mm:mm:mm:ss:ss:ss", etc)) {
  
  data$time = as.numeric(as.character(data$time))
  data$posX = as.numeric(as.character(data$posX))
  data$posY = as.numeric(as.character(data$posY))
  data$posZ = as.numeric(as.character(data$posZ))
  data$orientation = as.numeric(as.character(data$orientation))
  data$signal = as.numeric(as.character(data$signal))
  data$channel = as.numeric(as.character(data$channel))
  data$type = as.numeric(as.character(data$type))
  
  data$orientation.round = round_any(data$orientation, 45)
  data$orientation.round[data$orientation.round == 360] = 0
  
  data$scanMac = NULL
  data = data[data$type != 1,]
  data$type = NULL
  data$posZ = NULL
  data$channel = NULL
  
  data = data[data$mac %in% keepMacs,]    
  
  return(data)
  
}


# Using exploratory data analysis, we found the five Cisco routers. We found
# the sixth router by selecting the non-Cisco signal with the greatest number
# of records of its signal strength.
routers = c("00:14:bf:b1:97:8a", "00:14:bf:b1:97:90", "00:0f:a3:39:e1:c0",
            "00:14:bf:b1:97:8d", "00:14:bf:b1:97:81", "00:14:bf:3b:c7:c6")

# Clean our offline data with a call to cleanData and
offline2 = cleanData(offline, routers)





################################################################################
############################## PART III ########################################
################################################################################

# For each unique position and orientation combination, we have about 110 measurements.
# When cross-validating, we do not want to split the 110 measurements taken at a location
# across folds. Instead, we want to summarize them into one record. Our collapse function
# below takes the mean signal strength at each unique location-orientation location.
collapse = function(offline2, mac){
  S1 = offline2$signal[offline2$mac == mac]
  S1.orientation = offline2$orientation.round[offline2$mac == mac]
  S1.posX = offline2$posX[offline2$mac == mac]
  S1.posY = offline2$posY[offline2$mac == mac]
  
  S1.df = data.frame(S1, S1.orientation, S1.posX, S1.posY)
  
  combos1 = unique(S1.df[,-1])
  means = numeric()
  
  for (i in 1:nrow(combos1)){
    means[i] = mean(S1.df$S1[S1.df$S1.posX == combos1$S1.posX[i] &
                               S1.df$S1.posY == combos1$S1.posY[i] &
                               S1.df$S1.orientation == combos1$S1.orientation[i]])    
  }
  
  combos1$mean = means
  colnames(combos1) = c("orientation", "posX", "posY", "mean")
  
  return(combos1)
}


# Call collapse function on each signal strength to get mean signal strength at each
# unique location-orientation.  
S1 = collapse(offline2, "00:14:bf:b1:97:8a")
S2 = collapse(offline2, "00:14:bf:b1:97:90")
S3 = collapse(offline2, "00:0f:a3:39:e1:c0")
S4 = collapse(offline2, "00:14:bf:b1:97:8d")
S5 = collapse(offline2, "00:14:bf:b1:97:81")
S6 = collapse(offline2, "00:14:bf:3b:c7:c6")

# Create a data frame of all locations, orientations, and respective collapsed
# signal strengths. This will be our training set to find the best k to predict
# positions later on.
training.df = data.frame(x = S1$posX, y = S1$posY, theta = S1$orientation,
                         SS1 = S1$mean, SS2 = S2$mean, SS3 = S3$mean, SS4 = S4$mean,
                         SS5 = S5$mean, SS6 = S6$mean)


# Write the Nearest Neighbors function which takes in a point (npoint) and returns the
# points from training.df2, which are ordered by how far away they are from npoint
# (closest to farthest)
nearestN = function(npoint , training.df2) {
  
  # npoint is the point we're trying to find the nearest neighbor(s)
  # of from training.df2
  
  a = apply(training.df2[ , 3:8] , 1 , function(x) {
    x - npoint
  } )
  b = apply(a, 2 , function(x) {
    sqrt(sum(x^2))
  } )
  c = order(b)
  return(training.df2[c, 1:2])
}



# This function takes takes in a point and calls the nearest neighbor function on the
# point. It takes the mean position of the k nearest neighbors to predict the location
# of the point.
PredXY = function(point, training.df2, k) {
  closest = nearestN(point,training.df2)
  predX = mean(closest[1:k,1])
  predY = mean(closest[1:k,2])
  return(paste(predX,predY))
}



# cvKnn2 uses cross-validation. We split up training.df2 into v folds. For each fold,
# we called PredXY to predict the locations of the points in the fold from the rest of
# the training.df2 not in the fold. This returns a matrix of predicted positions
# for each point for different values of k.
cvKnn2 = function(training.df2, k, v) {
  n = nrow(training.df2)
  permRows = sample(n)
  folds = matrix(permRows, byrow = TRUE, nrow = v)
  
  
  vs = matrix(0, nrow = n, ncol = length(k))
  for (j in 1:length(k)) {
    for(i in 1:v) {
      for (h in 1:length(folds[i,])){
        
        
        vs[folds[i, h],j] = PredXY(k = k[j], point = as.vector(c(training.df2[folds[i,h],3:8]), mode = "numeric"),
                                   training.df2 = training.df2[-folds[i,h] , ])
      }
    }
  }  
  colnames(vs) = k
  
  return(vs)
}


# As the name implies, calcError calculates the error of our predicted values from
# by taking the Euclidean distance from our predicted values and their actual positions.
calcError = function(vs, posX, posY){
  
  error = matrix(NA, ncol = ncol(vs), nrow = nrow(vs))
  posX = rep(posX, ncol(vs))
  posY = rep(posY, ncol(vs))
  
  for(j in 1:length(vs)){
    
    estX = numeric()
    estY = numeric()
    
    estX[j] = as.numeric(strsplit(vs[j], " ")[[1]])[1]
    estY[j] = as.numeric(strsplit(vs[j], " ")[[1]])[2]
    
    
    error[j] = sqrt((estX[j] - posX[j]) ^2 + (estY[j] - posY[j]) ^2)    
    
  }
  return(error)  
}


# We now want to use cross-validation on our training set (training.df) to find the best k
# to use to predict the locations of our online data. We know that signal strengths vary
# with orientation, so we have to decide if we want to (a) do cross-validation for each orientation
# subset of training.df or (b) if we want to use a summary statistic of signal strengths for each
# position. From our plots, it seems that orientation does not significantly affect signal strength,
# but we will run our functions here to check.


## Find the best k's for each orientation (theta)

# Subset training.df by orientations
theta.0 = training.df[training.df$theta == 0,]
theta.45 = training.df[training.df$theta == 45,]
theta.90 = training.df[training.df$theta == 90,]
theta.135 = training.df[training.df$theta == 135,]
theta.180 = training.df[training.df$theta == 180,]
theta.225 = training.df[training.df$theta == 225,]
theta.270 = training.df[training.df$theta == 270,]
theta.315 = training.df[training.df$theta == 315,]


# Cross-validate and predict positions for each position and k
k.0 = cvKnn2(theta.0[-166,], k = 1:20, v = 15)
k.45 = cvKnn2(theta.45[-166,], k = 1:20, v = 15)
k.90 = cvKnn2(theta.90[-166,], k = 1:20, v = 15)
k.135 = cvKnn2(theta.135[-166,], k = 1:20, v = 15)
k.180 = cvKnn2(theta.180[-166,], k = 1:20, v = 15)
k.225 = cvKnn2(theta.225[-166,], k = 1:20, v = 15)
k.270 = cvKnn2(theta.270[-166,], k = 1:20, v = 15)
k.315 = cvKnn2(theta.315[-166,], k = 1:20, v = 15)


# Get the errors and best Ks for each theta

theta0.error = calcError(k.0, theta.0$x[-166], theta.0$y[-166])

x = numeric()
for (i in 1: ncol(theta0.error)) {
  x[i] = (mean(theta0.error[ ,i ]))
}

k.0 = which(x == min(x))
#k.0 = 4

theta45.error = calcError(k.45, theta.45$x[-166], theta.45$y[-166])

x = numeric()
for (i in 1: ncol(theta45.error)) {
  x[i] = (mean(theta45.error[ ,i ]))
}

k.45 = which(x == min(x))
#k.45 = 7

theta90.error = calcError(k.90, theta.90$x[-166], theta.90$y[-166])

x = numeric()
for (i in 1: ncol(theta90.error)) {
  x[i] = (mean(theta90.error[ ,i ]))
}

k.90 = which(x == min(x))
#k.90 = 6

theta135.error = calcError(k.135, theta.135$x[-166], theta.135$y[-166])

x = numeric()
for (i in 1: ncol(theta135.error)) {
  x[i] = (mean(theta135.error[ ,i ]))
}

k.135 = which(x == min(x))
#k.135 = 7

theta180.error = calcError(k.180, theta.180$x[-166], theta.180$y[-166])

x = numeric()
for (i in 1: ncol(theta180.error)) {
  x[i] = (mean(theta180.error[ ,i ]))
}

k.180 = which(x == min(x))
#k.180 = 4

theta225.error = calcError(k.225, theta.225$x[-166], theta.225$y[-166])

x = numeric()
for (i in 1: ncol(theta225.error)) {
  x[i] = (mean(theta225.error[ ,i ]))
}

k.225 = which(x == min(x))
#k.225 = 3

theta270.error = calcError(k.270, theta.270$x[-166], theta.270$y[-166])

x = numeric()
for (i in 1: ncol(theta270.error)) {
  x[i] = (mean(theta270.error[ ,i ]))
}

k.270 = which(x == min(x))
#k.270 = 5

theta315.error = calcError(k.315, theta.315$x[-166], theta.315$y[-166])

x = numeric()
for (i in 1: ncol(theta315.error)) {
  x[i] = (mean(theta315.error[ ,i ]))
}

k.315 = which(x == min(x))
#k.315 = 17


k.mean = round(mean(c(k.0,k.45,k.90,k.135,k.180,k.225,k.270,k.315)))
# k.mean = 7
k.median = round(median(c(k.0,k.45,k.90,k.135,k.180,k.225,k.270,k.315)))
# k.median = 6



# We also want to check what the best k is if we ignore orientation
# Take the means of signal strengths for each (X, Y) position (don't account for orientation)
training.df2 = aggregate(training.df, by = list(training.df$x, training.df$y) , FUN = mean)
training.df2$Group.1 = NULL
training.df2$Group.2 = NULL
training.df2$theta = NULL


# Cross-validate and get a matrix with the predicted values
training.predict = cvKnn2(training.df2[-166, ], k = 1:20, v = 15)
# Get the errors for each predicted value in the matrix
training.error = calcError(training.predict, training.df2$x[-166], training.df2$y[-166])


# Get the k with the smallest mean error
x = numeric()
for (i in 1: ncol(training.error)) {
  x[i] = (mean(training.error[ ,i ]))
}

k = which(x == min(x))
# k = 4



### Finally, we use our different best ks to predict the positions for the test data
### and analyze which one works best.

#Loading the test data and processing it with the function we wrote
testdata = readLines("~/Desktop/online.final.trace.txt")
testdata = testdata[-(grep("#", testdata))]
testdata = lapply(testdata, processLine)
testdata = as.data.frame(do.call("rbind", testdata), stringsAsFactors = FALSE)
names(testdata) = c("time", "scanMac", "posX", "posY", "posZ",
                    "orientation", "mac", "signal", "channel", "type")
testdata = cleanData(testdata, routers)

S1.test = collapse(testdata, "00:14:bf:b1:97:8a")
S2.test = collapse(testdata, "00:14:bf:b1:97:90")
S3.test = collapse(testdata, "00:0f:a3:39:e1:c0")
S4.test = collapse(testdata, "00:14:bf:b1:97:8d")
S5.test = collapse(testdata, "00:14:bf:b1:97:81")
S6.test = collapse(testdata, "00:14:bf:3b:c7:c6")

test.df = data.frame(x = S1.test$posX, y = S1.test$posY, theta = S1.test$orientation,
                     SS1 = S1.test$mean,SS2 = S2.test$mean, 
                     SS3 = S3.test$mean, SS4 = S4.test$mean,
                     SS5 = S5.test$mean, SS6 = S6.test$mean)



# This is testing the accuracy of the k we found without taking orientation into account
# k = 4
Predictions = character()
for (i in 1:nrow(test.df)) {
  
  Predictions[i]= PredXY(k = k, point = as.vector(c(test.df[i,4:9]), mode = "numeric"),
                         training.df2 = training.df2)
  
}

Predictions = matrix(Predictions, ncol = 1)
PredictionsNum = (strsplit(Predictions, " "))
PredictionsNum = lapply(PredictionsNum, as.numeric)
PredictionsNum = t(as.data.frame(PredictionsNum, stringsAsFactors = FALSE))
row.names(PredictionsNum) = NULL
colnames(PredictionsNum) = c("x","y")

errK4 = calcError(Predictions , test.df$x , test.df$y)
avg_errK4 = mean(errK4)


# We now edit our PredXY function to use each orientation's 
# best k depending on the orientation of our test data
# best ks k.0 = 4, k.45 - 7, k.90 - 6, k.135 - 7, k.180 - 4, k.225 - 3, k.270 - 5, k.315 - 17

PredXYonline = function(point, training.df2, k = c(k.0,k.45,k.90,k.135,k.180,k.225,k.270,k.315), orientation) {
  closest = nearestN(point,training.df2)
  if (orientation == 0) {
    predX = mean(closest[1:k[1],1])
    predY = mean(closest[1:k[1],2])
  } else if (orientation == 45) {
    predX = mean(closest[1:k[2],1])
    predY = mean(closest[1:k[2],2])
  } else if (orientation == 90) {
    predX = mean(closest[1:k[3],1])
    predY = mean(closest[1:k[3],2])
  } else if (orientation == 135) {
    predX = mean(closest[1:k[4],1])
    predY = mean(closest[1:k[4],2])
  } else if (orientation == 180) {
    predX = mean(closest[1:k[5],1])
    predY = mean(closest[1:k[5],2])
  } else if (orientation == 225) {
    predX = mean(closest[1:k[6],1])
    predY = mean(closest[1:k[6],2])
  } else if (orientation == 270) {
    predX = mean(closest[1:k[7],1])
    predY = mean(closest[1:k[7],2])
  } else if (orientation == 315) {
    predX = mean(closest[1:k[8],1])
    predY = mean(closest[1:k[8],2])
  }
  return(paste(predX,predY))
}

# These are our predictions where we adjusted k based on orientation

Prediction = character()
for (i in 1:nrow(test.df)) {
  
  Prediction[i]= PredXYonline( point = as.vector(c(test.df[i,4:9]), mode = "numeric"), training.df2 = training.df2,
                               orientation = test.df$theta[i])
  
}

Prediction = matrix(Prediction, ncol = 1)

PredictionNum = (strsplit(Prediction, " "))

PredictionNum = lapply(PredictionNum, as.numeric)

PredictionNum = t(as.data.frame(PredictionNum, stringsAsFactors = FALSE))

row.names(PredictionNum) = NULL
colnames(PredictionNum) = c("x","y")

errDegree = calcError(Prediction , test.df$x , test.df$y)
avg_errDegree = mean(errDegree)

# We also tested the median and mean ks for each orientation

# Using k.mean = 7
Predictions2 = character()
for (i in 1:nrow(test.df)) {
  
  Predictions2[i]= PredXY(k = k.mean, point = as.vector(c(test.df[i,4:9]), mode = "numeric"), training.df2 = training.df2)
  
}

Predictions2 = matrix(Predictions2, ncol = 1)

PredictionsNum2 = (strsplit(Predictions2, " "))

PredictionsNum2 = lapply(PredictionsNum2, as.numeric)

PredictionsNum2 = t(as.data.frame(PredictionsNum2, stringsAsFactors = FALSE))

row.names(PredictionsNum2) = NULL
colnames(PredictionsNum2) = c("x","y")

errAve = calcError(Predictions2 , test.df$x , test.df$y)
ave_errAve = mean(errAve)

# Using k.median = 6

Predictions3 = character()
for (i in 1:nrow(test.df)) {
  
  Predictions3[i]= PredXY(k = k.median, point = as.vector(c(test.df[i,4:9]), mode = "numeric"), training.df2 = training.df2)
  
}

Predictions3 = matrix(Predictions3, ncol = 1)

PredictionsNum3 = (strsplit(Predictions3, " "))

PredictionsNum3 = lapply(PredictionsNum3, as.numeric)

PredictionsNum3 = t(as.data.frame(PredictionsNum3, stringsAsFactors = FALSE))

row.names(PredictionsNum3) = NULL
colnames(PredictionsNum3) = c("x","y")

errMed = calcError(Predictions3 , test.df$x , test.df$y)
ave_errMed = mean(errMed)

#Error rates for different methods of prediction

#Average
errAve = calcError(Predictions2 , test.df$x , test.df$y)

#Median
errMed = calcError(Predictions3  , test.df$x , test.df$y)

#By orientation
errDegree = calcError(Prediction , test.df$x , test.df$y)

#No orientation
errK4 = calcError(Predictions , test.df$x , test.df$y)

# In our report we analyze these different attempts at predicting locations and the effect
# orientation may have in our predictions.


################################################################################
############################## PART II #########################################
################################################################################

### Use plots to explore our data, these plots are
### also used in our report to analyze our findings


# Plot the (X, Y) positions to get a sense of where X and Y axes and (0, 0) are
plot(offline2$posX, offline2$posY)

# The following 6 plots examine the effect of orientation on signal strength at point (0,0) for each router


par( mfrow = c( 2, 3 ) )

plot(x = 1:8,y = c(training.df[1,4], training.df[2,4],training.df[3,4],training.df[4,4],
                   training.df[5,4],training.df[6,4],training.df[7,4],training.df[8,4]), ylim = c(-70,-30), 
     ylab = "Signal Strength at (0,0)", xlab = "Orientation", 
     main = "Signal Strength vs Orienation: \n Router 00:14:bf:b1:97:8a at (0,0)")
plot(x = 1:8,y = c(training.df[1,5], training.df[2,5],training.df[3,5],training.df[4,5],
                   training.df[5,5],training.df[6,5],training.df[7,5],training.df[8,5]), ylim = c(-70,-30), 
     ylab = "Signal Strength at (0,0)", xlab = "Orientation", 
     main = "Signal Strength vs Orienation: \n Router 00:14:bf:b1:97:90 at (0,0)")
plot(x = 1:8,y = c(training.df[1,6], training.df[2,6],training.df[3,6],training.df[4,6],
                   training.df[5,6],training.df[6,6],training.df[7,6],training.df[8,6]), ylim = c(-70,-30), 
     ylab = "Signal Strength at (0,0)", xlab = "Orientation", 
     main = "Signal Strength vs Orienation: \n Router 00:0f:a3:39:e1:c0 at (0,0)")
plot(x = 1:8,y = c(training.df[1,7], training.df[2,7],training.df[3,7],training.df[4,7],
                   training.df[5,7],training.df[6,7],training.df[7,7],training.df[8,7]), ylim = c(-70,-30), 
     ylab = "Signal Strength at (0,0)", xlab = "Orientation", 
     main = "Signal Strength vs Orienation: \n Router 00:14:bf:b1:97:8d at (0,0)")
plot(x = 1:8,y = c(training.df[1,8], training.df[2,8],training.df[3,8],training.df[4,8],
                   training.df[5,8],training.df[6,8],training.df[7,8],training.df[8,8]), ylim = c(-70,-30), 
     ylab = "Signal Strength at (0,0)", xlab = "Orientation", 
     main = "Signal Strength vs Orienation: \n Router 00:14:bf:b1:97:81 at (0,0)")
plot(x = 1:8,y = c(training.df[1,9], training.df[2,9],training.df[3,9],training.df[4,9],
                   training.df[5,9],training.df[6,9],training.df[7,9],training.df[8,9]), ylim = c(-70,-30), 
     ylab = "Signal Strength at (0,0)", xlab = "Orientation", 
     main = "Signal Strength vs Orienation: \n Router 00:14:bf:3b:c7:c6 at (0,0)")


# The following 6 plots examine the effect of orientation on signal strength at point (22,5) for each router


par( mfrow = c( 2, 3 ) )

plot(x = 1:8,y = c(training.df[849,4], training.df[850,4],training.df[851,4],training.df[852,4],
                   training.df[853,4],training.df[854,4],training.df[855,4],training.df[856,4]), ylim = c(-85,-30), 
     ylab = "Signal Strength at (22,5)", xlab = "Orientation", 
     main = "Signal Strength vs Orienation: \n Router 00:14:bf:b1:97:8a at (22,5)")
plot(x = 1:8,y = c(training.df[849,5], training.df[850,5],training.df[851,5],training.df[852,5],
                   training.df[853,5],training.df[854,5],training.df[855,5],training.df[856,5]), ylim = c(-85,-30), 
     ylab = "Signal Strength at (22,5)", xlab = "Orientation", 
     main = "Signal Strength vs Orienation: \n Router 00:14:bf:b1:97:90 at (22,5)")
plot(x = 1:8,y = c(training.df[849,6], training.df[850,6],training.df[851,6],training.df[852,6],
                   training.df[853,6],training.df[854,6],training.df[855,6],training.df[856,6]), ylim = c(-85,-30), 
     ylab = "Signal Strength at (22,5)", xlab = "Orientation", 
     main = "Signal Strength vs Orienation: \n Router 00:0f:a3:39:e1:c0 at (22,5)")
plot(x = 1:8,y = c(training.df[849,7], training.df[850,7],training.df[851,7],training.df[852,7],
                   training.df[853,7],training.df[854,7],training.df[855,7],training.df[856,7]), ylim = c(-85,-30), 
     ylab = "Signal Strength at (22,5)", xlab = "Orientation", 
     main = "Signal Strength vs Orienation: \n Router 00:14:bf:b1:97:8d at (22,5)")
plot(x = 1:8,y = c(training.df[849,8], training.df[850,8],training.df[851,8],training.df[852,8],
                   training.df[853,8],training.df[854,8],training.df[855,8],training.df[856,8]), ylim = c(-85,-30), 
     ylab = "Signal Strength at (22,5)", xlab = "Orientation", 
     main = "Signal Strength vs Orienation: \n Router 00:14:bf:b1:97:81 at (22,5)")
plot(x = 1:8,y = c(training.df[849,9], training.df[850,9],training.df[851,9],training.df[852,9],
                   training.df[853,9],training.df[854,9],training.df[855,9],training.df[856,9]), ylim = c(-85,-30), 
     ylab = "Signal Strength at (22,5)", xlab = "Orientation", 
     main = "Signal Strength vs Orienation: \n Router 00:14:bf:3b:c7:c6 at (22,5)")


#Setting up the data for the violin plots


o1.1 = offline2$signal[offline2$mac == "00:14:bf:b1:97:8a" & offline2$orientation.round == 0]
o1.2 = offline2$signal[offline2$mac == "00:14:bf:b1:97:8a" & offline2$orientation.round == 45]
o1.3 = offline2$signal[offline2$mac == "00:14:bf:b1:97:8a" & offline2$orientation.round == 90]
o1.4 = offline2$signal[offline2$mac == "00:14:bf:b1:97:8a" & offline2$orientation.round == 135]
o1.5 = offline2$signal[offline2$mac == "00:14:bf:b1:97:8a" & offline2$orientation.round == 180]
o1.6 = offline2$signal[offline2$mac == "00:14:bf:b1:97:8a" & offline2$orientation.round == 225]
o1.7 = offline2$signal[offline2$mac == "00:14:bf:b1:97:8a" & offline2$orientation.round == 270]
o1.8 = offline2$signal[offline2$mac == "00:14:bf:b1:97:8a" & offline2$orientation.round == 315]




o2.1 = offline2$signal[offline2$mac == "00:14:bf:b1:97:90" & offline2$orientation.round == 0]
o2.2 = offline2$signal[offline2$mac == "00:14:bf:b1:97:90" & offline2$orientation.round == 45]
o2.3 = offline2$signal[offline2$mac == "00:14:bf:b1:97:90" & offline2$orientation.round == 90]
o2.4 = offline2$signal[offline2$mac == "00:14:bf:b1:97:90" & offline2$orientation.round == 135]
o2.5 = offline2$signal[offline2$mac == "00:14:bf:b1:97:90" & offline2$orientation.round == 180]
o2.6 = offline2$signal[offline2$mac == "00:14:bf:b1:97:90" & offline2$orientation.round == 225]
o2.7 = offline2$signal[offline2$mac == "00:14:bf:b1:97:90" & offline2$orientation.round == 270]
o2.8 = offline2$signal[offline2$mac == "00:14:bf:b1:97:90" & offline2$orientation.round == 315]



o3.1 = offline2$signal[offline2$mac == "00:0f:a3:39:e1:c0" & offline2$orientation.round == 0]
o3.2 = offline2$signal[offline2$mac == "00:0f:a3:39:e1:c0" & offline2$orientation.round == 45]
o3.3 = offline2$signal[offline2$mac == "00:0f:a3:39:e1:c0" & offline2$orientation.round == 90]
o3.4 = offline2$signal[offline2$mac == "00:0f:a3:39:e1:c0" & offline2$orientation.round == 135]
o3.5 = offline2$signal[offline2$mac == "00:0f:a3:39:e1:c0" & offline2$orientation.round == 180]
o3.6 = offline2$signal[offline2$mac == "00:0f:a3:39:e1:c0" & offline2$orientation.round == 225]
o3.7 = offline2$signal[offline2$mac == "00:0f:a3:39:e1:c0" & offline2$orientation.round == 270]
o3.8 = offline2$signal[offline2$mac == "00:0f:a3:39:e1:c0" & offline2$orientation.round == 315]


o4.1 = offline2$signal[offline2$mac == "00:14:bf:b1:97:8d" & offline2$orientation.round == 0]
o4.2 = offline2$signal[offline2$mac == "00:14:bf:b1:97:8d" & offline2$orientation.round == 45]
o4.3 = offline2$signal[offline2$mac == "00:14:bf:b1:97:8d" & offline2$orientation.round == 90]
o4.4 = offline2$signal[offline2$mac == "00:14:bf:b1:97:8d" & offline2$orientation.round == 135]
o4.5 = offline2$signal[offline2$mac == "00:14:bf:b1:97:8d" & offline2$orientation.round == 180]
o4.6 = offline2$signal[offline2$mac == "00:14:bf:b1:97:8d" & offline2$orientation.round == 225]
o4.7 = offline2$signal[offline2$mac == "00:14:bf:b1:97:8d" & offline2$orientation.round == 270]
o4.8 = offline2$signal[offline2$mac == "00:14:bf:b1:97:8d" & offline2$orientation.round == 315]



o5.1 = offline2$signal[offline2$mac == "00:14:bf:b1:97:81" & offline2$orientation.round == 0]
o5.2 = offline2$signal[offline2$mac == "00:14:bf:b1:97:81" & offline2$orientation.round == 45]
o5.3 = offline2$signal[offline2$mac == "00:14:bf:b1:97:81" & offline2$orientation.round == 90]
o5.4 = offline2$signal[offline2$mac == "00:14:bf:b1:97:81" & offline2$orientation.round == 135]
o5.5 = offline2$signal[offline2$mac == "00:14:bf:b1:97:81" & offline2$orientation.round == 180]
o5.6 = offline2$signal[offline2$mac == "00:14:bf:b1:97:81" & offline2$orientation.round == 225]
o5.7 = offline2$signal[offline2$mac == "00:14:bf:b1:97:81" & offline2$orientation.round == 270]
o5.8 = offline2$signal[offline2$mac == "00:14:bf:b1:97:81" & offline2$orientation.round == 315]


o6.1 = offline2$signal[offline2$mac == "00:14:bf:3b:c7:c6" & offline2$orientation.round == 0]
o6.2 = offline2$signal[offline2$mac == "00:14:bf:3b:c7:c6" & offline2$orientation.round == 45]
o6.3 = offline2$signal[offline2$mac == "00:14:bf:3b:c7:c6" & offline2$orientation.round == 90]
o6.4 = offline2$signal[offline2$mac == "00:14:bf:3b:c7:c6" & offline2$orientation.round == 135]
o6.5 = offline2$signal[offline2$mac == "00:14:bf:3b:c7:c6" & offline2$orientation.round == 180]
o6.6 = offline2$signal[offline2$mac == "00:14:bf:3b:c7:c6" & offline2$orientation.round == 225]
o6.7 = offline2$signal[offline2$mac == "00:14:bf:3b:c7:c6" & offline2$orientation.round == 270]
o6.8 = offline2$signal[offline2$mac == "00:14:bf:3b:c7:c6" & offline2$orientation.round == 315]




library(vioplot)



##  Set up plotting in two rows and three columns, plotting along rows first.
par( mfrow = c( 2, 3 ) )

##  The first plot is located in row 1, column 1:
vioplot(o1.1,o1.2,o1.3,o1.4,o1.5,o1.6,o1.7,o1.8, col = "red", ylim = c(-90,-30),
        names = c("0","45","90","135","180","225","270","315"))
title(ylab = "Signal Strength", xlab = "Orientation", main = "Router 1 \n00:14:bf:b1:97:8a")

##  The second plot is located in row 1, column 2:
vioplot(o2.1,o2.2,o2.3,o2.4,o2.5,o2.6,o2.7,o2.8, col = "green", ylim = c(-90,-30),
        names = c("0","45","90","135","180","225","270","315"))
title(ylab = "Signal Strength", xlab = "Orientation", main = "Router 2 \n00:14:bf:b1:97:90")


##  The third plot is located in row 1, column 3:
vioplot(o3.1,o3.2,o3.3,o3.4,o3.5,o3.6,o3.7,o3.8, col = "gold", ylim = c(-90,-30),
        names = c("0","45","90","135","180","225","270","315"))
title(ylab = "Signal Strength", xlab = "Orientation", main = "Router 3 \n00:0f:a3:39:e1:c0")


##  The fourth plot is located in row 2, column 1:
vioplot(o4.1,o4.2,o4.3,o4.4,o4.5,o4.6,o4.7,o4.8, col = "purple", ylim = c(-90,-30),
        names = c("0","45","90","135","180","225","270","315"))
title(ylab = "Signal Strength", xlab = "Orientation", main = "Router 4 \n00:14:bf:b1:97:8d")

##  The fifth plot is located in row 2, column 2:
vioplot(o5.1,o5.2,o5.3,o5.4,o5.5,o5.6,o5.7,o5.8, col = "blue", ylim = c(-90,-30),
        names = c("0","45","90","135","180","225","270","315"))
title(ylab = "Signal Strength", xlab = "Orientation", main = "Router 5 \n00:14:bf:b1:97:81")

##  The sixth plot is located in row 2, column 3:
vioplot(o6.1,o6.2,o6.3,o6.4,o6.5,o6.6,o6.7,o6.8, col = "orange", ylim = c(-90,-30),
        names = c("0","45","90","135","180","225","270","315"))
title(ylab = "Signal Strength", xlab = "Orientation", main = "Router 6 \n00:14:bf:3b:c7:c6")


# These violin plots show that on aggregate orientation does not have a huge impact
# even though it may differ per individual point





# Saving necessary items as rdas to load in the report.

save(training.df, file = "training.df.rda")
save(test.df, file = "test.df.rda")
save(PredictionsNum, file = "PredictionsNum.rda")
save(PredictionsNum2, file = "PredictionsNum2.rda")
save(PredictionsNum3, file = "PredictionsNum3.rda")
save(PredictionNum, file = "PredictionNum.rda")
save(errAve, file = "errAve.rda")
save(errK4, file = "errK4.rda")
save(errDegree, file = "errDegree.rda")
save(errMed, file = "errMed.rda")
save(offline2, file = "offline2.rda")

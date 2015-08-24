matrixDist <- function(matrix1, matrix2, norm = "L2") {
  
  # This function computes the distance between two given matrices based on the specified 
  # norm
  #
  #   Input:
  #       matrix1: first matrix
  #
  #       matrix2: second matrix
  #
  #       norm: a string specifying the distance norm, supported values are
  #             "L1", "L2", and "inf", default to "L2"
  #       
  #   Output:
  #       distance: distance of two matrices
  #
  
  if (!isTRUE(all.equal(dim(matrix1), dim(matrix2)))) {
    stop('The dimensionality of the two matrices should be the same')
  }

#   switch (norm,
#           "L1" =  (distance = firstNormDist(matrix1, matrix2)),
#           "L2" =  (distance = secondNormDist(matrix1, matrix2)),
#           "inf" =  (distance = infNormDist(matrix1, matrix2))
#   )

  # matrix to vector
  vector1 = as.vector(matrix1)
  vector2 = as.vector(matrix2)

  switch (norm,
          "L1" =  (distance = dist(rbind(vector1, vector2), method = "manhattan")),
          "L2" =  (distance = dist(rbind(vector1, vector2), method = "euclidean")),
          "inf" =  (distance = dist(rbind(vector1, vector2), method = "maximum"))
  )
  
  # since function 'dist' returns an object of type 'dist', need to convert it to a 
  # singleton vector

  distMatrix = as.matrix(distance)

  return(distMatrix[1, 2])
  
}

# below are internal functions

# # 1 norm
# firstNormDist <- function(matrix1, matrix2) {
#   
#   dist = 0
#   
#   for (i in 1 : dim(matrix1)[1]) {
#     for (j in 1 : dim(matrix1)[2]) {
#       dist = dist + abs(matrix1[i, j] - matrix2[i, j])
#     }
#   }
#   
#   return(dist)
# }
# 
# # 2 norm
# secondNormDist <- function(matrix1, matrix2) {
#   
#   dist = 0
#   
#   for (i in 1 : dim(matrix1)[1]) {
#     for (j in 1 : dim(matrix1)[2]) {
#       dist = dist + (matrix1[i, j] - matrix2[i, j]) ^ 2
#     }
#   }
#   
#   return(sqrt(dist))
# }
# 
# # Inf norm
# infNormDist <- function(matrix1, matrix2) {
#   
#   dist = -Inf
#   
#   for (i in 1 : dim(matrix1)[1]) {
#     for (j in 1 : dim(matrix1)[2]) {
#       
#       d = abs(matrix1[i, j] - matrix2[i, j])
#       
#       if (d > dist) {
#         dist = d
#       }
#     }
#   }
#   
#   return(dist)
# }
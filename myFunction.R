#List all the packages available
library()

#Install the DeliveryMan package
install.packages("/Users/marcellovendruscolo/Documents/rstudio-workspace/DeliveryMan/DeliveryMan_1.1.0.tar.gz", repos = NULL, type = "source")

#Load DeliveryMan package for this session
library(DeliveryMan)

#List all the packages currently loaded
search()

#Function for calculating the Manhattan distance between two given nodes
calculateManhattanDistance <- function(current_xCoordinate, current_yCoordinate, destination_xCoordinate, destination_yCoordinate) {
  dist <- abs(current_xCoordinate - destination_xCoordinate) + abs(current_yCoordinate - destination_yCoordinate)
  return(dist)
}

#To choose the next package to be picked up, Manhattan distance is used as a decision factor
chooseNextPackage <- function(current_xCoordinate, current_yCoordinate, packages_info) {
  
  #The number of packages to be handled in the problem
  amount_of_packages <- dim(packages_info)[1]
  
  #Initialising a variable to track the next package to be handled
  nextPackage <- 0
    
  #Creating a new column for registering the Manhattan distances of each of the remaining packages
  #Values in the Manhattan column are -1 to start with, as there is no negative Manhattan distance after calculation
  manhattanDistances <- matrix(seq(-1, -1, length.out = amount_of_packages), nrow = amount_of_packages, ncol = 1)
  packages_info <- cbind(packages_info, manhattanDistances)
  
  #Calculating the Manhattan distances of each package alternative that has not been handled yet
  for (i in 1:amount_of_packages) {
    if (packages_info[i,5] != 2)
      packages_info[i,6] <- calculateManhattanDistance(current_xCoordinate, current_yCoordinate, packages_info[i,1], packages_info[i,2])
  }
  
  #Selecting the package with smaller Manhattan distance, excluding those that have already been handled
  for (i in 1:amount_of_packages) {
    if (nextPackage == 0) {
      if (packages_info[i,6] != -1) {
        nextPackage <- i
      }
    } else {
        if (packages_info[i,6] < packages_info[nextPackage,6] & packages_info[i,6] != -1) {
          nextPackage <- i
        }
    }
  }
  return(nextPackage)
}

#Function to find the current best move in order to reach destination
findBestMove <- function(current_X, current_Y, destination_X, destination_Y, trafficConditions) {
  
  #Dimension of the x-axis and y-axis according to the given hroads and vroads matrices
  dimension_x_axis <- dim(trafficConditions$vroads)[1]
  dimension_y_axis <- dim(trafficConditions$hroads)[2]

  #Create a list containing a list to store all the nodes
  nodes_column <- vector(mode = "list", length = dimension_x_axis)
  for (x in 1:dimension_x_axis) {
    nodes_row <- vector(mode = "list", length = dimension_y_axis)
    for (y in 1:dimension_y_axis) {
      nodes_row[[y]] <- list("x" = x,"y" = y, "cost" = "inf", "visited" = "no", "first_move" = "null")
    }
    nodes_column[[x]] <- nodes_row
  }
  
  #Create the frontier list initially empty
  frontier_list <- vector(mode = "list", length = 0)
  
  #Set up the start node of this turn
  nodes_column[[current_X]][[current_Y]]$cost <- 0
  nodes_column[[current_X]][[current_Y]]$first_move <- 5
  
  #Append start node to the frontier list
  frontier_list[[length(frontier_list) + 1]] <- nodes_column[[current_X]][[current_Y]]
  
  #Repeat search until best path is found to destination, or break
  repeat {
    
    #If the frontier is empty and the destination has not been reached, there is no connected path between start and destination
    if (length(frontier_list) == 0)
      break
    else {
      #Assume the node from frontier list with lowest cost is the first one
      lowestCost_index <- 1
      #If the frontier list has more than one element, compare costs to find the index with lowest cost
      if (length(frontier_list) > 1) {
        for (tmp_index in 1:length(frontier_list)) {
          #If an element with lower cost than current is found, update index value holding the lowest cost node
          if (frontier_list[[tmp_index]]$cost < frontier_list[[lowestCost_index]]$cost) {
            lowestCost_index <- tmp_index
          }
        }
      }
      #Create a temporary node holding the node with lowest cost from the frontier list
      tmp_node <- frontier_list[[lowestCost_index]]
      #Remove node with lowest cost from frontier list
      frontier_list <- frontier_list[-lowestCost_index]
      
      #Check whether the temporary node is the destination. If positive, return the first move
      if (tmp_node$x == destination_X & tmp_node$y == destination_Y) {
        return(tmp_node$first_move)
      }
      
      #Check whether temporary node has a neighbour node to the right
      if (tmp_node$x + 1 <= dimension_x_axis) {
        #Create a node from the nodes list with coordinates (x + 1, y)
        right_neighbour <- nodes_column[[tmp_node$x + 1]][[tmp_node$y]]
        #Cost will be the cost to reach temporary node + the current traffic conditions between the two nodes + Manhattan distance
        right_neighbour$cost <- tmp_node$cost + trafficConditions$hroads[tmp_node$x, tmp_node$y] + calculateManhattanDistance(right_neighbour$x, right_neighbour$y, destination_X, destination_Y)
        #First move will refer back to the temporary node's first move
        right_neighbour$first_move <- tmp_node$first_move
        #If temporary node's first move is 5, it means it is the start node
        if (tmp_node$first_move == 5) {
          #Then, the right neighbour node's first move must be updated to 6 to reflect the first move
          right_neighbour$first_move <- 6
        }
        #Check whether the right neighbour has already been visited before
        if (right_neighbour$visited == "no") {
          #Also, check whether the right neighbour already exists in the frontier list
          repeated <- "no"
          if (length(frontier_list) > 0) {
            for (i in 1:length(frontier_list)) {
              if (frontier_list[[i]]$x == right_neighbour$x & frontier_list[[i]]$y == right_neighbour$y) {
                # As it already exists, check whether its previous cost was larger or smaller than through this new path
                repeated <- "yes"
                if (right_neighbour$cost < frontier_list[[i]]$cost) {
                  frontier_list <- frontier_list[-i]
                  frontier_list[[length(frontier_list) + 1]] <- right_neighbour
                }
              }
            }
          }
          #In case it does not exist in the frontier already, then add it
          if (repeated == "no") {
            frontier_list[[length(frontier_list) + 1]] <- right_neighbour
          }
        }
      }
      
      #Check whether temporary node has a neighbour node to the left
      if (tmp_node$x - 1 >= 1) {
        #Create a node from the nodes list with coordinates (x - 1, y)
        left_neighbour <- nodes_column[[tmp_node$x - 1]][[tmp_node$y]]
        #Cost will be the cost to reach temporary node + the current traffic conditions between the two nodes + Manhattan distance
        left_neighbour$cost <- tmp_node$cost + trafficConditions$hroads[tmp_node$x - 1, tmp_node$y] + calculateManhattanDistance(left_neighbour$x, left_neighbour$y, destination_X, destination_Y)
        #First move will refer back to the temporary node's first move
        left_neighbour$first_move <- tmp_node$first_move
        #If temporary node's first move is 5, it means it is the start node
        if (tmp_node$first_move == 5) {
          #Then, the left neighbour node's first move must be updated to 4 to reflect the first move
          left_neighbour$first_move <- 4
        }
        #Check whether the left neighbour has already been visited before
        if (left_neighbour$visited == "no") {
          #Also, check whether the left neighbour already exists in the frontier list
          repeated <- "no"
          if (length(frontier_list) > 0) {
            for (i in 1:length(frontier_list)) {
              if (frontier_list[[i]]$x == left_neighbour$x & frontier_list[[i]]$y == left_neighbour$y) {
                # As it already exists, check whether its previous cost was larger or smaller than through this new path
                repeated <- "yes"
                if (left_neighbour$cost < frontier_list[[i]]$cost) {
                  frontier_list <- frontier_list[-i]
                  frontier_list[[length(frontier_list) + 1]] <- left_neighbour
                }
              }
            }
          }
          #In case it does not exist in the frontier already, then add it
          if (repeated == "no") {
            frontier_list[[length(frontier_list) + 1]] <- left_neighbour
          }
        }
      }
      
      #Check whether temporary node has a neighbour node upwards
      if (tmp_node$y + 1 <= dimension_y_axis) {
        #Create a node from the nodes list with coordinates (x, y + 1)
        up_neighbour <- nodes_column[[tmp_node$x]][[tmp_node$y + 1]]
        #Cost will be the cost to reach temporary node + the current traffic conditions between the two nodes + Manhattan distance
        up_neighbour$cost <- tmp_node$cost + trafficConditions$vroads[tmp_node$x, tmp_node$y] + calculateManhattanDistance(up_neighbour$x, up_neighbour$y, destination_X, destination_Y)
        #First move will refer back to the temporary node's first move
        up_neighbour$first_move <- tmp_node$first_move
        #If temporary node's first move is 5, it means it is the start node
        if (tmp_node$first_move == 5) {
          #Then, the up neighbour node's first move must be updated to 8 to reflect the first move
          up_neighbour$first_move <- 8
        }
        #Check whether the up neighbour has already been visited before
        if (up_neighbour$visited == "no") {
          #Also, check whether the up neighbour already exists in the frontier list
          repeated <- "no"
          if (length(frontier_list) > 0) {
            for (i in 1:length(frontier_list)) {
              if (frontier_list[[i]]$x == up_neighbour$x & frontier_list[[i]]$y == up_neighbour$y) {
                # As it already exists, check whether its previous cost was larger or smaller than through this new path
                repeated <- "yes"
                if (up_neighbour$cost < frontier_list[[i]]$cost) {
                  frontier_list <- frontier_list[-i]
                  frontier_list[[length(frontier_list) + 1]] <- up_neighbour
                }
              }
            }
          }
          #In case it does not exist in the frontier already, then add it
          if (repeated == "no") {
            frontier_list[[length(frontier_list) + 1]] <- up_neighbour
          }
        }
      }
      
      #Check whether temporary node has a neighbour node downwards
      if (tmp_node$y - 1 >= 1) {
        #Create a node from the nodes list with coordinates (x, y - 1)
        down_neighbour <- nodes_column[[tmp_node$x]][[tmp_node$y - 1]]
        #Cost will be the cost to reach temporary node + the current traffic conditions between the two nodes + Manhattan distance
        down_neighbour$cost <- tmp_node$cost + trafficConditions$vroads[tmp_node$x, tmp_node$y - 1] + calculateManhattanDistance(down_neighbour$x, down_neighbour$y, destination_X, destination_Y)
        #First move will refer back to the temporary node's first move
        down_neighbour$first_move <- tmp_node$first_move
        #If temporary node's first move is 5, it means it is the start node
        if (tmp_node$first_move == 5) {
          #Then, the down neighbour node's first move must be updated to 2 to reflect the first move
          down_neighbour$first_move <- 2
        }
        #Check whether the down neighbour has already been visited before
        if (down_neighbour$visited == "no") {
          #Also, check whether the down neighbour already exists in the frontier list
          repeated <- "no"
          if (length(frontier_list) > 0) {
            for (i in 1:length(frontier_list)) {
              if (frontier_list[[i]]$x == down_neighbour$x & frontier_list[[i]]$y == down_neighbour$y) {
                # As it already exists, check whether its previous cost was larger or smaller than through this new path
                repeated <- "yes"
                if (down_neighbour$cost < frontier_list[[i]]$cost) {
                  frontier_list <- frontier_list[-i]
                  frontier_list[[length(frontier_list) + 1]] <- down_neighbour
                }
              }
            }
          }
          #In case it does not exist in the frontier already, then add it
          if (repeated == "no") {
            frontier_list[[length(frontier_list) + 1]] <- down_neighbour
          }
        }
      }
      
      #Update node in nodes list to show it has been explored and visited
      nodes_column[[tmp_node$x]][[tmp_node$y]]$visited = "yes"
    }
  }
}

#trafficConditions_information: list containing the hroads and vroads matrices (e.g., trafficConditions_information$hroads and trafficConditions_information$vroads). Dimensions of matrices are <dim,dim>.
#car_information: list containing current coordinates (e.g., car_information$x and car_information$y),
#the load of the car (car_information$load), and a variable nextMove (car_information$nextMove) that specifies the next move of the car.
#packages_information: matrix (N, 5) containing information about the N packages to be handled. Columns indicate: x pick-up coordinate,
#y pick-up coordinate, x drop-off coordinate, y drop-off coordinate, package status (0: not picked up, 1: carrying, 2: deliveried).
myFunction <- function(trafficConditions_information, car_information, packages_information) {
  
  #Whenever the delivery man does not have a package loaded in the car, pick up the nearest package (Manhattan Distance)
  if (car_information$load == 0) {
    #cat("Delivery Man is on the way to pick up a new package!")
    pickup_package <- chooseNextPackage(car_information$x, car_information$y, packages_information)
    #cat("The package with smaller Manhattan distance is: ", pickup_package, "\n")
    car_information$nextMove <- findBestMove(car_information$x, car_information$y, packages_information[pickup_package,1], packages_information[pickup_package,2], trafficConditions_information)
  }
  
  #If the delivery man has a package loaded in the car, choose shortest route to make the deliver
  if (car_information$load != 0) {
    #cat("Delivery Man is on the way to deliver package: ", car_information$load, "\n")
    car_information$nextMove <- findBestMove(car_information$x, car_information$y, packages_information[car_information$load,3], packages_information[car_information$load,4], trafficConditions_information)
  }
  
  return(car_information)
}

#Run one game of the Delivery Man
runDeliveryMan(carReady = myFunction, dim = 10, turns = 2000, doPlot = T, pause = 0.1, del = 5, verbose = T)

#Runs 500 games of the Delivery Man and compares the function myFunction against the par function
testDM(myFunction, verbose = 0, returnVec = FALSE, n = 500, seed = 21, timeLimit = 250)

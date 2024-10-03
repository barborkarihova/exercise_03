
IndexOfMin <- function(arr, first, last) {
  index <- first
  for ( k in (first + 1): last){
    print(k)
    print(index)
    if (arr[k] < arr[index]){
    index <- k
    }
  }
  return(index)
  
}


SelectionSort <- function(arr, n) {
  # arr: list of integers
  # n: the count of integers in the list
  
  # Traverse through all elements in the list
  for (i in 1:(n-1)) {
    # Assume the minimum element is the current i
  
    min_index = IndexOfMin(arr, (i+1), n)
    
    # Swap the found minimum element with the first element
    if (min_index != i) {
      temp <- arr[i]
      arr[i] <- arr[min_index]
      arr[min_index] <- temp
    }
  }
  
  return(arr)  # Return the sorted list
}

# Example usage:
arr <- c(64, 25, 12, 22, 11)
n <- length(arr)
sorted_arr <- SelectionSort(arr, n)
print(sorted_arr)

RecursiveSelectionSort <- function(arr, first, last) {
  # arr: list of integers
  # n: the count of integers in the list
  
  # Traverse through all elements in the list
  if  (first < last) {
    # Assume the minimum element is the current i
    
    min_index <- first
    for ( k in (first + 1): last){
      if (arr[k] < arr[min_index]){
        min_index <- k
      }
    }
    
    # Swap the found minimum element with the first element
    if (min_index != first) {
      temp <- arr[first]
      arr[first] <- arr[min_index]
      arr[min_index] <- temp
    }
    arr = RecursiveSelectionSort(arr, first+1,last)
  }
  
  
  return(arr)  # Return the sorted list
}

# Example usage:
arr <- c(64, 25, 12, 22, 11)
print(arr)
n <- length(arr)
sorted_arr <- RecursiveSelectionSort(arr,1, n)
print(sorted_arr)


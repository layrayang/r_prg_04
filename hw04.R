exchange.sort.asc <- function(input_vector, decreasing = FALSE){
vector_length <- length(input_vector)
  if (decreasing == TRUE){
  for(i in 1:(vector_length - 1)){
    for(j in (i+1):vector_length){
      if(input_vector[i] < input_vector[j]){
        gg <- input_vector[i]
        input_vector[i] <- input_vector[j]
        input_vector[j] <- gg
      }
    }
  } 
  return(input_vector)
} else{
  for(i in 1:(vector_length - 1)){
    for(j in (i+1):vector_length){
      if(input_vector[i] > input_vector[j]){
        gg <- input_vector[i]
        input_vector[i] <- input_vector[j]
        input_vector[j] <- gg
      }
    }
  } 
  return(input_vector)
}
}
new_factor <- runif(3)
exchange.sort.asc(new_factor)

##�зǮt
mm <- function(input_vector){
  mean(input_vector)
  sum87<- sum((input_vector - mean(input_vector))^2)
  h<- sum87 / (length(input_vector) - 1)
  standard_deviation<- sqrt(h)
return(standard_deviation)
}
mm(new_factor)
  
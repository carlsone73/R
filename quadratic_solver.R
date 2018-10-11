quadratic<- function() {
  a <- readline("What is the quadratic term? ")
  b <- readline("What is the linear term? ")
  c <- readline("What is the constant term? ")
  
  solution_1 = (-b + sqrt(b^2 - 4*a*c))/(2*a)
  solution_2 = (-b - sqrt(b^2 - 4*a*c))/(2*a)
  
  return(list(solution_1, solution_2))
         
}


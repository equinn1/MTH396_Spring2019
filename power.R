power <- function(delta,ssize,sigma){
  n_sig = 0
  for(i in 1:1000){
    x1 <- rnorm(ssize,0,sigma)
    x2 <- rnorm(ssize,delta,sigma)
    tt <- t.test(x1,y=x2,var.equal=TRUE)
    if (tt$p.value < 0.05) n_sig = n_sig+1
  } 
  return(n_sig/1000)
}

for(j in 1:20){
  print(power(0.1,500,5))
} 


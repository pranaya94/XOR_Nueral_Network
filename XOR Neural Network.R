library(dplyr)

xor_nn <- function(XOR,theta1,theta2,init_w = 0, learn = 0, alpha = 0.01)
{
  if(init_w == 1)
  {
        theta1 = matrix(runif(6,-1,1),2,3)
        
        theta2 = matrix(runif(6,-1,1),1,3)
        
  
  }   
 
        t1_DELTA = matrix(0,nrow(theta1),ncol(theta1))
        t2_DELTA = matrix(0,nrow(theta2),ncol(theta2))
        
        
        m = 0
        J = 0.0
        
        print("Neural Network Output")
        
        sigmoid <- function(x)
        {
          result = 1.0 / (1 + exp(-x))
          return(result)
        }
        
        for(i in 1:nrow(XOR))
        {
                  a1 = c(1,XOR[i,1:2])
                  
                  z2 = theta1 %*% a1
                  
                  a2 = c(1,sigmoid(z2))
                  
                  z3 = theta2 %*% a2
                  
                  h = sigmoid(z3)
                  
                  J = J + (XOR[i,3]*log(h)) + ((1-XOR[i,3]) * log(1-h))
                  
                  m = m + 1
                  
                if(learn == 1)  
                    {
                  
                      delta3 = h - XOR[i,3]
                      delta3
                      delta2 = (t(theta2) %*% delta3)[2:3,] *  (z2 * (1 - z2)) #[2:3,] given as theta error from bias node not propagated back 
                      delta2
                      
                      t2_DELTA = t2_DELTA + (delta3 %*% a2) #accumulating errors
                      t1_DELTA = t1_DELTA + (delta2 %*% a1)
                }
                  else {
                    #print(c("Hypothesis for ",XOR[i,1:2]," is ",h)) #printed every 1000 runs ie total of 10 times
                      }
                  print(c("Hypothesis for ",XOR[i,1:2]," is ",h)) #printed every 1000 runs ie total of 10 times
          } # nd for
        
        J = J/-m # average cost 
        
        if(learn == 1)
        {
          theta1 = theta1 - (alpha * (t1_DELTA/m))
          theta2 = theta2 - (alpha * (t2_DELTA/m))
        }
        else{
          print(c("J: ",J))
        }
        
        theta1_new = theta1;
        theta2_new = theta2;
        
        theta_list <- list(theta1_new,theta2_new)
        return(theta_list)
        
        
   
   } # end function


XOR = matrix(c(0,0,0,
               0,1,1,
               1,0,1,
               1,1,0),4,3,byrow = TRUE)


theta1 = matrix(0,2,3)

theta2 = matrix(0,1,3)

theta_list <- xor_nn(XOR,theta1,theta2,1,1,0.01) # initializing theta to raondom vlaues

for (n in 1:1400)
{
  theta_list <- xor_nn(XOR,theta_list[[1]],theta_list[[2]],0,1,0.01)
  print(n)
  
 # if (i%%1000 == 0){
    # print(c("Iteration : ",i))
    # theta_list <- xor_nn(XOR,theta_list[[1]],theta_list[[2]])
      
     #  }
    
}

theta_list <- xor_nn(XOR,theta_list[[1]],theta_list[[2]])



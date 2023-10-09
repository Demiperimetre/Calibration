library(deSolve)

# balldropg = function(X,theta) 
# # X contient  la position initiale et le temps 
# # theta contient la constante graviationnelle  
# {
#  X0=X[,1]
#  t=X[,2]
#  g=theta[1]
#  h=max(-g*.5*t^2+X0,0)
#  return(h)
# }

## attention x0
balldropg = function(t,theta) 
  # X contient  la position initiale et le temps 
  # theta contient la constante graviationnelle  
{
  g=theta[1]
  h0=theta[2]
  h=-g*.5*t^2+h0
  h[h<0]=0
  return(h)
}


acceleration = function(t,state,parameters){
  g=parameters[1]
  C=parameters[2]
  h=state[1]
  v=state[2]
  dv = -g+C*v^2
  dh= v
  list(c(dh=dh,dv=dv))
}

balldropgfrot = function(t,theta)
{
  g=theta[1]
  h0=theta[2]
  C = theta[3]
  state=c(h=h0,v=0)
  parameters=c(g,C)
  out=ode(state,seq(0,max(t)+.1,.01),func=acceleration,parameters)  
  h = out[,2]
  h[h<0] = 0
  times = out[,1]
  ind=sapply(t,function(temps) which.min(abs(times-temps)))
  return(h[ind])
}

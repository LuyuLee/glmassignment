####pringt figure 2
 n=40
 x = runif(n)
 y = sin(2*pi*x) + 0.2*rnorm(n)

 xe = min(x) + (max(x) - min(x))*(0:10)/10

source("pspline.R")
 out3 = pspline(x, y, xnew=xe, knots=3)

 out8 = pspline(x, y, xnew=xe, knots=6)
 
 out15 = pspline(x, y, xnew=xe, knots=12)

 out25 = pspline(x, y, xnew=xe, knots=20)

par(mfrow=c(2,2))

plot(x,y, main="no. of knots = 3")
lines(xe, out3$m)

plot(x,y, main="no. of knots = 6")
lines(xe, out8$m)


plot(x,y, main="no. of knots = 12")
lines(xe, out15$m)

plot(x,y, main="no. of knots = 20")
lines(xe, out25$m)



n=40
x = runif(n)
y = sin(2*pi*x) + 0.2*rnorm(n)

xe = min(x) + (max(x) - min(x))*(0:10)/10

out3 = pspline(x, y, xnew=x, knots=3)
ans3 = mean((y - out3$m)^2) 

out8 = pspline(x, y, xnew=x, knots=6)
ans8 = mean((y - out8$m)^2) 

out15 = pspline(x, y, xnew=x, knots=12)
ans15 = mean((y - out15$m)^2) 

out25 = pspline(x, y, xnew=x, knots=20)
ans25 = mean((y - out25$m)^2)
ans = data.frame(Knot=c(3, 8, 15, 25), rMSE = c(ans3, ans8, ans15, ans25))
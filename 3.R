xy = read.table("E:/general Linear model/assignment/Li Jialiang/Lecture03B/data03B01.dat")
 
x1 = xy[,1]
x2 = xy[,2]
x3 = xy[,3]
x4 = xy[,4]
y = xy[,5]

library('mgcv')

n = length(y)

 CV0 = 0
 for (i in 1:n)
 {
   out = gam(y~ s(x1) + s(x2) + s(x3) + s(x4), subset=-i)
    yipred = predict(out, list(x1= x1[i], x2= x2[i], x3= x3[i], x4= x4[i]))
    CV0 = CV0 + (y[i]-yipred)^2
 }
 CV0

 CV1 = 0
 for (i in 1:n)
 {
   out = gam(y~ x1 + s(x2) + s(x3) + s(x4), subset=-i)
    yipred = predict(out, list(x1= x1[i], x2= x2[i], x3= x3[i], x4= x4[i]))
    CV1 = CV1 + (y[i]-yipred)^2
 }
 CV1

 CV2 = 0
 for (i in 1:n)
 {
   out = gam(y~ s(x1) + x2 + s(x3) + s(x4), subset=-i)
    yipred = predict(out, list(x1= x1[i], x2= x2[i], x3= x3[i], x4= x4[i]))
    CV2 = CV2 + (y[i]-yipred)^2
 }
 CV2

 CV3 = 0
 for (i in 1:n)
 {
   out = gam(y~ s(x1) + s(x2) + x3 + s(x4), subset=-i)
    yipred = predict(out, list(x1= x1[i], x2= x2[i], x3= x3[i], x4= x4[i]))
    CV3 = CV3 + (y[i]-yipred)^2
 }
 CV3


 CV4 = 0
 for (i in 1:n)
 {
   out = gam(y~ s(x1) + s(x2) + s(x3) + x4, subset=-i)
    yipred = predict(out, list(x1= x1[i], x2= x2[i], x3= x3[i], x4= x4[i]))
    CV4 = CV4 + (y[i]-yipred)^2
 }
 CV4


 CV5 = 0
 for (i in 1:n)
 {
   out = gam(y~ x1 + x2 + s(x3) + s(x4), subset=-i)
    yipred = predict(out, list(x1= x1[i], x2= x2[i], x3= x3[i], x4= x4[i]))
    CV5 = CV5 + (y[i]-yipred)^2
 }
 CV5


c(CV0, CV1, CV2, CV3, CV4, CV5)/n


 out0 = gam(y~ s(x1) + s(x2) + s(x3) + s(x4))
 out1 = gam(y~ x1 + s(x2) + s(x3) + s(x4))
 out2 = gam(y~ s(x1) + x2 + s(x3) + s(x4))
 out3 = gam(y~ s(x1) + s(x2) + x3 + s(x4))
 out4 = gam(y~ s(x1) + s(x2) + s(x3) + x4)
 out5 = gam(y~ x1 + x2 + s(x3) + s(x4))

AIC(out0, out1, out2, out3, out4, out5)



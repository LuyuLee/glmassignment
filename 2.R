xy = read.table("E:/general Linear model/assignment/Li Jialiang/Lecture03B/data03B01.dat")
 
x1 = xy[,1]
x2 = xy[,2]
x3 = xy[,3]
x4 = xy[,4]
y = xy[,5]

library('mgcv')
out = gam(y~x1+x2+s(x3)+s(x4))

out$coefficients  # output the estimated coefficients

anova(out)    # model testing 

par(mfrow = c(1, 2))   
plot(out, se=TRUE)  # ploting the nonlinear parts
 
########

xyNEW = read.table("E:/general Linear model/assignment/Li Jialiang/Lecture03B/data03B01p.dat")

predict.gam(out, newdata = list(x1 = xyNEW[,1], x2 = xyNEW[,2], x3 = xyNEW[,3],
                            x4 = xyNEW[,4]))

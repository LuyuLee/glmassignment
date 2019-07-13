#install.packages('mda')
library(mda)

# training

  xy = read.table('E:/general Linear model/assignment/Li Jialiang/Lecture03C/data03C02.dat')
 
  x = data.matrix(xy[,2:9])
  y = xy[,10]
  outmars = mars(x, y, degree = 2)

# prediction

  xy = read.table('E:/general Linear model/assignment/Li Jialiang/Lecture03C/data03C03.dat')
  xnew = data.matrix(xy[,2:9])
  predicted = predict(outmars, xnew)
  mars_error  = mean((xy[,10]-predicted)^2)

  mars_error


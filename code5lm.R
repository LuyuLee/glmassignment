# training

  xy = read.table('E:/general Linear model/assignment/Li Jialiang/Lecture03C/data03C02.dat')
 
  lcavol = xy[,2]
  lweight= xy[,3]	
  age	= xy[,4]
  lbph = xy[,5]
  svi = xy[,6]
  lcp	= xy[,7]
  gleason = xy[,8]
  pgg45 = xy[,9]
  lpsa = xy[,10]	

  outlm = lm(lpsa ~ lcavol+lweight+age +
		lbph+svi+lcp+gleason+pgg45   )


# prediction

  xy = read.table('E:/general Linear model/assignment/Li Jialiang/Lecture03C/data03C03.dat')
  xnew =list(lcavol = xy[,2],
	  lweight= xy[,3],		
	  age	= xy[,4],
 	  lbph = xy[,5],
	  svi = xy[,6],
	  lcp	= xy[,7],
	  gleason = xy[,8],
	  pgg45 = xy[,9])
  
  predicted = predict(outlm, xnew)
  lm_error  = mean((xy[,10]-predicted)^2)

  lm_error



xy = read.table('E:/general Linear model/assignment/Li Jialiang/Lecture03B/spam.dat')
index = sample(nrow(xy), nrow(xy) * 0.5)
df = xy
xy = df[index,]
test = data.frame(as.matrix(df[-index,]))

x1 = xy[,1]
x2 = xy[,2]
x3 = xy[,3]
x4 = xy[,4]
x5 = xy[,5]
x6 = xy[,6]
x7 = xy[,7]
x8 = xy[,8]
x9 = xy[,9]
x10 = xy[,10]
x11 = xy[,11]
x12 = xy[,12]
x13 = xy[,13]
x14 = xy[,14]
x15 = xy[,15]
x16 = xy[,16]
x17 = xy[,17]
x18 = xy[,18]
x19 = xy[,19]
x20 = xy[,20]
x21 = xy[,21]
x22 = xy[,22]
x23 = xy[,23]
x24 = xy[,24]
x25 = xy[,25]
x26 = xy[,26]
x27 = xy[,27]
x28 = xy[,28]
x29 = xy[,29]
x30 = xy[,30]
x31 = xy[,31]
x32 = xy[,32]
x33 = xy[,33]
x34 = xy[,34]
x35 = xy[,35]
x36 = xy[,36]
x37 = xy[,37]
x38 = xy[,38]
x39 = xy[,39]
x40 = xy[,40]
x41 = xy[,41]
x42 = xy[,42]
x43 = xy[,43]
x44 = xy[,44]
x45 = xy[,45]
x46 = xy[,46]
x47 = xy[,47]
x48 = xy[,48]
x49 = xy[,49]
x50 = xy[,50]
x51 = xy[,51]
x52 = xy[,52]
x53 = xy[,53]
x54 = xy[,54]
x55 = xy[,55]
x56 = xy[,56]
x57 = xy[,57]

y   = xy[,58];

library('gam')

model0 = gam(y~s(x1)+s(x2)+s(x3)+s(x4)+s(x5)+s(x6)+s(x7)+s(x8)+s(x9)+s(x10)+
		s(x11)+s(x12)+s(x13)+s(x14)+s(x15)+s(x16)+s(x17)+s(x18)+s(x19)+s(x20)+
		s(x21)+s(x22)+s(x23)+s(x24)+s(x25)+s(x26)+s(x27)+s(x28)+s(x29)+s(x30)+
		s(x31)+s(x32)+s(x33)+s(x34)+s(x35)+s(x36)+s(x37)+s(x38)+s(x39)+s(x30)+
		s(x41)+s(x42)+s(x43)+s(x44)+s(x45)+s(x46)+s(x47)+s(x48)+s(x49)+s(x50)+
		s(x51)+s(x52)+s(x53)+s(x54)+s(x55)+s(x56)+s(x57), family='binomial')

anova(model0)

model1 = gam(y~ s(x4)+s(x5)+s(x6)+s(x7)+s(x8)+
		s(x14)+s(x16)+s(x17)+
		s(x24)+s(x25)+s(x26)+s(x27)+s(x28)+
		s(x36)+s(x37)+
		s(x42)+s(x46)+s(x49)+s(x50)+
		s(x52)+s(x53)+s(x55)+s(x57), family='binomial')


dex = c(4,5,6,7,8,14,16,17,24,25,26,27,28,36,37,42,46,49,50,52,53,55,57)
y.test = test[, 58]
par(ask=TRUE, mfrow=c(2,2))
plot(model1)

###predict###
text = test[,dex]
test.pred = predict.Gam(model1,  newdata = test)
log = exp(test.pred) / (1 + exp(test.pred))
ansdex = which(test.pred > 0.5)
ans = array()
ans[ansdex] = 1
ans[-ansdex] = 0
MSPE = mean((y.test - ans)^2)


######


predict(model1, newdata=list(x1 =0, x2=0.5, x3=0, x4=0, x5=0, x6=0, x7=0, x8=0, x9=10,
		x11=0, x12=0, x13=0, x14=0, x15=0, x16=0, x17=0, x18=0, x19=0,x20=0, 
		x21=0, x22=0, x23=0, x24=0, x25=0, x26=0, x27=0, x28=0, x29=0, x30=0, 
		x31=0, x32=0, x33=0, x34=0, x35=0, x36=0, x37=0, x38=0, x39=0, x40=0, 
		x41=0, x42=0, x43=0, x44=0, x45=0, x46=0, x47=0, x48=0, x49=0, x50=0, 
		x51=0.729, x52=0, x53=0.729, x54=0, x55=0, x56=3.833, x57=9), type='response')



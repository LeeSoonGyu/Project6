# 사례연구 6 이순규
library(ggplot2); library(lattice); library(latticeExtra)
library(esquisse); library(plotly); library(rgl)
library(gridExtra); library(scatterplot3d)
library(plotrix); library(dplyr); library(dygraphs)
library(maps); library(ggmap); library(reshape2)
library(UsingR); library(stringr); library(mlmRev)

# R에 포함된 기본 함수 차트 그리기
chart_data <- c(305,450,320,460,330,480,380,520)
names(chart_data) <- c('2018 1분기','2019 1분기',
                       '2018 2분기','2019 2분기',
                       '2018 3분기','2019 3분기',
                       '2018 4분기','2019 4분기')

barplot(chart_data, ylim = c(0,600),
        ylab = '매출액(단위: 만원)',
        xlab = '년도별 분기 현황',
        col = rainbow(8),
        main = '2018년도 vs 2019년도 매출현황 비교')

# 2. 가로 막대 차트
barplot(chart_data, xlim = c(0,600),horiz = T,
        ylab = '매출액(단위: 만원)',
        xlab = '년도별 분기 현황',
        col = rainbow(8),space = 1, cex.names = 0.8,
        main = '2018년도 vs 2019년도 매출현황 비교')

# 3. 누적 막대 차트
data("VADeaths")
barplot(VADeaths,beside =F, col =rainbow(5))
title(main = '미국 버지니아주 하위계층 사망비율',font.main = 4)
legend(3.8,200, c('50-54','55-59','60-64','65-69','70-74'),cex =0.8,fill=rainbow(5))

# 4. 점 차트
par(mfrow = c(1,1))
X11()
dotchart(chart_data, color = c('blue','red'),
         lcolor = 'black', pch = 6,
         labels = names(chart_data),
         xlab = '매출액',
         main = '분기별 판매현황 : 점차트 시각화',
         cex = 1.2)

# 5. 원형 차트
par(mfrow = c(1,1))
X11()
pie(chart_data, labels = names(chart_data),col = rainbow(8),cex =1.2, lty =2,border = 'red')
title('2018~2019년도 분기별 매출현황')

# 6. 상자 그래프
X11()
boxplot(VADeaths, range = 0,notch = T)
abline(h = 37, lty =3 , col = 'red')
title('상자 그래프 시각화 ')

# 7. 히스토그램
data(iris)

X11()
hist(iris$Sepal.Width, xlab = 'iris$Sepal.Width',col = 'blue', freq = T,
     main = 'iris 꽃받침 너비 histogram', xlim = c(2.0,4.5))
X11()
hist(iris$Sepal.Width, xlab = 'iris$Sepal.Width',col = 'skyblue', freq = F,
     main = "iris 꽃받침 너비 histogram", xlim = c(2.0,4.5))

lines(density(iris$Sepal.Width),col = 'red')

# 8. 산점도 그래프
data('airquality')
X11()
plot(Ozone~Wind ,  data = airquality, col = Month,pch = '♥',axes=T, ann =T)
title('산점도 그래프 시각화')
legend(18,120,c('5월','6월','7월','8월','9월'),cex = 0.8)

# 9. 중첩 자료 시각화
data(galton)
galtonData <- as.data.frame(table(galton$child, galton$parent))
head(galtonData)

names(galtonData) = c('child', 'parent', 'freq')
head(galtonData)
parent <- as.numeric(galtonData$parent)
child <- as.numeric(galtonData$child)

X11()
plot(parent, child, 
           pch = 21, col = 'blue', bg = 'green', 
           cex = 0.2 * galtonData$freq, 
           xlab = 'parent', ylab = 'child')
title('중첩 자료 시각화')

# 10. 변수간의 비교 시각화
pairs(iris[1:4], pch= 21, bg=c('red','green','blue')[unclass(iris$Species)])
legend('top', col = c('red','green','blue'), legend = levels(iris$Species), pch = 20,
       xpd = NA,ncol = 3, bty = "n", inset = -0.20, pt.cex = 1.5)

# 11. 3차원 산점도
# 꽃의 종류별 분류
iris_setosa = iris[iris$Species == 'setosa',]
iris_versicolor = iris[iris$Species == 'versicolor',]
iris_virginica = iris[iris$Species == 'virginica',]

d3 <- scatterplot3d(iris$Petal.Length,
                    iris$Sepal.Length,
                    iris$Sepal.Width,
                    type = 'n')
d3$points3d(iris_setosa$Petal.Length,  
            iris_setosa$Sepal.Length,
            iris_setosa$Sepal.Width,
            bg = 'orange',pch = 21)
d3$points3d(iris_versicolor$Petal.Length,
            iris_versicolor$Sepal.Length,
            iris_versicolor$Sepal.Width,
            bg = 'blue',pch = 23)
d3$points3d(iris_virginica$Petal.Length,
            iris_virginica$Sepal.Length,
            iris_virginica$Sepal.Width,
            bg = 'gray',pch = 25)

title('iris의 3차원 산점도')

# 12. 밀도 그래프
data("Chem97")
Chem97
X11()
densityplot(~gcsescore | factor(score), data = Chem97,
            groups = gender, plot.points = T, auto.key = T,
            main = '밀도 그래프')

# 13. 지도 시각화
# 인천의 위도와 경도
incheon <- c(left = 126.12183, bottom = 37.00465,
             right = 126.79200, top = 37.83552)
ic_map <- get_stamenmap(incheon, zoom = 10, maptype = 'terrain')
ggmap(ic_map)

# 지도 이미지에 레이어 적용
pop <- read.csv('E:/BigDate/R programing/dataset3/dataset3/population201901.csv',header =T)
pop

region <- pop$'지역명'
lon <- pop$LON 
lat <- pop$LAT 
tot_pop <- as.numeric(str_replace_all(pop$'총인구수',',',''))
df <- data.frame(region,lon,lat,tot_pop)
df <- df[1:17,]
daegu <- c(left =123.4423013, bottom = 32.8528306,
           right = 131.601445,top = 38.8714354) 
# 중심지역인 대구
map <- get_stamenmap(daegu, zoom = 7, maptype = 'watercolor')
layer1<-ggmap(map)
layer1
layer2 <- layer1 + geom_point(data = df,
                              aes(x= lon,y = lat,
                                  color = factor(tot_pop),
                                  size = factor(tot_pop)))
layer2
layer3 <- layer2 + geom_text(data= df,
                             aes(x = lon + 0.01, y = lat + 0.08,
                                 label = region),size = 3)
layer3

# ggplot / plotly 사용

# 1. 막대 차트(세로)
value <- c(305,450,320,460,330,480,380,520)
period <- c('2018 1분기','2019 1분기',
            '2018 2분기','2019 2분기',
            '2018 3분기','2019 3분기',
            '2018 4분기','2019 4분기')
chart_data2 <- data.frame(Period = period,Value = value)
str(chart_data2)

# lattice()
X11()
barchart(Value~Period, data = chart_data2,col = rainbow(8))

# esquisse()
ggplot(chart_data2) +
  aes(x = Period, fill = Period, weight = Value) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  ylim(0L, 600L)

# plot_ly
plot_ly(data = chart_data2,
        x = period,
        y = value,
        type = 'bar',
        color = period)
plot3d(period,value, col = rainbow(8))

chart_data3<-data.frame(
period2 <- c(2018.1, 2018.2, 2018.3,2018.4,2019.1,2019.2,2019.3,2019.4),
value2 <- c(305,320,330,380,450,460,480,520)
)
dyBarChart(dygraph(chart_data3))

# 2. 막대 차트(가로)
# lattice
X11()
barchart(Period~Value, data = chart_data2,col = rainbow(8),horiz=T)

# esquisse()

ggplot(chart_data2) +
  aes(x = Period, fill = Period, weight = Value) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  coord_flip() +
  theme_minimal() +
  ylim(0L, 600L) 

# plot_ly
plot_ly(data = chart_data2,
        x = value,
        y = period,
        type = 'bar',
        color = period,
        horizon = T)

# 3. 누적막대 차트

data("diamonds")

X11()
ggplot(diamonds, aes(x = clarity, fill = cut)) + geom_bar()

# 4. 점 차트
dotplot(period~value,chart_data2)
dotplot()

ggplot(chart_data2, aes(value,period))+
        geom_point()+theme_linedraw()

chart_data2 %>% plot_ly() %>% add_markers(x =value, y =period)

# 5. 원형 차트
ggplot(chart_data2, aes(x='',y=Value,fill=Period))+
        geom_bar(stat = 'identity')+
        coord_polar('y')+
        geom_text(aes(label=Value),position = position_stack((vjust = 0.5)))

pie3D(chart_data2$Value,labels = paste(chart_data2$Period,chart_data2$Value),
      explode = 0.3,labelcex = 0.8,shade =0.5,radius = 2.5)

# 6. 상자 그래프
VADeaths3<- as.data.frame.table(VADeaths)  
VADeaths3

X11()
ggplot(data = VADeaths3, aes(x=Var2,y=Freq))+
        geom_boxplot(fill=c('orange','yellow','blue','purple'),color='black',width=0.3)+
        geom_abline(intercept = 37, slope = 0 , color = 'red',linetype = 'dashed')

bwplot(Freq~Var2,data = VADeaths3)

plot_ly(y = VADeaths3$Freq,x = VADeaths3$Var2, type = 'box',quartilemethod = 'exclusive')

# 7. 히스토그램
X11()
histogram(~Sepal.Width, data=iris,xlab ='Sepal의 너비',ylab ='빈도수')

X11()
ggplot(iris) + aes(x = Sepal.Width) + geom_histogram(bins = 10L, fill = '#112446',color = 'black')

iris_sw <- iris$Sepal.Width
iris_sw
plot_ly(x = iris_sw, type = 'histogram')

# 8. 산점도
data("airquality")
X11()
xyplot(Ozone ~ Wind , group = Month,data = airquality,pch = '☆',
      col = c('orange','red','blue','purple','black'),auto.key  = T)

X11()
ggplot(airquality)+
      geom_point(aes(x=Wind, y=Ozone,color = factor(Month)))

# 9. 중첩자료 시각화
par(mfrow = c(1, 1))

X11()
xyplot(child~parent,cex = 0.2 * galtonData$freq, col = 'white',fill = 'red',pch = 21, alpha = 0.5)

X11()
ggplot(galtonData) +
  aes(x = parent, y = child, colour = freq) +
  geom_point(mapping = aes(size = 0.5*freq)) +
  scale_color_gradient() +
  theme_minimal()

# 10. 변수간의 비교 시각화
data("iris")

X11()
splom(~iris[1:4] |Species, data= iris)

p1<-ggplot(iris) +
  aes(x = Sepal.Length, y = Sepal.Width, colour = Species) +
  geom_point(shape = 'circle', size = 1.5) +
  scale_color_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(Species))
p2<-ggplot(iris) +
  aes(x = Sepal.Length, y = Petal.Length, colour = Species) +
  geom_point(shape = 'circle', size = 1.5) +
  scale_color_hue(direction = 1) +
  theme_minimal()
p3<-ggplot(iris) +
  aes(x = Sepal.Length, y = Petal.Width, colour = Species) +
  geom_point(shape = 'circle', size = 1.5) +
  scale_color_hue(direction = 1) +
  theme_minimal()
p4<-ggplot(iris) +
  aes(x = Sepal.Width, y = Petal.Length, colour = Species) +
  geom_point(shape = 'circle', size = 1.5) +
  scale_color_hue(direction = 1) +
  theme_minimal()
p5<-ggplot(iris) +
  aes(x = Sepal.Width, y = Petal.Width, colour = Species) +
  geom_point(shape = 'circle', size = 1.5) +
  scale_color_hue(direction = 1) +
  theme_minimal()
p6<-ggplot(iris) +
  aes(x = Petal.Length, y = Petal.Width, colour = Species) +
  geom_point(shape = 'circle', size = 1.5) +
  scale_color_hue(direction = 1) +
  theme_minimal()
grid.arrange(p1,p2,p3,p4,p5,p6, ncol=3) 

# ggplot의 여러 시각화를 동시에 한화면에 출력
# gridextra 패키지가 필요 grid.arrange 함수를 이용

# 11. 3차원 산점도 그래프
plot3d(iris[1:150,1:3])
with(iris, plot3d(iris[1:150, 1:3], type = 's', col = as.integer(iris$Species)))

cloud(Sepal.Length ~ Sepal.Width * Petal.Length,data=iris,group = Species,
      zlim = rev(range(iris$Sepal.Length)),auto.key = T)

plot_ly(iris, x = iris$Sepal.Length, y = iris$Sepal.Width, z = iris$Petal.Length,
        color = iris$Species, colors = c('#BF382A', '#0C4B8E')) %>% add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Sepal.Length'),
                      yaxis = list(title = 'Sepal.Width'),
                      zaxis = list(title = 'Petal.Length'))) 

# 12. 밀도그래프
ggplot(Chem97) +
        aes(x = gcsescore,
            fill = gender,
            colour = gender,
            group = gender) +
            geom_density(adjust = 1L) +
            scale_fill_brewer(palette = 'Set1', direction = -1) +
            scale_color_brewer(palette = 'Set1', direction = -1) +
            theme_bw() +
            facet_wrap(vars(score))

hist(iris$Sepal.Width, xlab = 'iris$Sepall.Width',col = 'gray',freq = F)
lines(density(iris$Sepal.Width), col = 'red',lwd = 5)

# 13.지도시각화
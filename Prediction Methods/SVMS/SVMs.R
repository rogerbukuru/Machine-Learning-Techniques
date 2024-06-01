rm(list = ls())
data("iris")


library(ggplot2)

p1 = ggplot(data = iris[1:100,],aes(x = Petal.Length, y = c(0)))+
            geom_point(aes(color = Species))

boundaries = data.frame(seperator = c(2,2.6))

p1+geom_point(data = boundaries, aes(x=boundaries$seperator[1]), color = "blue", size =3)+
  geom_point(data=boundaries, aes(x=boundaries$seperator[2]), color = "#F066EA", size = 3)


p2 = ggplot(data=iris[1:100,], aes(x=Petal.Width, y=Petal.Length))+
           geom_point(aes(color=Species))

p2
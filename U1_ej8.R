pez <- c(3.6,1.2,2.1,1.2,1.5,3.6,3.6,1.2,2.1,3.6,2.3,1.5,2,1.2)
hist(pez)
boxplot(pez)
pez <- tibble::as_tibble(pez)

library(tidyverse)
ggplot(pez,aes(x=value))+
  geom_histogram(bins = 6,fill='darkgreen')+
  labs(y= "frecuencia", x = "talla")+
  theme_light()

ggplot(pez, aes(x=value))+
  geom_boxplot(varwidth = F,col='darkgreen',fill='lightgreen')+
  #geom_dotplot(stackdir='center', dotsize=1)+
  labs(x = "talla")+
  coord_flip()+
  theme_light()

?nclass.Sturges
nclass.Sturges(pez)
range(pez)

diff(range(pez))
amplitud <- diff(range(pez))/nclass.Sturges(pez)

x_cut <- cut(pez, breaks = 5, right =F)

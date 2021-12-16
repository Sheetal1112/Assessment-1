install.packages("janitor")

library(tidyverse)
library(skimr)
library(janitor)
library(knitr)

#6
fastfood<-read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-09-04/fastfood_calories.csv")

#7
fastfood%>%head(10)%>%
kable()

kable(fastfood[1:10,1:5])

#8
fastfood%>%
  filter(calories>1000)

#9
dont_eat_this<-fastfood%>%
  filter(total_fat>40 & total_carb>80)%>%
  arrange(desc(dont_eat_this))

#10
fastfood%>%
  mutate(calories=case_when("heavy"~calories>500))

#11
skim(fastfood)

#12
fastfood%>%
  count(restaurant)

fastfood%>%
  count(restaurant)%>%
  arrange(desc(n))

n_distinct(fastfood)

#13
fastfood%>%
  group_by(restaurant)%>%
  summarise(mean(calories))%>%
  kable()
  
#14
average_calories<-fastfood%>%
  group_by(restaurant)%>%
  summarise(average_calories=mean(calories))%>%
  ungroup()

max_fat<-fastfood%>%
  group_by(restaurant)%>%
  summarise(max_fat=max(total_fat))%>%
  ungroup()

min_cholesterol<-fastfood%>%
  group_by(restaurant)%>%
  summarise(min_cholesterol=min(cholesterol))%>%
  ungroup()

#15
#Data visualisation assists in the telling of stories, by translating data into a more intelligible format. A good visualisation tells a storey by removing noise from data and emphasising crucial information. Data visualisation also enables businesses to get insight into their massive amounts of data.
fastfood%>%
  select(total_fat,restaurant)%>%
  ggplot(aes(restaurant,total_fat))+
  geom_col()
  
#16
cholesterol_sodium<-fastfood%>%
  group_by(restaurant)%>%
  summarise(cholesterol_sodium=sum(cholesterol+sodium))%>%
  ungroup()

fastfood%>%
  select(-salad)

#17
fastfood%>%
  filter(restaurant =="Mcdonalds")%>%
  ggplot(aes(sugar,protein))+
  geom_point(colour="purple")

#19
calories_type<-cut_number(fastfood$calories,n=3,labels=c("low", "med", "high"))

#20
fastfood%>%
   select(restaurant,calories)%>%
   ggplot(aes(restaurant,calories))+
  geom_col()+
  labs(x="Restaurant names", y="Number of calories",title="Calories type of each restaurant", caption="Image1:Bargraph")+
    theme_test()

#21
fastfood$trans_fat_percent=fastfood$trans_fat/fastfood$total_fat

#22
fastfood%>%
  select(restaurant,trans_fat,item)%>%
  ggplot(aes(restaurant,trans_fat,colour=restaurant))+
  geom_violin()+
  labs(x="Restaurant Names", y= "Amount of trans_fat", title="transfat for each type of restaurant")+
  theme_linedraw()
  
#23
fastfood%>%
  group_by(restaurant)%>%
  summarise(total_fat=mean(total_fat))

#24
fastfood%>%
  ggplot(aes(restaurant,total_fat,fill=restaurant))+
  geom_col()+
  coord_flip()+
  labs(x="Restaurant Names",y="amount of total fat",title="Total fat for each type of restaurant")+
  theme_get()


  


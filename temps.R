library(tidyverse)
library(formattable)
library(bayestestR)
library(tidylog)
library(ggridges)
library(reshape2)
library(viridis)
library(magrittr)
library(ggalt)

#Data frame containing tea varieties, min/max recommended steeping temperatures, and calculated temperatures at 33rd and 66th percentiles
temps<-data.frame(茶=c("ほうじ茶","くき茶","玄米茶","玉緑茶","煎茶","深蒸し茶","浅蒸し茶","玉露",
                        "烏龍茶","青茶","白茶","生茶","熱茶", "緑茶","かぶせ茶", "抹茶"),
                  極小=c(175,170,170,160,150,150,140,105,195,175,175,185,200,175,135,155),
                  極度=c(200,185,190,175,175,165,160,135,205,195,190,195,212,195,150,175)) %>%
  mutate(.,
         冷温=round(.$極小+(.$極度-.$極小)/3),
         高温=round(.$極度-(.$極度-.$極小)/3)) %>%
  arrange(.,極小,極度) %>%
  select(.,茶,極小,冷温,高温,極度)

#Formatted table with color gradient according to temperature
formattable(temps,
            align = c("r","l","l","l","l"),
            list("茶" = formatter("span",style = ~ style(font.weight = "bold")), 
              "極小"= color_tile("#FFEC8B", "#FF8C00"),
              "冷温"= color_tile("#FFEC8B", "#FF8C00"),
              "高温"= color_tile("#FFEC8B", "#FF8C00"),
              "極度"= color_tile("#FFEC8B", "#FF8C00")))

#Dumbell plot comparing the ranges of reccomended temperatures
ggplot(temps, aes(x=reorder(茶, 極小), xend=reorder(茶, 極小), y=極小)) + 
  geom_segment(aes(x=reorder(茶, 極小), xend=reorder(茶, 極小), y=極小, yend=極度,color=factor(reorder(茶, 極小))), size=12) +
  geom_text(aes(y=極小, label=極小),
            color="black", size=2, vjust=1.5)+
  geom_text(aes(y=極度, label=極度), 
            color="black", size=2, vjust=-1) +
  geom_text(aes(y=極度, label=reorder(茶, 極小)),
            color="black", size=3.5, vjust=-3)+
  scale_color_viridis(discrete = TRUE, option = "C") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none") +
  ylab("Temperature") +
  ylim(100,225)

#Data frame for graph, containing normal distributions spanning each reccomended temperature range
curves <- data.frame("ほうじ茶"=distribution_normal(1000, mean=mean(range(temps[temps$茶=="ほうじ茶",c(2,5)])),
                                                    sd=diff(range(temps[temps$茶=="ほうじ茶",2:5])/8)),
                     "くき茶"=distribution_normal(1000, mean=mean(range(temps[temps$茶=="くき茶",2:5])),
                                                   sd=diff(range(temps[temps$茶=="くき茶",2:5])/8)),
                     "玄米茶"=distribution_normal(1000, mean=mean(range(temps[temps$茶=="玄米茶",2:5])),
                                                     sd=diff(range(temps[temps$茶=="玄米茶",2:5])/8)),
                     "玉緑茶"=distribution_normal(1000, mean=mean(range(temps[temps$茶=="玉緑茶",2:5])),
                                                        sd=diff(range(temps[temps$茶=="玉緑茶",2:5])/8)),
                     "煎茶"=distribution_normal(1000, mean=mean(range(temps[temps$茶=="煎茶",2:5])), 
                                                  sd=diff(range(temps[temps$茶=="煎茶",2:5])/8)),
                     "抹茶"=distribution_normal(1000, mean=mean(range(temps[temps$茶=="抹茶",2:5])), 
                                                  sd=diff(range(temps[temps$茶=="抹茶",2:5])/8)),
                     "深蒸し茶"=distribution_normal(1000, mean=mean(range(temps[temps$茶=="深蒸し茶",2:5])), 
                                                            sd=diff(range(temps[temps$茶=="深蒸し茶",2:5])/8)),
                     "浅蒸し茶"=distribution_normal(1000, mean=mean(range(temps[temps$茶=="浅蒸し茶",2:5])),
                                                           sd=diff(range(temps[temps$茶=="浅蒸し茶",2:5])/8)),
                     "玉露"=distribution_normal(1000, mean=mean(range(temps[temps$茶=="玉露",2:5])),         
                                                   sd=diff(range(temps[temps$茶=="玉露",2:5])/8)),
                     "かぶせ茶"=distribution_normal(1000, mean=mean(range(temps[temps$茶=="かぶせ茶",c(2,5)])),
                                                    sd=diff(range(temps[temps$茶=="かぶせ茶",2:5])/8)),
                     "烏龍茶"=distribution_normal(1000, mean=mean(range(temps[temps$茶=="烏龍茶",2:5])),  
                                                       sd=diff(range(temps[temps$茶=="烏龍茶",2:5])/8)),
                     "青茶"=distribution_normal(1000, mean=mean(range(temps[temps$茶=="青茶",2:5])),
                                                        sd=diff(range(temps[temps$茶=="青茶",2:5])/8)),
                     "白茶"=distribution_normal(1000, mean=mean(range(temps[temps$茶=="白茶",2:5])),  
                                                 sd=diff(range(temps[temps$茶=="白茶",2:5])/8)),
                     "生茶"=distribution_normal(1000, mean=mean(range(temps[temps$茶=="生茶",2:5])), 
                                                     sd=diff(range(temps[temps$茶=="生茶",2:5])/8)),
                     "熱茶"=distribution_normal(1000, mean=mean(range(temps[temps$茶=="熱茶",2:5])),
                                                      sd=diff(range(temps[temps$茶=="熱茶",2:5])/8)),
                     "緑茶"=distribution_normal(1000, mean=mean(range(temps[temps$茶=="緑茶",2:5])),
                                              sd=diff(range(temps[temps$茶=="緑茶",2:5])/8))) %>% 
  select(.,temps$茶) %>%
  melt(.) %>%
  rename(.,
         Temperature=value,
         茶=variable)

#Chart plotting the range of suggested temperatures against one another, with a normal distribution spanning each recommended temp range
ggplot(curves, aes(x = Temperature, y = 茶, group= 茶, fill=..x..)) + 
  geom_density_ridges_gradient(rel_min_height = 0.01, scale=3, alpha=0.25) +
  theme_ridges()+
  scale_fill_viridis(option="C") +
  xlab("Temperature") +
  xlim(100,225)
  

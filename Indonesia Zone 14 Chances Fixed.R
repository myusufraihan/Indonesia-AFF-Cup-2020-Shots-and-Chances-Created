pacman::p_load(tidyverse, understatr, here, 
               ggrepel, ggsoccer, glue, 
               janitor, extrafont, ggforce, 
               RColorBrewer, png, patchwork, 
               cowplot, dplyr, ggplot2)

## Load in the csv
ina_chances <- readr::read_csv("C:/Users/Leonardus/Desktop/R stuff/indonesia aff shot maps/All Indonesia Chances Created.csv")

## Filter to only Zone 14 chances
ina_chances_zone14 <- ina_chances %>%
  filter(X >= 66.6 & X <= 83 & Y >= 33.3 & Y <= 66.6)

## Count chances created
chances_count <- dplyr::count(ina_chances_zone14, Player) %>%
  arrange(desc(n))

head(chances_count)

## Plot the viz!
ggp_inachances <- ina_chances_zone14 %>%
  ggplot(data = ina_chances_zone14, mapping = aes(x = X, y = Y))+
# Add football pitch to plot
  ggsoccer::annotate_pitch()+
  ggsoccer::theme_pitch()+
# Add points and arrows 
  geom_point(ina_chances_zone14, mapping = aes(x=X,y=Y,colour=Event))+
  geom_segment(ina_chances_zone14,
               mapping = aes(x=as.numeric(X),
                             y=as.numeric(Y),
                             xend=as.numeric(xend),
                             yend=as.numeric(yend),
                             colour=Event),
               arrow=arrow(length=unit(0.08,"inches")))+
# Flip pitch into a vertical view
  coord_flip(xlim = c(50,100),
             ylim = c(0,100))+
## Add the description
  geom_rect(xmin=83,xmax=66.6,ymin=66.6,ymax=33.3,alpha=0.015,fill="lightgrey",size=1,linetype="dashed",color="grey")+
  annotate("text",x=53,y=98,family="Montserrat SemiBold",label="ZONE 14 CHANCES",alpha=0.5,hjust=1,color="black", size = 10)+
  annotate("text",x=56,y=98,family="Montserrat SemiBold",label=paste(sum(chances_count$n), "TOTAL CHANCES CREATED"), alpha=0.5,hjust=1,color="black", size = 5)+
  annotate("text",x=68,y=50,family="Montserrat SemiBold",label="ZONE 14",alpha=0.25,color="black", size = 5)+
  annotate("text",x=57,y=-2,family="Montserrat SemiBold",
           label="
           Ricky Kambuaya: 4
           Witan Sulaeman: 3
           Rachmat Irianto: 2
           Alfeandra Dewangga: 1
           Dedik Setiawan: 1",hjust = 0,color="black", size = 4)+
  labs(title = "Where do Indonesia create their chances from?",
       subtitle = "AFF Suzuki Cup 2020",
       caption = glue("Made using FC Python's Event Tagging Tool
                      By @myusufraihan"))+
  theme(
    legend.position = "top",
    plot.title = element_text(colour = "black", hjust = .5, face = "bold", size = 20),
    plot.subtitle = element_text(colour = "black", hjust = .5, face = "bold", size = 15),
    plot.caption = element_text(colour = "black", hjust = .5, face = "bold", size = 10))+
  theme(text=element_text(family="Montserrat SemiBold", face="bold", size=12))+
## Add player image
  cowplot::draw_image(
    "C:/Users/Leonardus/Desktop/R stuff/indonesia aff shot maps/Viz/kambuayapng.png", 
    x = 56, y = -3, width = 35, height = 35)

ggp_inachances

## Save plot as PNG
ggplot2::ggsave(path = "C:/Users/Leonardus/Desktop/R stuff/indonesia aff shot maps/Viz", 
                filename = "Indonesia Zone 14 Chances + Foto.png", 
                width = 10, height = 8, dpi = 300, units = "in", device='png')
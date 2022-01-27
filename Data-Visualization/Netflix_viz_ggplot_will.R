#set up
library(tidyverse)
library(treemapify)
library(readr)
library(ggthemes)
library(ggdark)
library(ggpubr)
library(RColorBrewer)

netflix_titles <- read_csv("C:/Users/Will Jiang/Desktop/Emory Desktop/Data Viz/HW/HW1/netflix_titles.csv")

#Data Preparation
netflix <- select(netflix_titles,type,title,country,release_year,rating,listed_in)
netflix <- na.omit(netflix) #drop NA values
netflix$country <- gsub('United States','USA',netflix$country)
netflix$country <- gsub('United Kingdom','UK',netflix$country)

colnames(netflix)[6] <- 'category'

#create table with unique country
netflix_uni_country <- netflix %>%
  separate_rows(country,sep = ',') %>%
  mutate(across(where(is.character), str_trim))

#create table with unique category
netflix_uni_category <- netflix %>%
  separate_rows(category, sep = ',') %>%
  mutate(across(where(is.character), str_trim))

#RELEASE YEAR-----------------------------------------------------------------------

#Release Year of Movie/TV Show on Netflix
netflix_year <- count(netflix, release_year, type)

release_graph <- ggplot(netflix_year,aes(x=release_year,y=n),stat='identity') +
  geom_area(aes(fill=type)) +
  scale_fill_manual(values=c('#e50914','#a39999')) +
  dark_theme_light() +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank())+
  theme(legend.title = element_blank(),
        legend.position = c(0.2, 0.8))+
  ggtitle('Release Year of Movie/TV Show on Netflix')+
  theme(plot.title = element_text(face = 'bold'))

release_graph

#COUNTRY--------------------------------------------------------------------------

#Top 10% Country of Netflix Source
netflix_uni_country_cnt <- netflix_uni_country %>%
  count(country) %>%
  slice_max(n,prop=0.1) %>%
  mutate('max'=ifelse(country=='USA','yes','no'))

country_graph <- ggplot(netflix_uni_country_cnt,aes(x=reorder(country,n),y=n,fill=max)) +
  geom_bar(stat='identity') +
  coord_flip() +
  scale_fill_manual(values=c('yes'='#e50914','no'='#a39999'),guide = FALSE) +
  geom_text(aes(label=n), color='white',position=position_stack(vjust=0.9)) +
  dark_theme_light() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank())+
  ggtitle('Top 10% Country of Netflix Source')+
  theme(plot.title = element_text(face = 'bold'))

country_graph

#CATEGORY-------------------------------------------------------------------------------

netflix_tv <- filter(netflix,netflix$type=='TV Show')
netflix_movie <- filter(netflix,netflix$type=='Movie')

#create new movie table with unique category
netflix_movie_uni_category <- netflix_movie %>%
  separate_rows(category, sep = ',') %>%
  mutate(across(where(is.character), str_trim))

netflix_tv_uni_category <- netflix_tv %>%
  separate_rows(category, sep = ',') %>%
  mutate(across(where(is.character), str_trim))

netflix_movie_uni_category_cnt <- netflix_movie_uni_category %>%
  count(category)

netflix_tv_uni_category_cnt <- netflix_tv_uni_category %>%
  count(category)

#Different Movie Categories on Netflix
#expand color
getPalette = colorRampPalette(brewer.pal(9, 'Reds'))

mvcate_graph <- ggplot(netflix_movie_uni_category_cnt, aes(area=n, fill=category, label=category)) +
  geom_treemap() +
  geom_treemap_text(colour='black', place='centre', size = 15) +
  scale_fill_manual(values=getPalette(30))+
  dark_theme_light() +
  theme(legend.position = 'none')+
  ggtitle('Different Movie Genres on Netflix')+
  theme(plot.title = element_text(face = 'bold'))+
  labs(caption=' ')

#Different TV Show Categories on Netflix
tvcate_graph <- ggplot(netflix_tv_uni_category_cnt, aes(area=n, fill=category, label=category)) +
  geom_treemap() +
  geom_treemap_text(colour='black', place='centre', size = 15) +
  scale_fill_manual(values=getPalette(30)) +
  dark_theme_light() +
  theme(legend.position = 'none')+
  ggtitle('Different TV Show Genres on Netflix')+
  labs(caption='Copyright: Wei (Will) Jiang, Source: https://www.kaggle.com/shivamb/netflix-shows')+
  theme(plot.title = element_text(face = 'bold'),
        plot.caption = element_text(face = 'italic'))

mvcate_graph
tvcate_graph

#RATING-------------------------------------------------------------------------
#Countries with Movie/TV Show Rating Categories on Netflix
netflix_rate <- netflix_uni_country %>%
  filter(country %in% netflix_uni_country_cnt$country) %>%
  count(country,rating) %>%
  slice(-c(124:126))

getPalette2 = colorRampPalette(brewer.pal(9, 'Reds'))

rating_graph <- ggplot(netflix_rate,aes(x=reorder(country,desc(country)),
                        y=reorder(rating,n),
                        size=n,
                        color=rating)) +
  geom_count() +
  scale_color_manual(values = getPalette2(14)) +
  scale_size_area(max_size = 13) +
  dark_theme_light() +
  theme(legend.position = 'none') +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())+
  ggtitle('Top 10% Country with Movie/TV Show Rating on Netflix')+
  theme(plot.title = element_text(face = 'bold'))

rating_graph

#ALL IN ONE------------------------------------------------------------------------

library(cowplot)
netflix_plot <- ggdraw() +
  draw_plot(release_graph, x = 0, y = .75, width = .5, height = .25) +
  draw_plot(country_graph, x = .5, y = .75, width = .5, height = .25) +
  draw_plot(rating_graph, x = 0, y = 0.25, width = 1, height = 0.5) +
  draw_plot(mvcate_graph, x = 0, y = 0, width = 0.5, height = 0.25) +
  draw_plot(tvcate_graph, x = 0.5, y = 0, width = 0.5, height = 0.25)

netflix_plot

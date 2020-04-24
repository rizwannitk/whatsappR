library(rwhatsapp)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(stringr)
library(sqldf)
library(memisc)
library(tidytext)
library(gganimate)
library(tidyverse)
library(gifski)
library(av)

theme_set(theme_minimal())

#read the file & create a tibble

chat <- rwa_read("D:/Users/33980/Desktop/chat_test.txt")

View(chat)

## cleanup data

chat<-na.omit(chat)

chat<-filter(chat, !grepl('MAIK',author))

###Plot Chat by Date

chat %>%
  mutate(day = date(time)) %>%
  count(day) %>%
  ggplot(aes(x = day, y = n)) +
  geom_bar(stat = "identity") +
  ylab("") + xlab("") +
  ggtitle("Messages per day")


### Plot by day

chat %>%
  mutate(weekday = weekdays(time)) %>%
  count(weekday) %>%
  ggplot(aes(x = factor(weekday, levels= c("Sunday", "Monday", 
                                           "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")), y = n, , fill=weekday)) +
  geom_bar(stat = "identity") +
  ylab("Day of Week ") + xlab("") +
  ggtitle("Messages by day")


### Plot by Month

chat %>%
  mutate(month = month(time,label=TRUE, abbr=TRUE)) %>%
  count(month) %>%
  ggplot(aes(x = month, y = n, , fill=month)) +
  geom_bar(stat = "identity") +
  ylab("") + xlab("") +
  ggtitle("Messages by month")


### By who send messages

chat %>%
  mutate(day = date(time)) %>%
  count(author) %>%
  ggplot(aes(x = reorder(author, n), y = n,fill=author)) +
  geom_bar(stat = "identity") +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Number of messages")

## Most used emojis  by Sender

chat %>%
  unnest(emoji) %>%
  count(author, emoji, sort = TRUE) %>%
  #count( emoji, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder(emoji, n), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y")  +
  ggtitle("Most often used emojis")

### identify type of message

for (i in nrow(chat):1) {
  
  #msg <- paste(msgs[i,"text"], prevmsg, sep = " ")
  
  msg <-chat[i,"text"]
  
  isMedia <- 0
  numEmojies <- 0
  isHyperlink <- 0
  isimage <- 0
  isvedio <- 0
  isaudio <- 0
  isdocument <-0
  issticker  <-0
  isdeleted <-0
  
  if (unlist(gregexpr(pattern = "image omitted", text = msg))[1] >= 0) {
    isimage <- 1
    #print(msgs$sender[i])
    #print(msgs$timestamp[i])
    #print(isimage)
    text <- ""
    
  }
  
  else if (unlist(gregexpr(pattern = "video omitted", text = msg))[1] >= 0) {
    isvedio <- 1
    #print(msgs$sender[i])
    #print(msgs$timestamp[i])
    #print(isimage)
    text <- ""
    
  }
  
  else if (unlist(gregexpr(pattern = "audio omitted", text = msg))[1] >= 0) {
    isaudio <- 1
    #print(msgs$sender[i])
    #print(msgs$timestamp[i])
    #print(isimage)
    text <- ""
    
  }
  
  else if (unlist(gregexpr(pattern = "document omitted", text = msg))[1] >= 0) {
    isdocument  <-1
    #print(msgs$sender[i])
    #print(msgs$timestamp[i])
    #print(isimage)
    text <- ""
    
  }
  
  else if (unlist(gregexpr(pattern = "sticker omitted", text = msg))[1] >= 0) {
    issticker  <-1
    #print(msgs$sender[i])
    #print(msgs$timestamp[i])
    #print(isimage)
    text <- ""
    
  }
  
  else if (unlist(gregexpr(pattern = "https?:\\/\\/[\\w\\d][\\w\\d\\-]*(\\.[\\w\\d\\-]+)*\\.[\\w]+(\\/[\\w\\-\\_\\(\\)\\.\\?\\=\\&]*)*", text = msg, perl = TRUE))[1] >= 0) {
    isHyperlink <- 1
    text <- ""
  }
  
  else if (unlist(gregexpr(pattern = "This message was deleted.", text = msg, perl = TRUE))[1] >= 0) {
    isdeleted <- 1
    text <- ""
  }
  
  
  
  chat$isimage[i] <- isimage
  chat$isvedio[i] <- isvedio
  chat$isaudio[i] <- isaudio
  chat$issticker[i] <- issticker
  chat$isdocument[i] <- isdocument
  chat$ishyperlink[i] <- isHyperlink
  chat$isdeleted[i] <-isdeleted
}

chat$msgtype <- cases(
  "image" <- chat$isimage==1,
  "hyperlink" <- chat$ishyperlink==1,
  "audio" <- chat$isaudio==1,
  "document" <- chat$isdocument==1,
  "vedio" <- chat$isvedio==1,
  "sticker" <- chat$issticker==1,
  "deleted" <- chat$isdeleted==1,
  "text" <- chat$isimage==0 & chat$ishyperlink==0 & chat$isaudio==0 &  
    chat$isvedio==0  &   chat$isdocument==0 & chat$issticker==0 & chat$isdeleted==0
)

### Plot non text messages

nontextmsgs <-chat[chat$msgtype != 'text',]


ggplot(data =nontextmsgs  ,mapping = aes(x =nontextmsgs$msgtype, fill=nontextmsgs$msgtype) ) + theme_bw() + 
  geom_bar() +
  labs(x = "type", y = "Count") +
  theme(legend.position="right") +
  scale_fill_discrete(name="Type")

### non text messages by Sender

ggplot(data =nontextmsgs  ,mapping = aes(x =nontextmsgs$msgtype, fill=nontextmsgs$msgtype) ) + theme_bw() + 
  geom_bar() +
  labs(x = "type", y = "Count") +
  theme(legend.position="right") +
  scale_fill_discrete(name="Type")+
  facet_wrap(~author, ncol = 2, scales = "free_y")

chat$chat_date <-as_date(chat$time)

#### Animation

chat$chat_date <-as_date(chat$time)


chat_count <- chat %>% 
  group_by(author,chat_date) %>% 
  summarise(count = n()) 

chat_count<-as.data.frame(chat_count)

chat_min_date<-min(chat$chat_date)
chat_max_date<-max(chat$chat_date)

dates_loop <-seq(chat_min_date, chat_max_date, by=1)

chat_count_new <- chat_count %>% 
  filter( chat_count$chat_date== chat_min_date)

for (i in 1:length(dates_loop)) {
  chat_count_new <- chat_count %>% 
    dplyr::filter(chat_date <= dates_loop[i]) %>% 
    dplyr::group_by(author) %>% 
    dplyr::summarise(count = sum(count, na.rm = TRUE)) %>% 
    dplyr::mutate(chat_date = dates_loop[i]) %>% 
    dplyr::bind_rows(chat_count_new)
}


chat_formatted <- chat_count_new %>%
  group_by(chat_date) %>%
  dplyr::mutate(
    rank = min_rank(-count) * 1,
    Value_rel = count / count[rank == 1],
    Value_lbl = paste0(" ", count)
  ) %>%
  filter(rank <= 20) %>%
  ungroup()

chat_formatted$chat_date =as.Date(chat_formatted$chat_date ,"%d-%b-%y")


staticplot = ggplot(chat_formatted, aes(rank, group = author,
                                        fill = as.factor(author), color = as.factor(author))) +
  geom_tile(aes(y = count/2,
                height = count,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(author, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=count,label = Value_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))


anim = staticplot + transition_states( chat_date, transition_length = 4, state_length = 5) +
  view_follow(fixed_x = FALSE)  +
  labs( title = 'Messages  on : {closest_state}',
        #subtitle  =  "Top 20 IATA",
        caption  = "maikaltho friends")

animate(anim, 1000, fps = 10,  width = 1200, height = 1000,
        renderer = av_renderer("animation.mp4"),start_pause = 3 ,end_pause=5)

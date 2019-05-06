#load library
library(anytime)
library(plyr)
library(ggplot2)
library(scales)
#import data
ted_main_raw=read.csv(file.choose(), header=TRUE, sep=",")
#remove na
ted_main=na.omit(ted_main_raw)
#convert Unix date
ted_main$published_date=anytime(ted_main$published_date)
ted_main$film_date=anytime(ted_main$film_date)
#tag dictionary with frequency
tags_raw=paste(unlist(ted_main$tags), collapse =" ")
tags<-gsub("\\[","",tags_raw)
tags_split=strsplit(tags,"]|,")
freq=table(tags_split)

#general exploration
median_comment=median(ted_main$comments)
mean_comment=mean(ted_main$comments)
median_view=median(ted_main$views)
mean_view=mean(ted_main$views)

#most viewed
most_viewed=head(arrange(ted_main,desc(views)), n =25)
ggplot(most_viewed, aes(x=reorder(main_speaker,-views),y=views, fill= most_viewed$speaker_occupation)) +geom_bar(stat = "identity")+theme(axis.text.x=element_text(angle=90))+labs(title = "Top 25 Most Viewed TED Talks", x="Main Speaker", y="Number of views")+scale_y_continuous(labels=comma)+scale_fill_discrete(name = "Speaker Occupation")





qplot(ted_main$main_speaker,geom="histogram",stat="count")
barplot(prop.table(table(ted_main$main_speaker)))
hist(table(as.factor(ted_main$main_speaker)), freq=TRUE)

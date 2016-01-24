library(ggplot2)
library(stringi)
library(tm)
library(tau)
library(plyr)
library(languageR)

  ############ Set minimum and maximum word frequency
    a <- 15
    b <- 20
    
    ##### Read Alice in Wonderland and parse data
    #data("alice")
    #alice <- c(data[216])
    crude <- scan_tokenizer(alice)
    crude <- removeWords(crude, stopwords("SMART"))
crude <- textcnt(crude, method = "string", n =1L, lower = 1L)
wcrude <- crude
crude <- ldply(crude, data.frame)
wcrude <- stri_split_fixed(wcrude, " ")
zcrude <- data.frame(crude$.id, unlist(wcrude))
percent <- zcrude$unlist.wcrude.
Category <- zcrude$crude..id


Fpercent <- seq(1, length(percent))
FCategory <- seq(1, length(percent))
for (i in 1:length(percent)) {
SCategory <- stri_split_fixed(Category, " ")
Spercent <- stri_split_fixed(percent, " ")
if (Spercent[i] >= a & Spercent[i] <= b) {Fpercent[i] <- Spercent[i]} else {Fpercent[i] <- NA}
if (Spercent[i] >= a & Spercent[i] <= b) {FCategory[i] <- SCategory[i]} else {FCategory[i] <- NA}
}

Fmyd <-data.frame(Fpercent <- na.omit(unlist(Fpercent)), FCategory <- na.omit(unlist(FCategory)))



for ( i in 1:length(percent)) {
SCategory <- stri_split_fixed(Category, " ")
Spercent <- stri_split_fixed(percent, " ")
if (SCategory[i] == s) {print(paste(SCategory[i], "--", Spercent[i], sep = ""))}
}

Fmyd


ggplot(Fmyd, aes(x = FCategory, y = Fpercent, fill = FCategory)) + geom_bar(width = .75, stat="identity", colour="black", size=1) + coord_polar(theta = "x") + xlab("") + ylab("") + ggtitle("Radar Plot") + theme(legend.background = element_rect(colour = 'black', fill = "grey92", size = 1.5, linetype='solid'), plot.background = element_rect(fill="skyblue", color="black", size=2.5, linetype="solid"), panel.border = element_rect(fill=NA,color="black", size=2.5, linetype="solid"), panel.grid.major = element_line(colour = "skyblue", size=1), axis.text.x = element_text(colour = "black", face = "bold", angle = 90, size = 10), axis.line = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) + theme(legend.position="none",  axis.text.y=element_blank())



ggplot(Fmyd, aes(x = FCategory, y = Fpercent, fill = FCategory)) + geom_bar(width = .75, stat="identity", colour="black", size=1) + xlab("") + ylab("") + ggtitle("Radar Plot") + theme(legend.background = element_rect(colour = 'black', fill = "grey92", size = 1.5, linetype='solid'), plot.background = element_rect(fill="skyblue", color="black", size=2.5, linetype="solid"), panel.border = element_rect(fill=NA,color="black", size=2.5, linetype="solid"), panel.grid.major = element_line(colour = "skyblue", size=1), axis.text.x = element_text(colour = "black", face = "bold", angle = 90, size = 10), axis.line = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) + theme(legend.position="none",  axis.text.y=element_blank())














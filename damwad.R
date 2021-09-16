#Author: MEK
#Purpose: find insights from EU trade data

#Loading libraries 
library(ggplot2)

#Loading file
df <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Arbejde/full202052.dat")

#Extra-EU exports
extra_eu <- df[ which(df$FLOW==2  & df$TRADE_TYPE=='E'),] 

#Intra-EU exports
intra_eu <- df[ which(df$FLOW==2 & df$TRADE_TYPE=='I'),] 

#Creating a total sum of the trade value by EU member country
agg_intra <- aggregate(intra_eu$VALUE_IN_EUROS, by=list(DECLARANT_ISO=intra_eu$DECLARANT_ISO), FUN=sum)
agg_extra <- aggregate(extra_eu$VALUE_IN_EUROS, by=list(DECLARANT_ISO=extra_eu$DECLARANT_ISO), FUN=sum)

#Transform total value to EUR billon for future use
agg_intra$intra<-agg_intra$x/1000000000
agg_extra$extra<-agg_extra$x/1000000000

#Rename DECLARANT_ISO to country names
names <- c(DE="Germany",FR="France",NL="Netherlands",IT="Italy",BE="Belgium",ES="Spain",PL="Poland",AT="Austria",
           CZ="Czechia",SE="Serbia",HU="Hungary",SK="Slovakia",RO="Romania",DK="Denmark",PT="Portugal",FI="Finland",
           IE="Ireland",GR="Greece",GB="Great Britain", SI="Slovenia",LT="Lithuania",BG="Bulgaria",HR="Croatia",
           LU="Luxembourg",LV="Latvia",EE="Estonia", CY="Cyprus", MT="Malta")
agg_intra$clean_names <- as.character(names[agg_intra$DECLARANT_ISO])
agg_extra$clean_names <- as.character(names[agg_extra$DECLARANT_ISO])

#Create dataset showing the share of intra and extra EU exports
agg_extra$intra <- agg_intra$intra
agg_extra$sum <- agg_extra$intra+agg_extra$extra
agg_extra$share_intra <- (agg_extra$intra/agg_extra$sum)*100
agg_extra$share_extra <- (agg_extra$extra/agg_extra$sum)*100
agg_extra$clean_names <- factor(agg_extra$clean_names, levels = agg_extra$clean_names[order(-agg_extra$share_intra)])
keeps <- c("clean_names", "share_intra", "share_extra")
df <- agg_extra[keeps]
agg_extra.long<-melt(df,id.vars="clean_names")

######################################################################
### INSIGHT 1: Plot showing exports of goods to other Member States ###
######################################################################
#Sort
agg_intra$clean_names <- factor(agg_intra$clean_names, levels = agg_intra$clean_names[order(-agg_intra$intra)])

### Plot 1
 ggplot(agg_intra, aes(x=clean_names, y=intra)) + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color="lightgrey"),
    axis.ticks.y = element_blank(),
    axis.ticks.length.x.bottom = unit(1, "mm"),
    panel.background = element_blank()
  ) +
  geom_bar(stat = "identity", fill = "dodgerblue3") +
  xlab("EU Member States") +
  ylab("EUR Billon") + 
  theme(axis.text.x = element_text(angle = 90) ) +
  scale_y_continuous(expand = c(0,0),  breaks = seq(0, 1300, by = 100), limits=c(0,1300)) +
  ggtitle("Exports of goods to other Member States") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=18))
 
 
 ######################################################################
 ### INSIGHT 2: Plot showing exports of goods to intra vs. extra EU ####
 ######################################################################
 
 ### Plot 2
 ggplot(agg_extra.long, aes(x=clean_names, y=value, fill=variable)) +
   geom_col(position = "dodge")+
   theme(axis.text.x = element_text(angle = 90) ) +
   scale_fill_manual(name="Export",
                     breaks=c("share_intra", "share_extra"),
                     labels=c("Intra", "Extra"),
                     values=c('coral2','dodgerblue3')) +
   theme(
     panel.grid.major.x = element_blank(),
     panel.grid.minor.x = element_blank(),
     panel.grid.major.y = element_line(color="lightgrey"),
     axis.ticks.y = element_blank(),
     axis.ticks.length.x.bottom = unit(1, "mm"),
     panel.background = element_blank()
   ) +
   xlab("EU Member States") +
   ylab("Share %") + 
   scale_y_continuous(expand = c(0,0), breaks = seq(0, 90, by = 10), limits=c(0,90)) +
   ggtitle("Exports of goods, intra EU and extra EU") +
   theme(plot.title = element_text(hjust = 0.5, face="bold", size=18)) 

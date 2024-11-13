##################################################################################
# Theme analysis - parental worry 
##################################################################################

##################################################################################
# Load data 
##################################################################################
SaSt_worry_themes_frequencies <- read_xlsx("SaSt_themes_frequencies_worry.xlsx", sheet = 3)  
# Unfortunately cannot be shared due to privacy concerns 

##################################################################################
# Load packages 
##################################################################################
library(tidyverse)
library(qgraph)

##################################################################################
# Group network 
##################################################################################
# Change into scores 0 or 1 
SaSt_worry_themes_frequencies2 <- SaSt_worry_themes_frequencies

for(i in 4:which(colnames(SaSt_worry_themes_frequencies2)=="19")){
  for(j in 1:nrow(SaSt_worry_themes_frequencies2)){
    if(is.na(SaSt_worry_themes_frequencies2[j, i])){
      SaSt_worry_themes_frequencies2[j, i] <- NA
    }else if(SaSt_worry_themes_frequencies2[j, i] > 1){
      SaSt_worry_themes_frequencies2[j, i] <- 1 
    }else{
      SaSt_worry_themes_frequencies2[j, i] <- SaSt_worry_themes_frequencies2[j, i]
    }
  }
}

# Total selected subthemes worry 
Sum_worry_themes_frequencies2 <- colSums(as.matrix(SaSt_worry_themes_frequencies2[which(colnames(SaSt_worry_themes_frequencies2)=="1"):which(colnames(SaSt_worry_themes_frequencies2)=="19")]))

# Occurence subthemes worry 
crossprod_freq_worry_themes_v2 <- crossprod(as.matrix(SaSt_worry_themes_frequencies2[which(colnames(SaSt_worry_themes_frequencies2)=="1"):which(colnames(SaSt_worry_themes_frequencies2)=="19")]))
# change diagonal 

# Plot occurrence 
# Remove diagonal 
crossprod_freq_worry_themes2_v2 <- crossprod_freq_worry_themes_v2  
crossprod_freq_worry_themes2_v2

crossprod_freq_worry_themes2_nodewidth_v2 <- diag(crossprod_freq_worry_themes2_v2) + 1  
crossprod_freq_worry_themes2_nodewidth2_v2 <- crossprod_freq_worry_themes2_nodewidth_v2/nrow(SaSt_worry_themes_frequencies2) * 50  # percentage based on total observations 
crossprod_freq_worry_themes2_nodewidth3_v2 <- crossprod_freq_worry_themes2_nodewidth_v2/nrow(SaSt_worry_themes_frequencies2)  # 
diag(crossprod_freq_worry_themes2_v2) <- rep(NA, length(diag(crossprod_freq_worry_themes2_v2)))


genworrynetfreq_v2 <- qgraph(crossprod_freq_worry_themes2_v2, 
                          layout = "spring", 
                          border.width = crossprod_freq_worry_themes2_nodewidth2_v2, 
                          posCol = "black")


# Add legend 
# Subthemes as nodeNames 
nodenamesworry <- c("Negative mood", 
                    "Screen use", 
                    "Lack of activities", 
                    "Lack of adolescent-contact", 
                    "Social/friends", 
                    "Atmosphere", 
                    "School absence", 
                    "School performance", 
                    "Substance use", 
                    "Eating habits", 
                    "Spendings", 
                    "Chores", 
                    "Physical complaints", 
                    "Continuous worry", 
                    "Parenting behavior", 
                    "Suicidality", 
                    "Future", 
                    "External event", 
                    "Other")

labelsworry <- c(11, 
                 1, 
                 2, 
                 14, 
                 3, 
                 15, 
                 4, 
                 5, 
                 6, 
                 7, 
                 8, 
                 9, 
                 12, 
                 16, 
                 17, 
                 13, 
                 18, 
                 19, 
                 10)

# Themes as group 
groupsworry <- c("Adolescent well-being",  # 1
                 "Adolescent behavior",    # 2
                 "Adolescent behavior",    # 3 
                 "Family interaction",     # 4 
                 "Adolescent behavior",    # 5 
                 "Family interaction",     # 6 
                 "Adolescent behavior",    # 7 
                 "Adolescent behavior",    # 8 
                 "Adolescent behavior",    # 9 
                 "Adolescent behavior",    # 10 
                 "Adolescent behavior",    # 11 
                 "Adolescent behavior",    # 12 
                 "Adolescent well-being",  # 13 
                 "Parent",                 # 14 
                 "Parent",                 # 15 
                 "Adolescent well-being",  # 16 
                 "Future",                 # 17 
                 "External",               # 18 
                 "Adolescent behavior")    # 19 

# #F0C9C5 family interaction 
# #FF8662 well being 
# #D4E9D8 adolescent behavior 
# #F6D739 future 
# #CBDBA7 parent 
# #789342 external 

bordercolorworrytheme <- c("#E26E3F", "#F1DBBD", "#E2A144", "#B8C6E4", "#385492", "#789342") 

bordercolorworrytheme <- c("#F0C9C5", "#FF8662", "#D4E9D8", "#F6D739", "#CBDBA7", "#789342") 

bordercolorworrytheme <- c("#FFDB6D", "#C4961A", "#D16103", "#52854C", "#4E84C4", "#293352")


bordercolorworry <- c("#FF8662",  # 1 
                      "#D4E9D8",  # 2 
                      "#D4E9D8",  # 3 
                      "#F0C9C5",  # 4 
                      "#D4E9D8",  # 5 
                      "#F0C9C5",  # 6 
                      "#D4E9D8",  # 7 
                      "#D4E9D8",  # 8 
                      "#D4E9D8",  # 9 
                      "#D4E9D8",  # 10 
                      "#D4E9D8",  # 11 
                      "#D4E9D8",  # 12 
                      "#FF8662",  # 13 
                      "#CBDBA7",  # 14 
                      "#CBDBA7",  # 15 
                      "#FF8662",  # 16 
                      "#F6D739",  # 17 
                      "#789342",  # 18 
                      "#D4E9D8")  # 19

bordercolorworry2 <- c(bordercolorworrytheme[2],  # 1 
                       bordercolorworrytheme[3],  # 2 
                       bordercolorworrytheme[3],  # 3 
                       bordercolorworrytheme[1],  # 4 
                       bordercolorworrytheme[3],  # 5 
                       bordercolorworrytheme[1],  # 6
                       bordercolorworrytheme[3],  # 7 
                       bordercolorworrytheme[3],  # 8 
                       bordercolorworrytheme[3],  # 9, 
                       bordercolorworrytheme[3],  # 10, 
                       bordercolorworrytheme[3],  # 11, 
                       bordercolorworrytheme[3],  # 12 
                       bordercolorworrytheme[2],  # 13 
                       bordercolorworrytheme[5],  # 14 
                       bordercolorworrytheme[5],  # 15 
                       bordercolorworrytheme[2],  # 16 
                       bordercolorworrytheme[4],  # 17 
                       bordercolorworrytheme[6],  # 18 
                       bordercolorworrytheme[3])  # 19 

groupcolorworry <- c(bordercolorworrytheme[3], 
                     bordercolorworrytheme[2], 
                     bordercolorworrytheme[6], 
                     bordercolorworrytheme[1], 
                     bordercolorworrytheme[4], 
                     bordercolorworrytheme[5])


qgraph(crossprod_freq_worry_themes2_v2, 
       layout = "spring", 
       border.width = crossprod_freq_worry_themes2_nodewidth2_v2, 
       posCol = "black", 
       title = "B", 
       # color = "white", 
       groups = groupsworry,   # corresponding with main themes 
       nodeNames = nodenamesworry,  # corresponding with subthemes 
       color = rep("white", 6),   # for each group 
       border.color = bordercolorworry,
       legend = TRUE, 
       legend.cex = 0.25)

# End figure 
net_worry_theme_group <- qgraph(crossprod_freq_worry_themes2_v2, 
       layout = "spring", 
       pie = crossprod_freq_worry_themes2_nodewidth3_v2, 
       posCol = "black", 
       title = "B", 
       groups = groupsworry,   # corresponding with main themes 
       nodeNames = nodenamesworry,  # corresponding with subthemes 
       color = rep("white", 6),   # for each group 
       border.color = bordercolorworry2,
       pieColor = bordercolorworry2, 
       legend = TRUE, 
       legend.cex = 0.25, 
       repulsion = 0.85, 
       labels = labelsworry)

# For legend 
qgraph(crossprod_freq_worry_themes2_v2, 
       layout = "spring", 
       pie = crossprod_freq_worry_themes2_nodewidth3_v2, 
       posCol = "black", 
       title = "B", 
       groups = groupsworry,   # corresponding with main themes 
       nodeNames = nodenamesworry,  # corresponding with subthemes 
       # color = rep("white", 6),   # for each group 
       border.color = bordercolorworry2,
       pieColor = bordercolorworry2, 
       color = groupcolorworry, 
       legend = TRUE, 
       legend.cex = 0.25, 
       repulsion = 0.85, 
       labels = labelsworry)

# export as 6 x 9  


# Table 
# bar plot frequencies 
crossprod_freq_worry_themes2_nodewidth_v4 <- crossprod_freq_worry_themes2_nodewidth_v2 - 1   # so accurate numbers (added + 1 for network vis before)

freqnodesbarplotdata_v2 <- data.frame(nodes = c(1:19), 
                                   value = crossprod_freq_worry_themes2_nodewidth_v4, 
                                   color = bordercolorworry2, 
                                   labels = labelsworry)


# Order x axis by theme 
freqnodesbarplotdata_v2$group <- as.numeric(as.factor(freqnodesbarplotdata_v2$color))
freqnodesbarplotdata_v2$nodes2 <- order(freqnodesbarplotdata_v2$group, freqnodesbarplotdata_v2$nodes)

freqnodesbarplotdata2_v2 <- freqnodesbarplotdata_v2[order(freqnodesbarplotdata_v2$group, freqnodesbarplotdata_v2$nodes),]

freqnodesbarplotdata2_v2$nodes2 <- 1:19

# horizontal 
ggplot(freqnodesbarplotdata2_v2, aes(x = nodes2, y = value, color = as.factor(nodes), fill = as.factor(nodes), group = as.factor(nodes), level = as.numeric(as.factor(color)))) + 
  geom_col(position = "dodge", alpha = 0.8) + 
  theme_apa() + 
  scale_fill_manual(values = bordercolorworry2) + 
  scale_color_manual(values = bordercolorworry2) + 
  xlab("Subthemes") + 
  ylab("% selected") + 
  theme(legend.position = "none", 
        legend.title = element_blank()) + 
  scale_x_continuous(breaks = 1:19, labels = freqnodesbarplotdata2_v2$nodes) + 
  scale_y_continuous(n.breaks = 20) + 
  coord_flip() + 
  theme(axis.title.y = element_blank()) + 
  labs(title = "A", subtitle = "Subthemes")


# Or vertical 
ggplot(freqnodesbarplotdata2_v2, aes(x = nodes2, y = value, color = as.factor(nodes), fill = as.factor(nodes), group = as.factor(nodes), level = as.numeric(as.factor(color)))) + 
  geom_col(position = "dodge", alpha = 0.8) + 
  theme_apa() + 
  scale_fill_manual(values = bordercolorworry2) + 
  scale_color_manual(values = bordercolorworry2) + 
  xlab("Subthemes") + 
  ylab("Count") + 
  theme(legend.position = "none", 
        legend.title = element_blank()) + 
  scale_x_continuous(breaks = 1:19, labels = freqnodesbarplotdata2_v2$nodes) + 
  scale_y_continuous(n.breaks = 20) + 
  labs(title = "A")


freqnodesbarplotdata2_v2$group2 <- c("", "", "", "Adolescent \nbehavior", "", "", "", "", 
                                     "", "", 
                                     "Adolescent \nwell-being", "",  "", 
                                     "Family \ninteraction", "", 
                                     "Parent", "", 
                                     "Future", 
                                     "External")
  

# rotated labels  x-axis - used 
ggplot(freqnodesbarplotdata2_v2, aes(x = interaction(group, labels), y = value, color = as.factor(nodes), fill = as.factor(nodes), group = as.factor(nodes), level = as.numeric(as.factor(color)))) + 
  geom_col(position = "dodge", alpha = 0.8) + 
  theme_apa() + 
  scale_fill_manual(values = bordercolorworry2) + 
  scale_color_manual(values = bordercolorworry2) + 
  xlab("Subthemes") + 
  ylab("Count") + 
  theme(legend.position = "none", 
        legend.title = element_blank()) + 
  # scale_x_continuous(breaks = 1:19) + 
  scale_y_continuous(n.breaks = 20) + 
  labs(title = "A") + 
  annotate(geom = "text", x = 1:19, y = -11, label = 1:19, size = 5) +
  annotate(geom = "text", x = 1:19, y = -22, label = freqnodesbarplotdata2_v2$group2, size = 3, angle = 30) +
  coord_cartesian(xlim = c(-0.1, 20), ylim = c(-5, 145), expand = FALSE, clip = "off") + 
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        axis.text.y = element_text(size = 15, color = "black"), 
        axis.title.y = element_text(size = 16)) 
  

# horizontal labels 
ggplot(freqnodesbarplotdata2_v2, aes(x = interaction(group, labels), y = value, color = as.factor(nodes), fill = as.factor(nodes), group = as.factor(nodes), level = as.numeric(as.factor(color)))) + 
  geom_col(position = "dodge", alpha = 0.8) + 
  theme_apa() + 
  scale_fill_manual(values = bordercolorworry2) + 
  scale_color_manual(values = bordercolorworry2) + 
  xlab("Subthemes") + 
  ylab("Count") + 
  theme(legend.position = "none", 
        legend.title = element_blank()) + 
  # scale_x_continuous(breaks = 1:19) + 
  scale_y_continuous(n.breaks = 20) + 
  labs(title = "A") + 
  annotate(geom = "text", x = 1:19, y = -15, label = 1:19, size = 4) +
  annotate(geom = "text", x = 1:19, y = -30, label = freqnodesbarplotdata2_v2$group2, size = 2) +
  coord_cartesian(xlim = c(-0.1, 20), ylim = c(-5, 145), expand = FALSE, clip = "off") + 
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# export 7 x 9 

##################################################################################
# Total selected subthemes worry per parent 
SaSt_worry_themes_frequencies_sub <- SaSt_worry_themes_frequencies2[c(which(colnames(SaSt_worry_themes_frequencies2)=="PPN"), which(colnames(SaSt_worry_themes_frequencies2)=="1"):which(colnames(SaSt_worry_themes_frequencies2)=="19"))]

Sum_worry_themes_freq_parent <- aggregate(. ~ PPN, SaSt_worry_themes_frequencies_sub, sum)

Sum_worry_themes_freq_parent2 <- Sum_worry_themes_freq_parent
colnames(Sum_worry_themes_freq_parent2) <- c(colnames(Sum_worry_themes_freq_parent2)[1], labelsworry)  # change labels into same as plots 

Sum_worry_themes_freq_parent2_t <- colSums(Sum_worry_themes_freq_parent2[,-1])

# Adolescent behavior 
sum(Sum_worry_themes_freq_parent2_t[c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")])

min(Sum_worry_themes_freq_parent2["2"])  # undertaking activities 
max(Sum_worry_themes_freq_parent2["2"])
sum(Sum_worry_themes_freq_parent2["2"])

min(Sum_worry_themes_freq_parent2["4"])  # school absence  
max(Sum_worry_themes_freq_parent2["4"])
sum(Sum_worry_themes_freq_parent2["4"])

sum(Sum_worry_themes_freq_parent2["5"])  # school performance 

sum(Sum_worry_themes_freq_parent2["7"])  # eating habbits 
sum(Sum_worry_themes_freq_parent2["7"]>0) 
min(Sum_worry_themes_freq_parent2["7"])
max(Sum_worry_themes_freq_parent2["7"])

sum(Sum_worry_themes_freq_parent2["1"])  # screen use 
sum(Sum_worry_themes_freq_parent2["1"]>0) 
min(Sum_worry_themes_freq_parent2["1"])
max(Sum_worry_themes_freq_parent2["1"])

# Adolescent well-being 
sum(Sum_worry_themes_freq_parent2_t[c("11", "12", "13")])

sum(Sum_worry_themes_freq_parent2["12"])  # physical complaints  
sum(Sum_worry_themes_freq_parent2["12"]>0)  # number of parents adhered 
min(Sum_worry_themes_freq_parent2["12"])
max(Sum_worry_themes_freq_parent2["12"])

sum(Sum_worry_themes_freq_parent2["11"])  # negative mood  
sum(Sum_worry_themes_freq_parent2["11"]>0)  # number of parents adhered 
min(Sum_worry_themes_freq_parent2["11"])
max(Sum_worry_themes_freq_parent2["11"])

sum(Sum_worry_themes_freq_parent2["13"])  # suicidality  
sum(Sum_worry_themes_freq_parent2["13"]>0)  # number of parents adhered 
min(Sum_worry_themes_freq_parent2["13"])
max(Sum_worry_themes_freq_parent2["13"])

# Family interaction 
sum(Sum_worry_themes_freq_parent2_t[c("14", "15")])

sum(Sum_worry_themes_freq_parent2["14"])  # lack of adolescent-contact  
sum(Sum_worry_themes_freq_parent2["14"]>0)  # number of parents adhered 
min(Sum_worry_themes_freq_parent2["14"])
max(Sum_worry_themes_freq_parent2["14"])

sum(Sum_worry_themes_freq_parent2["15"])  # atmosphere   
sum(Sum_worry_themes_freq_parent2["15"]>0)  # number of parents adhered 
min(Sum_worry_themes_freq_parent2["15"])
max(Sum_worry_themes_freq_parent2["15"])

##################################################################################
# Per example couple  
##################################################################################
# Participant numbers are replaced by letters for privacy concerns 

############################################################################################
# Parent A  
############################################################################################
freq_themes2_p62 <- SaSt_worry_themes_frequencies2[SaSt_worry_themes_frequencies2$PPN == "A", which(colnames(SaSt_worry_themes_frequencies2)=="1"):which(colnames(SaSt_worry_themes_frequencies2)=="19")]
crossprod_freq_themes_p62 <- crossprod(as.matrix(freq_themes2_p62))
crossprod_freq_themes_p62  


crossprod_freq_themes_p62_nodewidth <- diag(crossprod_freq_themes_p62) + 1  # make relative to many time points (how many times responded)? 
crossprod_freq_themes_p62_nodewidth2 <- crossprod_freq_themes_p62_nodewidth/nrow(freq_themes2_p62) * 50 
crossprod_freq_themes_p62_nodewidth3 <- crossprod_freq_themes_p62_nodewidth/nrow(freq_themes2_p62) 
diag(crossprod_freq_themes_p62) <- rep(NA, length(diag(crossprod_freq_themes_p62)))

# Network 
qgraph(crossprod_freq_themes_p62, 
       layout = net_worry_theme_group$layout, 
       pie = crossprod_freq_themes_p62_nodewidth3, 
       posCol = "black", 
       title = "B", 
       groups = groupsworry,   # corresponding with main themes 
       nodeNames = nodenamesworry,  # corresponding with subthemes 
       color = rep("white", 6),   # for each group 
       border.color = bordercolorworry2,
       pieColor = bordercolorworry2, 
       legend = TRUE, 
       legend.cex = 0.25, 
       repulsion = 0.85, 
       labels = labelsworry)

# For legend 
qgraph(crossprod_freq_themes_p62, 
       layout = net_worry_theme_group$layout, 
       pie = crossprod_freq_themes_p62_nodewidth3, 
       posCol = "black", 
       title = "B", 
       groups = groupsworry,   # corresponding with main themes 
       nodeNames = nodenamesworry,  # corresponding with subthemes 
       # color = rep("white", 6),   # for each group 
       border.color = bordercolorworry2,
       pieColor = bordercolorworry2, 
       color = groupcolorworry, 
       legend = TRUE, 
       legend.cex = 0.25, 
       repulsion = 0.85, 
       labels = labelsworry)


# Histogram 
# Table 
# bar plot frequencies 
crossprod_freq_themes_p62_nodewidth4 <- crossprod_freq_themes_p62_nodewidth - 1  # remove again so actual number 
freqnodesbarplotdata_pb <- data.frame(nodes = c(1:19), 
                                      value = crossprod_freq_themes_p62_nodewidth4, 
                                      color = bordercolorworry2, 
                                      labels = labelsworry)


# Order x axis by theme 
freqnodesbarplotdata_pb$group <- as.numeric(as.factor(freqnodesbarplotdata_pb$color))
freqnodesbarplotdata_pb$nodes2 <- order(freqnodesbarplotdata_pb$group, freqnodesbarplotdata_pb$nodes)

freqnodesbarplotdata2_pb <- freqnodesbarplotdata_v2[order(freqnodesbarplotdata_pb$group, freqnodesbarplotdata_pb$nodes),]

freqnodesbarplotdata2_pb$nodes2 <- 1:19

freqnodesbarplotdata2_pb$group2 <- c("", "", "", "Adolescent \nbehavior", "", "", "", "", 
                                     "", "", 
                                     "Adolescent \nwell-being", "",  "", 
                                     "Family \ninteraction", "", 
                                     "Parent", "", 
                                     "Future", 
                                     "External")

ggplot(freqnodesbarplotdata_pb, aes(x = interaction(group, labels), y = value, color = as.factor(nodes), fill = as.factor(nodes), group = as.factor(nodes), level = as.numeric(as.factor(color)))) + 
  geom_col(position = "dodge", alpha = 0.8) + 
  theme_apa() + 
  scale_fill_manual(values = bordercolorworry2) + 
  scale_color_manual(values = bordercolorworry2) + 
  xlab("Subthemes") + 
  ylab("Count") + 
  theme(legend.position = "none", 
        legend.title = element_blank()) + 
  # scale_x_continuous(breaks = 1:19) + 
  scale_y_continuous(n.breaks = 10) + 
  labs(title = "A") + 
  annotate(geom = "text", x = 1:19, y = -2, label = 1:19, size = 5) +
  annotate(geom = "text", x = 1:19, y = -4, label = freqnodesbarplotdata2_pb$group2, size = 3, angle = 30) +
  coord_cartesian(xlim = c(-0.1, 20), ylim = c(-1, 30), expand = FALSE, clip = "off") + 
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        axis.text.y = element_text(size = 15, color = "black"), 
        axis.title.y = element_text(size = 16)) 


############################################################################################
# Parent B 
############################################################################################
freq_themes2_p61 <- SaSt_worry_themes_frequencies2[SaSt_worry_themes_frequencies2$PPN == "B", which(colnames(SaSt_worry_themes_frequencies2)=="1"):which(colnames(SaSt_worry_themes_frequencies2)=="19")]
crossprod_freq_themes_p61 <- crossprod(as.matrix(freq_themes2_p61))
crossprod_freq_themes_p61   


crossprod_freq_themes_p61_nodewidth <- diag(crossprod_freq_themes_p61) + 1   # so they show up in network 
crossprod_freq_themes_p61_nodewidth2 <- crossprod_freq_themes_p61_nodewidth/nrow(freq_themes2_p61) * 50 
crossprod_freq_themes_p61_nodewidth3 <- crossprod_freq_themes_p61_nodewidth/nrow(freq_themes2_p61) 
diag(crossprod_freq_themes_p61) <- rep(NA, length(diag(crossprod_freq_themes_p61)))

# Network 
qgraph(crossprod_freq_themes_p61, 
       layout = net_worry_theme_group$layout, 
       pie = crossprod_freq_themes_p61_nodewidth3, 
       posCol = "black", 
       title = "B", 
       groups = groupsworry,   # corresponding with main themes 
       nodeNames = nodenamesworry,  # corresponding with subthemes 
       color = rep("white", 6),   # for each group 
       border.color = bordercolorworry2,
       pieColor = bordercolorworry2, 
       legend = TRUE, 
       legend.cex = 0.25, 
       repulsion = 0.85, 
       labels = labelsworry)

# For legend 
qgraph(crossprod_freq_themes_p61, 
       layout = net_worry_theme_group$layout, 
       pie = crossprod_freq_themes_p61_nodewidth3, 
       posCol = "black", 
       title = "B", 
       groups = groupsworry,   # corresponding with main themes 
       nodeNames = nodenamesworry,  # corresponding with subthemes 
       # color = rep("white", 6),   # for each group 
       border.color = bordercolorworry2,
       pieColor = bordercolorworry2, 
       color = groupcolorworry, 
       legend = TRUE, 
       legend.cex = 0.25, 
       repulsion = 0.85, 
       labels = labelsworry)

# Histogram 
# Table 
# bar plot frequencies 
crossprod_freq_themes_p61_nodewidth4 <- crossprod_freq_themes_p61_nodewidth - 1  # remove again so actual number 
freqnodesbarplotdata_pa <- data.frame(nodes = c(1:19), 
                                      value = crossprod_freq_themes_p61_nodewidth4, 
                                      color = bordercolorworry2, 
                                      labels = labelsworry)


# Order x axis by theme 
freqnodesbarplotdata_pa$group <- as.numeric(as.factor(freqnodesbarplotdata_pa$color))
freqnodesbarplotdata_pa$nodes2 <- order(freqnodesbarplotdata_pa$group, freqnodesbarplotdata_pa$nodes)

freqnodesbarplotdata2_pa <- freqnodesbarplotdata_v2[order(freqnodesbarplotdata_pa$group, freqnodesbarplotdata_pa$nodes),]

freqnodesbarplotdata2_pa$nodes2 <- 1:19

freqnodesbarplotdata2_pa$group2 <- c("", "", "", "Adolescent \nbehavior", "", "", "", "", 
                                     "", "", 
                                     "Adolescent \nwell-being", "",  "", 
                                     "Family \ninteraction", "", 
                                     "Parent", "", 
                                     "Future", 
                                     "External")

ggplot(freqnodesbarplotdata_pa, aes(x = interaction(group, labels), y = value, color = as.factor(nodes), fill = as.factor(nodes), group = as.factor(nodes), level = as.numeric(as.factor(color)))) + 
  geom_col(position = "dodge", alpha = 0.8) + 
  theme_apa() + 
  scale_fill_manual(values = bordercolorworry2) + 
  scale_color_manual(values = bordercolorworry2) + 
  xlab("Subthemes") + 
  ylab("Count") + 
  theme(legend.position = "none", 
        legend.title = element_blank()) + 
  # scale_x_continuous(breaks = 1:19) + 
  scale_y_continuous(n.breaks = 10) + 
  labs(title = "A") + 
  annotate(geom = "text", x = 1:19, y = -2, label = 1:19, size = 5) +
  annotate(geom = "text", x = 1:19, y = -4, label = freqnodesbarplotdata2_pa$group2, size = 3, angle = 30) +
  coord_cartesian(xlim = c(-0.1, 20), ylim = c(-1, 30), expand = FALSE, clip = "off") + 
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        axis.text.y = element_text(size = 15, color = "black"), 
        axis.title.y = element_text(size = 16)) 


############################################################################################






##################################################################################
# Theme analysis - parental self-efficacy (insecure) 
##################################################################################

##################################################################################
# Load data 
##################################################################################
SaSt_insecure_themes_frequencies <- read_xlsx("SaSt_themes_frequencies_insecure.xlsx", sheet = 3)
# Unfortunately, data cannot be shared due to privacy concerns  

##################################################################################
# Load packages 
##################################################################################


##################################################################################
# Group network 
##################################################################################
# Change into scores 0 or 1 
SaSt_insecure_themes_frequencies2 <- SaSt_insecure_themes_frequencies

for(i in 4:which(colnames(SaSt_insecure_themes_frequencies2)=="11")){
  for(j in 1:nrow(SaSt_insecure_themes_frequencies2)){
    if(is.na(SaSt_insecure_themes_frequencies2[j, i])){
      SaSt_insecure_themes_frequencies2[j, i] <- NA
    }else if(SaSt_insecure_themes_frequencies2[j, i] > 1){
      SaSt_insecure_themes_frequencies2[j, i] <- 1 
    }else{
      SaSt_insecure_themes_frequencies2[j, i] <- SaSt_insecure_themes_frequencies2[j, i]
    }
  }
}

# Total selected subthemes  
Sum_insecure_themes_frequencies2 <- colSums(as.matrix(SaSt_insecure_themes_frequencies2[which(colnames(SaSt_insecure_themes_frequencies2)=="1"):which(colnames(SaSt_insecure_themes_frequencies2)=="11")]))

# Occurence subthemes worry 
crossprod_freq_insecure_themes_v2 <- crossprod(as.matrix(SaSt_insecure_themes_frequencies2[which(colnames(SaSt_insecure_themes_frequencies2)=="1"):which(colnames(SaSt_insecure_themes_frequencies2)=="11")]))
# change diagonal 

# Plot occurrence 
# Remove diagonal 
crossprod_freq_insecure_themes2_v2 <- crossprod_freq_insecure_themes_v2  

crossprod_freq_insecure_themes2_nodewidth_v2 <- diag(crossprod_freq_insecure_themes2_v2) + 1  # make relative to many time points (how many times responded)? 
crossprod_freq_insecure_themes2_nodewidth2_v2 <- crossprod_freq_insecure_themes2_nodewidth_v2/nrow(SaSt_insecure_themes_frequencies2) * 20   # did times 20 because it looked better than times 100 (would get % then)
crossprod_freq_insecure_themes2_nodewidth3_v2 <- crossprod_freq_insecure_themes2_nodewidth_v2/nrow(SaSt_insecure_themes_frequencies2) # 

diag(crossprod_freq_insecure_themes2_v2) <- rep(NA, length(diag(crossprod_freq_insecure_themes2_v2)))


geninsnetfreq <- qgraph(crossprod_freq_insecure_themes2_v2, 
                        layout = "spring", 
                        border.width = crossprod_freq_insecure_themes2_nodewidth3_v2, 
                        title = "A", 
                        posCol = "black")

# working here 

# Add legend 
# Subthemes as nodeNames 
nodenamesinsecure <- c("Mood",   # 1
                       "Adolescent-contact",  # 2 
                       "Atmosphere",   # 3 
                       "Lack of support",  # 4 
                       "Parenting behavior",  # 5 
                       "Substance use",  # 6 
                       "Future",  # 7 
                       "Mood",  # 8
                       "Continuous insecurity",  # 9 
                       "Other",   # 10
                       "Physical complaints")  # 11

labelsinsecure <- c(2,  
                    4, 
                    5, 
                    6, 
                    7, 
                    1, 
                    10, 
                    8, 
                    9, 
                    11, 
                    3)

# Themes as group 
groupsinsecure <- c("Adolescent well-being",  # 1
                 "Family interaction",    # 2
                 "Family interaction",    # 3 
                 "Family interaction",     # 4 
                 "Parent",    # 5 
                 "Adolescent behavior",     # 6 
                 "Future",    # 7 
                 "Parent",    # 8 
                 "Parent",    # 9 
                 "External",    # 10 
                 "Adolescent well-being")    # 11 

# #F0C9C5 family interaction 
# #FF8662 well being 
# #D4E9D8 adolescent behavior 
# #F6D739 future 
# #CBDBA7 parent 
# #789342 external 

bordercolorworrytheme <- c("#FFDB6D", "#C4961A", "#D16103", "#52854C", "#4E84C4", "#293352")

groupcolorinsecure <- c(bordercolorworrytheme[3], 
                     bordercolorworrytheme[2], 
                     bordercolorworrytheme[6], 
                     bordercolorworrytheme[1], 
                     bordercolorworrytheme[4], 
                     bordercolorworrytheme[5])

bordercolorinsecure2 <- c(bordercolorworrytheme[2],  # 1 
                       bordercolorworrytheme[1],  # 2 
                       bordercolorworrytheme[1],  # 3 
                       bordercolorworrytheme[1],  # 4 
                       bordercolorworrytheme[5],  # 5 
                       bordercolorworrytheme[3],  # 6
                       bordercolorworrytheme[4],  # 7 
                       bordercolorworrytheme[5],  # 8 
                       bordercolorworrytheme[5],  # 9, 
                       bordercolorworrytheme[6],  # 10, 
                       bordercolorworrytheme[2])  # 11 



# End figure 
netinsthemes <- qgraph(crossprod_freq_insecure_themes2_v2, 
       layout = "spring", 
       pie = crossprod_freq_insecure_themes2_nodewidth3_v2, 
       posCol = "black", 
       title = "B", 
       groups = groupsinsecure,   # corresponding with main themes 
       nodeNames = nodenamesinsecure,  # corresponding with subthemes 
       color = rep("white", 6),   # for each group 
       border.color = bordercolorinsecure2,
       pieColor = bordercolorinsecure2, 
       legend = TRUE, 
       legend.cex = 0.25, 
       repulsion = 0.85 , 
       labels = labelsinsecure)

# For legend 
qgraph(crossprod_freq_insecure_themes2_v2, 
       layout = "spring", 
       pie = crossprod_freq_insecure_themes2_nodewidth3_v2, 
       posCol = "black", 
       title = "B", 
       groups = groupsinsecure,   # corresponding with main themes 
       nodeNames = nodenamesinsecure,  # corresponding with subthemes 
       # color = rep("white", 6),   # for each group 
       border.color = bordercolorinsecure2,
       pieColor = bordercolorinsecure2, 
       color = groupcolorinsecure, 
       legend = TRUE, 
       legend.cex = 0.25, 
       repulsion = 0.85, 
       labels = labelsinsecure)

# export as 6 x 9  

#######################################################################################
# Panel A: histogram  
#######################################################################################
# Plot frequencies - diagonal 
freqnodesbarplotdata_ins_v2 <- data.frame(nodes = c(1:11), 
                                      value = crossprod_freq_insecure_themes2_nodewidth_v2, 
                                      color = bordercolorinsecure2, 
                                      labels = labelsinsecure)


# Order x axis by theme 
freqnodesbarplotdata_ins_v2$group <- as.numeric(as.factor(freqnodesbarplotdata_ins_v2$color))
freqnodesbarplotdata_ins_v2$nodes2 <- order(freqnodesbarplotdata_ins_v2$group, freqnodesbarplotdata_ins_v2$nodes)

freqnodesbarplotdata2_ins_v2 <- freqnodesbarplotdata_ins_v2[order(freqnodesbarplotdata_ins_v2$group, freqnodesbarplotdata_ins_v2$nodes),]

freqnodesbarplotdata2_ins_v2$nodes2 <- 1:11

# horizontal 
ggplot(freqnodesbarplotdata2_ins_v2, aes(x = nodes2, y = value, color = as.factor(nodes), fill = as.factor(nodes), group = as.factor(nodes), level = as.numeric(as.factor(color)))) + 
  geom_col(position = "dodge", alpha = 0.8) + 
  theme_apa() + 
  scale_fill_manual(values = bordercolorinsecure2) + 
  scale_color_manual(values = bordercolorinsecure2) + 
  xlab("Subthemes") + 
  ylab("% selected") + 
  theme(legend.position = "none", 
        legend.title = element_blank()) + 
  scale_x_continuous(breaks = 1:11, labels = freqnodesbarplotdata2_ins_v2$nodes) + 
  scale_y_continuous(n.breaks = 20) + 
  coord_flip() + 
  theme(axis.title.y = element_blank()) + 
  labs(title = "A", subtitle = "Subthemes")


# Or vertical 
ggplot(freqnodesbarplotdata2_ins_v2, aes(x = nodes2, y = value, color = as.factor(nodes), fill = as.factor(nodes), group = as.factor(nodes), level = as.numeric(as.factor(color)))) + 
  geom_col(position = "dodge", alpha = 0.8) + 
  theme_apa() + 
  scale_fill_manual(values = bordercolorinsecure2) + 
  scale_color_manual(values = bordercolorinsecure2) + 
  xlab("Subthemes") + 
  ylab("Count") + 
  theme(legend.position = "none", 
        legend.title = element_blank()) + 
  scale_x_continuous(breaks = 1:11, labels = freqnodesbarplotdata2_ins_v2$nodes) + 
  scale_y_continuous(n.breaks = 20) + 
  labs(title = "A")


freqnodesbarplotdata2_ins_v2$group2 <- c( "Adolescent \nbehavior", 
                                     "Adolescent \nwell-being",   "", 
                                     " ", "Family \ninteraction", "", "", 
                                     "Parent", "", 
                                     "Future", 
                                     "External")


# rotated labels  x-axis - used 
ggplot(freqnodesbarplotdata2_ins_v2, aes(x = interaction(group, labels), y = value, color = as.factor(nodes), fill = as.factor(nodes), group = as.factor(nodes), level = as.numeric(as.factor(color)))) + 
  geom_col(position = "dodge", alpha = 0.8) + 
  theme_apa() + 
  scale_fill_manual(values = bordercolorinsecure2) + 
  scale_color_manual(values = bordercolorinsecure2) + 
  xlab("Subthemes") + 
  ylab("Count") + 
  theme(legend.position = "none", 
        legend.title = element_blank()) + 
  # scale_x_continuous(breaks = 1:19) + 
  scale_y_continuous(n.breaks = 20) + 
  labs(title = "A") + 
  annotate(geom = "text", x = 1:11, y = -11, label = 1:11, size = 5) +
  annotate(geom = "text", x = 1:11, y = -20, label = freqnodesbarplotdata2_ins_v2$group2, size = 3, angle = 30) +
  coord_cartesian(xlim = c(-0.1, 12), ylim = c(-5, 145), expand = FALSE, clip = "off") + 
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        axis.text.y = element_text(size = 15, color = "black"), 
        axis.title.y = element_text(size = 16)) 


# horizontal labels 
ggplot(freqnodesbarplotdata2_ins_v2, aes(x = interaction(group, labels), y = value, color = as.factor(nodes), fill = as.factor(nodes), group = as.factor(nodes), level = as.numeric(as.factor(color)))) + 
  geom_col(position = "dodge", alpha = 0.8) + 
  theme_apa() + 
  scale_fill_manual(values = bordercolorinsecure2) + 
  scale_color_manual(values = bordercolorinsecure2) + 
  xlab("Subthemes") + 
  ylab("Count") + 
  theme(legend.position = "none", 
        legend.title = element_blank()) + 
  # scale_x_continuous(breaks = 1:19) + 
  scale_y_continuous(n.breaks = 20) + 
  labs(title = "A") + 
  annotate(geom = "text", x = 1:11, y = -15, label = 1:11, size = 4) +
  annotate(geom = "text", x = 1:11, y = -30, label = freqnodesbarplotdata2_ins_v2$group2, size = 2) +
  coord_cartesian(xlim = c(-0.1, 12), ylim = c(-5, 145), expand = FALSE, clip = "off") + 
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# export 7 x 9 

#######################################################################################
# Total selected subthemes worry per parent 
SaSt_insecure_themes_frequencies_sub <- SaSt_insecure_themes_frequencies2[c(which(colnames(SaSt_insecure_themes_frequencies2)=="PPN"), which(colnames(SaSt_insecure_themes_frequencies2)=="1"):which(colnames(SaSt_insecure_themes_frequencies2)=="11"))]

Sum_insecure_themes_freq_parent <- aggregate(. ~ PPN, SaSt_insecure_themes_frequencies_sub, sum)

Sum_insecure_themes_freq_parent2 <- Sum_insecure_themes_freq_parent 
colnames(Sum_insecure_themes_freq_parent2) <- c(colnames(Sum_insecure_themes_freq_parent2)[1], labelsinsecure)  # change labels into same as plots 

Sum_insecure_themes_freq_parent2_t <- colSums(Sum_insecure_themes_freq_parent2[,-1])


# Adolescent well-being 
sum(Sum_insecure_themes_freq_parent2_t[c("2", "3")])

sum(Sum_insecure_themes_freq_parent2["2"])  # atmosphere   
sum(Sum_insecure_themes_freq_parent2["2"]>0)  # number of parents adhered 
min(Sum_insecure_themes_freq_parent2["2"])
max(Sum_insecure_themes_freq_parent2["2"])

# Family interaction
sum(Sum_insecure_themes_freq_parent2_t[c("4", "5", "6")])

sum(Sum_insecure_themes_freq_parent2["4"])  # contact  
sum(Sum_insecure_themes_freq_parent2["4"]>0)  # number of parents adhered 
min(Sum_insecure_themes_freq_parent2["4"])
max(Sum_insecure_themes_freq_parent2["4"])

sum(Sum_insecure_themes_freq_parent2["5"])  # atmosphere   
sum(Sum_insecure_themes_freq_parent2["5"]>0)  # number of parents adhered 
min(Sum_insecure_themes_freq_parent2["5"])
max(Sum_insecure_themes_freq_parent2["5"])

sum(Sum_insecure_themes_freq_parent2["6"])  # lack of support   
sum(Sum_insecure_themes_freq_parent2["6"]>0)  # number of parents adhered 
min(Sum_insecure_themes_freq_parent2["6"])
max(Sum_insecure_themes_freq_parent2["6"])

# Parent 
sum(Sum_insecure_themes_freq_parent2_t[c("7", "8", "9")])

sum(Sum_insecure_themes_freq_parent2["7"])  # parenting behavior    
sum(Sum_insecure_themes_freq_parent2["7"]>0)  # number of parents adhered 
min(Sum_insecure_themes_freq_parent2["7"])
max(Sum_insecure_themes_freq_parent2["7"])

sum(Sum_insecure_themes_freq_parent2["8"])  # mood    
sum(Sum_insecure_themes_freq_parent2["8"]>0)  # number of parents adhered 
min(Sum_insecure_themes_freq_parent2["8"])
max(Sum_insecure_themes_freq_parent2["8"])

sum(Sum_insecure_themes_freq_parent2["9"])  # continuous insecurity    
sum(Sum_insecure_themes_freq_parent2["9"]>0)  # number of parents adhered 
min(Sum_insecure_themes_freq_parent2["9"])
max(Sum_insecure_themes_freq_parent2["9"])

# Future 
sum(Sum_insecure_themes_freq_parent2["10"])  # parenting behavior    
sum(Sum_insecure_themes_freq_parent2["10"]>0)  # number of parents adhered 
min(Sum_insecure_themes_freq_parent2["10"])
max(Sum_insecure_themes_freq_parent2["10"])


#######################################################################################
# Example of couple  
#######################################################################################
# Due to privacy reasons, the PPN are adjusted into letters 

##############################################################################################
# Parent A  
##############################################################################################
freq_themes2_p62_ins <- SaSt_insecure_themes_frequencies2[SaSt_insecure_themes_frequencies2$PPN == "A", which(colnames(SaSt_insecure_themes_frequencies2)=="1"):which(colnames(SaSt_insecure_themes_frequencies2)=="11")]
crossprod_freq_themes_p62_ins <- crossprod(as.matrix(freq_themes2_p62_ins))
crossprod_freq_themes_p62_ins 


crossprod_freq_themes_p62_nodewidth_ins <- diag(crossprod_freq_themes_p62_ins) + 1  # make relative to many time points (how many times responded)? 
crossprod_freq_themes_p62_nodewidth2_ins <- crossprod_freq_themes_p62_nodewidth_ins/nrow(freq_themes2_p62_ins) * 20 
crossprod_freq_themes_p62_nodewidth2_ins2 <- crossprod_freq_themes_p62_nodewidth_ins/nrow(freq_themes2_p62_ins) 
diag(crossprod_freq_themes_p62_ins) <- rep(NA, length(diag(crossprod_freq_themes_p62_ins)))

# Network 
qgraph(crossprod_freq_themes_p62_ins, 
       layout = netinsthemes$layout, 
       pie = crossprod_freq_themes_p62_nodewidth2_ins2, 
       posCol = "black", 
       title = "B", 
       groups = groupsinsecure,   # corresponding with main themes 
       nodeNames = nodenamesinsecure,  # corresponding with subthemes 
       color = rep("white", 6),   # for each group 
       border.color = bordercolorinsecure2,
       pieColor = bordercolorinsecure2, 
       legend = TRUE, 
       legend.cex = 0.25, 
       repulsion = 0.85 , 
       labels = labelsinsecure)

# Histogram 
# Plot frequencies - diagonal 
crossprod_freq_themes_p62_nodewidth2_ins3 <- crossprod_freq_themes_p62_nodewidth_ins - 1
freqnodesbarplotdata_ins_pb <- data.frame(nodes = c(1:11), 
                                          value = crossprod_freq_themes_p62_nodewidth2_ins3, 
                                          color = bordercolorinsecure2, 
                                          labels = labelsinsecure)


# Order x axis by theme 
freqnodesbarplotdata_ins_pb$group <- as.numeric(as.factor(freqnodesbarplotdata_ins_pb$color))
freqnodesbarplotdata_ins_pb$nodes2 <- order(freqnodesbarplotdata_ins_pb$group, freqnodesbarplotdata_ins_pb$nodes)

freqnodesbarplotdata_ins_pb <- freqnodesbarplotdata_ins_pb[order(freqnodesbarplotdata_ins_pb$group, freqnodesbarplotdata_ins_pb$nodes),]

freqnodesbarplotdata_ins_pb$nodes2 <- 1:11
freqnodesbarplotdata_ins_pb$group2 <- c( "Adolescent \nbehavior", 
                                         "Adolescent \nwell-being",   "", 
                                         " ", "Family \ninteraction", "", "", 
                                         "Parent", "", 
                                         "Future", 
                                         "External")

ggplot(freqnodesbarplotdata_ins_pb, aes(x = interaction(group, labels), y = value, color = as.factor(nodes), fill = as.factor(nodes), group = as.factor(nodes), level = as.numeric(as.factor(color)))) + 
  geom_col(position = "dodge", alpha = 0.8) + 
  theme_apa() + 
  scale_fill_manual(values = bordercolorinsecure2) + 
  scale_color_manual(values = bordercolorinsecure2) + 
  xlab("Subthemes") + 
  ylab("Count") + 
  theme(legend.position = "none", 
        legend.title = element_blank()) + 
  # scale_x_continuous(breaks = 1:19) + 
  scale_y_continuous(n.breaks = 10) + 
  labs(title = "A") + 
  annotate(geom = "text", x = 1:11, y = -2, label = 1:11, size = 5) +
  annotate(geom = "text", x = 1:11, y = -4, label = freqnodesbarplotdata_ins_pb$group2, size = 3, angle = 30) +
  coord_cartesian(xlim = c(-0.1, 12), ylim = c(-1, 30), expand = FALSE, clip = "off") + 
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        axis.text.y = element_text(size = 15, color = "black"), 
        axis.title.y = element_text(size = 16)) 


#######################################################################################
# Parent B 
#######################################################################################
freq_themes2_p61_ins <- SaSt_insecure_themes_frequencies2[SaSt_insecure_themes_frequencies2$PPN == "B", which(colnames(SaSt_insecure_themes_frequencies2)=="1"):which(colnames(SaSt_insecure_themes_frequencies2)=="11")]
crossprod_freq_themes_p61_ins <- crossprod(as.matrix(freq_themes2_p61_ins))
crossprod_freq_themes_p61_ins  # if never selected (if rowsum = 0), remove? 


crossprod_freq_themes_p61_nodewidth_ins <- diag(crossprod_freq_themes_p61_ins) + 1  # make relative to many time points (how many times responded)? 
crossprod_freq_themes_p61_nodewidth2_ins <- crossprod_freq_themes_p61_nodewidth_ins/nrow(freq_themes2_p61_ins) * 20 
crossprod_freq_themes_p61_nodewidth2_ins2 <- crossprod_freq_themes_p61_nodewidth_ins/nrow(freq_themes2_p61_ins) 

diag(crossprod_freq_themes_p61_ins) <- rep(NA, length(diag(crossprod_freq_themes_p61_ins)))

# Network 
qgraph(crossprod_freq_themes_p61_ins, 
        layout = netinsthemes$layout, 
        pie = crossprod_freq_themes_p61_nodewidth2_ins2, 
        posCol = "black", 
        title = "B", 
        groups = groupsinsecure,   # corresponding with main themes 
        nodeNames = nodenamesinsecure,  # corresponding with subthemes 
        color = rep("white", 6),   # for each group 
        border.color = bordercolorinsecure2,
        pieColor = bordercolorinsecure2, 
        legend = TRUE, 
        legend.cex = 0.25, 
        repulsion = 0.85 , 
        labels = labelsinsecure)

# Histogram 
# Plot frequencies - diagonal 
crossprod_freq_themes_p61_nodewidth2_ins3 <- crossprod_freq_themes_p61_nodewidth_ins - 1
freqnodesbarplotdata_ins_pa <- data.frame(nodes = c(1:11), 
                                          value = crossprod_freq_themes_p61_nodewidth2_ins3, 
                                          color = bordercolorinsecure2, 
                                          labels = labelsinsecure)


# Order x axis by theme 
freqnodesbarplotdata_ins_pa$group <- as.numeric(as.factor(freqnodesbarplotdata_ins_pa$color))
freqnodesbarplotdata_ins_pa$nodes2 <- order(freqnodesbarplotdata_ins_pa$group, freqnodesbarplotdata_ins_pa$nodes)

freqnodesbarplotdata_ins_pa <- freqnodesbarplotdata_ins_pa[order(freqnodesbarplotdata_ins_pa$group, freqnodesbarplotdata_ins_pa$nodes),]

freqnodesbarplotdata_ins_pa$nodes2 <- 1:11
freqnodesbarplotdata_ins_pa$group2 <- c( "Adolescent \nbehavior", 
                                          "Adolescent \nwell-being",   "", 
                                          " ", "Family \ninteraction", "", "", 
                                          "Parent", "", 
                                          "Future", 
                                          "External")

ggplot(freqnodesbarplotdata_ins_pa, aes(x = interaction(group, labels), y = value, color = as.factor(nodes), fill = as.factor(nodes), group = as.factor(nodes), level = as.numeric(as.factor(color)))) + 
  geom_col(position = "dodge", alpha = 0.8) + 
  theme_apa() + 
  scale_fill_manual(values = bordercolorinsecure2) + 
  scale_color_manual(values = bordercolorinsecure2) + 
  xlab("Subthemes") + 
  ylab("Count") + 
  theme(legend.position = "none", 
        legend.title = element_blank()) + 
  # scale_x_continuous(breaks = 1:19) + 
  scale_y_continuous(n.breaks = 10) + 
  labs(title = "A") + 
  annotate(geom = "text", x = 1:11, y = -2, label = 1:11, size = 5) +
  annotate(geom = "text", x = 1:11, y = -4, label = freqnodesbarplotdata_ins_pa$group2, size = 3, angle = 30) +
  coord_cartesian(xlim = c(-0.1, 12), ylim = c(-1, 30), expand = FALSE, clip = "off") + 
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        axis.text.y = element_text(size = 15, color = "black"), 
        axis.title.y = element_text(size = 16)) 



################################################################################################
# Appendix B: Individual networks 
################################################################################################
# Data can unfortunately not be shared due to privacy concerns. 

# Only for two parents with more than 36 (more than 60% possible based on 63 being the max) responses (because of power)
# 60% like done in Veenman et al. (2023)

indnetalias <- number_observations_sast3$alias[which(number_observations_sast3$count_number > 36)]

# Test stationarity 
# Add date variable 
datasast_evening_sast_sub2$time <- as.Date(datasast_evening_sast_sub2$date2, "%d/%m/%Y")

# Select one variable 

detrendsast <- function(datasast_evening_sast_sub2, alias){
  
  detrendlist <- list()
  
  detrendlist$sigmatrix <- data.frame(alias = alias, 
                                      Worry = rep(NA, length(alias)), 
                                      SE = rep(NA, length(alias)), 
                                      Warmth = rep(NA, length(alias)), 
                                      Criticism = rep(NA, length(alias)))
  
  for(i in 1:length(alias)){
    # Select one person 
    datasast_evening_sast_sub2_81 <- datasast_evening_sast_sub2[datasast_evening_sast_sub2$alias == alias[i], c("alias", "time", "Worry", "Self-efficacy", "Warmth", "Criticism")]
    
    # Worry 
    regrworry_81 <- lm(Worry ~ time, data = datasast_evening_sast_sub2_81) 
    detrendlist$sigmatrix[i, "Worry"] <- summary(regrworry_81)$coefficients[2,4]
    datasast_evening_sast_sub2_81$Worry2[!is.na(datasast_evening_sast_sub2_81$Worry)] <- residuals(regrworry_81)
    
    # Self-efficacy 
    regrse_81 <- lm(`Self-efficacy` ~ time, data = datasast_evening_sast_sub2_81) 
    detrendlist$sigmatrix[i, "SE"] <- summary(regrse_81)$coefficients[2,4]
    datasast_evening_sast_sub2_81$SE2[!is.na(datasast_evening_sast_sub2_81$`Self-efficacy`)] <- residuals(regrse_81)
    
    # Warmth 
    regrwarm_81 <- lm(Warmth ~ time, data = datasast_evening_sast_sub2_81) 
    detrendlist$sigmatrix[i, "Warmth"] <- summary(regrwarm_81)$coefficients[2,4]
    datasast_evening_sast_sub2_81$Warmth2[!is.na(datasast_evening_sast_sub2_81$Warmth)] <- residuals(regrwarm_81)
    
    # Criticism 
    regrcri_81 <- lm(Criticism ~ time, data = datasast_evening_sast_sub2_81) 
    detrendlist$sigmatrix[i, "Criticism"] <- summary(regrcri_81)$coefficients[2,4]
    datasast_evening_sast_sub2_81$Criticism2[!is.na(datasast_evening_sast_sub2_81$Criticism)] <- residuals(regrcri_81)
    
    # Save data in list 
    detrendlist$data[[i]] <- datasast_evening_sast_sub2_81
  }
  
  return(detrendlist)
}

detrendsast_try1 <- detrendsast(datasast_evening_sast_sub2, indnetalias)
detrendsast_try1$sigmatrix < 0.05

#############################################################################################
# For one participant - example for function 
regrworry_81 <- lm(Worry ~ time, data = datasast_evening_sast_sub2_81) 
summary(regrworry_81)
summary(regrworry_81)$coefficients[2,4] < 0.05
datasast_evening_sast_sub2_81$Worry2[!is.na(datasast_evening_sast_sub2_81$Worry)] <- residuals(regrworry_81)
# more obs dan sub3 because worry is more often measured than other vars (even measured when not in contact child)

regrse_81 <- lm(`Self-efficacy` ~ time, data = datasast_evening_sast_sub2_81) 
summary(regrse_81)
datasast_evening_sast_sub2_81$SE2[!is.na(datasast_evening_sast_sub2_81$`Self-efficacy`)] <- residuals(regrse_81)


regrwarm_81 <- lm(Warmth ~ time, data = datasast_evening_sast_sub2_81) 
summary(regrwarm_81)
datasast_evening_sast_sub2_81$SE2[!is.na(datasast_evening_sast_sub2_81$`Self-efficacy`)] <- residuals(regrse_81)


regrwarm_81 <- lm(Warmth ~ time, data = datasast_evening_sast_sub2_81) 
summary(regrwarm_81)
datasast_evening_sast_sub2_81$Warmth2[!is.na(datasast_evening_sast_sub2_81$Warmth)] <- residuals(regrwarm_81)


regrcri_81 <- lm(Criticism ~ time, data = datasast_evening_sast_sub2_81) 
summary(regrcri_81)
datasast_evening_sast_sub2_81$Criticism2[!is.na(datasast_evening_sast_sub2_81$Criticism)] <- residuals(regrcri_81)

#############################################################################################
# Estimate individuals networks 

length(which(detrendsast_try1$sigmatrix[, 2:5] < 0.05))/length(detrendsast_try1$sigmatrix[, 2:5] < 0.05)

#############################################################################################
# Person A  
# Criticism non stationary 

## Histogram + line figure nondetrended data (completed responses only)
# Into long format + only select nondetrended variables 
detrendsast_p3_long <- detrendsast_try1$data[[3]][, 1:6] %>% 
  pivot_longer(-c(alias, time), names_to = "variables", values_to = "value")


histp3 <- ggplot(detrendsast_p3_long, aes(x = value, color = variables, fill = variables)) + 
  geom_histogram(position = "dodge", binwidth = 0.8, alpha=.75) + 
  theme_bw() + 
  scale_fill_manual(values = c(bordercolorworrytheme[3], bordercolorworrytheme[2], 
                               bordercolorworrytheme[5], bordercolorworrytheme[4])) + 
  scale_color_manual(values = (c(bordercolorworrytheme[3], bordercolorworrytheme[2], 
                                 bordercolorworrytheme[5], bordercolorworrytheme[4]))) + 
  # scale_color_manual(values = (c("#D3D0CBFF", "#9FB1BCFF"))) + 
  facet_grid(~fct_inorder(variables)) + 
  xlab("Response") + 
  ylab("Count") + 
  theme(legend.position = "none", 
        legend.title = element_blank(), 
        axis.text=element_text(size=12), 
        strip.text = element_text(size = 14)) + 
  scale_x_continuous(breaks = 1:7) 

# Add additional time variable for plot 
detrendsast_p3_long$time2 <- rep(1:(nrow(detrendsast_p3_long)/4), each = 4)

# Over time 
timep3 <- ggplot(detrendsast_p3_long, aes(x = time2, y = value, color = variables, 
                                          fill = variables, group = variables)) + 
  geom_line() + 
  geom_point() + 
  coord_cartesian(ylim = c(1, 7)) + 
  ylab("Score") + 
  xlab("\nCompleted Days") + 
  # ggtitle("A. Parental Worry") + 
  theme_bw() + 
  theme(panel.grid.major = element_line(color = "#ececec"), 
        legend.position = "none", 
        legend.title = element_blank()
  ) + 
  scale_color_manual(values = 
                       c(bordercolorworrytheme[3], bordercolorworrytheme[2], 
                         bordercolorworrytheme[5], bordercolorworrytheme[4])) + 
  scale_y_continuous(breaks = 1:7,
                     labels = c("1 \n (Not at all)", "2", "3", "4", "5", "6", "7 \n (Very)")) + 
  scale_x_continuous(breaks = seq(0, (nrow(detrendsast_p1_long)/4), by = 5)) + 
  facet_grid(~fct_inorder(variables), scales = "free") 

## 1: Estimate network on nondetrended data 
datasast_evening_sast_sub2_61_n1 <- detrendsast_try1$data[[3]][, c("Worry", "Self-efficacy", "Warmth", "Criticism")]
net61_var_n1 <- graphicalVAR::graphicalVAR(datasast_evening_sast_sub2_61_n1)
plot(net61_var_n1) 

qgraph(net61_var_n1$PCC, 
       layout = sastnetworkgroupfig$layout, 
       labels = c("Worry", "Self-efficacy", "Warmth", "Criticism"), 
       edge.labels = TRUE,
       edge.label.cex = 2,
       theme = "gray",
       color = c("white"), 
       border.color = c(bordercolorworrytheme[4], bordercolorworrytheme[2], 
                        bordercolorworrytheme[5], bordercolorworrytheme[3]),
       border.width = 9, 
       negDashed = TRUE,
       negCol = "#8a4d18",
       posCol = "black",
       edge.width = 0.75, 
       legend.cex = 0.8,
       vsize = 12,  # from 9 to 12
       label.cex = 2.5,  # from 2 to 2.5 
       esize = 37,  # from 23 to 37 
       cut = 0, 
       mar = c(4,4,4,4), 
       asize = 5, 
       legend = FALSE, 
       maximum = max(sastnetworkgroup$graph), 
       title = "Contemporaneous") 

qgraph(net61_var_n1$PDC, 
       layout = sastnetworkgroupfig$layout, 
       labels = c("Worry", "Self-efficacy", "Warmth", "Criticism"), 
       edge.labels = TRUE,
       edge.label.cex = 2,
       theme = "gray",
       color = c("white"), 
       border.color = c(bordercolorworrytheme[4], bordercolorworrytheme[2], 
                        bordercolorworrytheme[5], bordercolorworrytheme[3]),
       border.width = 9, 
       negDashed = TRUE,
       negCol = "#8a4d18",
       posCol = "black",
       edge.width = 0.75, 
       legend.cex = 0.8,
       vsize = 12,  # from 9 to 12
       label.cex = 2.5,  # from 2 to 2.5 
       esize = 37,  # from 23 to 37 
       cut = 0, 
       mar = c(4,4,4,4), 
       asize = 5, 
       legend = FALSE, 
       maximum = max(sastnetworkgroup$graph), 
       title = "Temporal")

## 2: Estimate network on partially detrended data (only variables detrended that are non stationary)
datasast_evening_sast_sub2_61_n2 <- detrendsast_try1$data[[3]][, c("Worry", "Self-efficacy", "Warmth", "Criticism2")]
net61_var_n2 <- graphicalVAR::graphicalVAR(datasast_evening_sast_sub2_61_n2)
plot(net61_var_n2) 

qgraph(net61_var_n2$PCC, 
       layout = sastnetworkgroupfig$layout, 
       labels = c("Worry", "Self-efficacy", "Warmth", "Criticism"), 
       edge.labels = TRUE,
       edge.label.cex = 2,
       theme = "gray",
       color = c("white"), 
       border.color = c(bordercolorworrytheme[4], bordercolorworrytheme[2], 
                        bordercolorworrytheme[5], bordercolorworrytheme[3]),
       border.width = 9, 
       negDashed = TRUE,
       negCol = "#8a4d18",
       posCol = "black",
       edge.width = 0.75, 
       legend.cex = 0.8,
       vsize = 12,  # from 9 to 12
       label.cex = 2.5,  # from 2 to 2.5 
       esize = 37,  # from 23 to 37 
       cut = 0, 
       mar = c(4,4,4,4), 
       asize = 5, 
       legend = FALSE, 
       maximum = max(sastnetworkgroup$graph), 
       title = "Contemporaneous") 

# For main manuscript 
qgraph(net61_var_n2$PCC, 
       layout = sastnetworkgroupfig$layout, 
       labels = c("Worry", "Self-efficacy", "Warmth", "Criticism"), 
       edge.labels = FALSE,
       edge.label.cex = 3,
       theme = "gray",
       color = c("white"), 
       border.color = c(bordercolorworrytheme[4], bordercolorworrytheme[2], 
                        bordercolorworrytheme[5], bordercolorworrytheme[3]),
       border.width = 20, 
       negDashed = TRUE,
       negCol = "#8a4d18",
       posCol = "black",
       edge.width = 0.75, 
       legend.cex = 0.8,
       vsize = 12,  # from 9 to 12
       label.cex = 2.5,  # from 2 to 2.5 
       esize = 37,  # from 23 to 37 
       cut = 0, 
       mar = c(4,4,4,4), 
       asize = 5, 
       legend = FALSE, 
       maximum = max(sastnetworkgroup$graph), 
       title = "") 

qgraph(net61_var_n2$PDC, 
       layout = sastnetworkgroupfig$layout, 
       labels = c("Worry", "Self-efficacy", "Warmth", "Criticism"), 
       edge.labels = TRUE,
       edge.label.cex = 2,
       theme = "gray",
       color = c("white"), 
       border.color = c(bordercolorworrytheme[4], bordercolorworrytheme[2], 
                        bordercolorworrytheme[5], bordercolorworrytheme[3]),
       border.width = 9, 
       negDashed = TRUE,
       negCol = "#8a4d18",
       posCol = "black",
       edge.width = 0.75, 
       legend.cex = 0.8,
       vsize = 12,  # from 9 to 12
       label.cex = 2.5,  # from 2 to 2.5 
       esize = 37,  # from 23 to 37 
       cut = 0, 
       mar = c(4,4,4,4), 
       asize = 5, 
       legend = FALSE, 
       maximum = max(sastnetworkgroup$graph), 
       title = "Temporal")

## 3: Estimate network on detrended data (all variables detrended also ones that are considered stationary)
datasast_evening_sast_sub2_61_n3 <- detrendsast_try1$data[[3]][, c("Worry2", "SE2", "Warmth2", "Criticism2")]
net61_var_n3 <- graphicalVAR::graphicalVAR(datasast_evening_sast_sub2_61_n3)
plot(net61_var_n3) 

qgraph(net61_var_n3$PCC, 
       layout = sastnetworkgroupfig$layout, 
       labels = c("Worry", "Self-efficacy", "Warmth", "Criticism"), 
       edge.labels = TRUE,
       edge.label.cex = 2,
       theme = "gray",
       color = c("white"), 
       border.color = c(bordercolorworrytheme[4], bordercolorworrytheme[2], 
                        bordercolorworrytheme[5], bordercolorworrytheme[3]),
       border.width = 9, 
       negDashed = TRUE,
       negCol = "#8a4d18",
       posCol = "black",
       edge.width = 0.75, 
       legend.cex = 0.8,
       vsize = 12,  # from 9 to 12
       label.cex = 2.5,  # from 2 to 2.5 
       esize = 37,  # from 23 to 37 
       cut = 0, 
       mar = c(4,4,4,4), 
       asize = 5, 
       legend = FALSE, 
       maximum = max(sastnetworkgroup$graph), 
       title = "Contemporaneous") 

qgraph(net61_var_n3$PDC, 
       layout = sastnetworkgroupfig$layout, 
       labels = c("Worry", "Self-efficacy", "Warmth", "Criticism"), 
       edge.labels = TRUE,
       edge.label.cex = 2,
       theme = "gray",
       color = c("white"), 
       border.color = c(bordercolorworrytheme[4], bordercolorworrytheme[2], 
                        bordercolorworrytheme[5], bordercolorworrytheme[3]),
       border.width = 9, 
       negDashed = TRUE,
       negCol = "#8a4d18",
       posCol = "black",
       edge.width = 0.75, 
       legend.cex = 0.8,
       vsize = 12,  # from 9 to 12
       label.cex = 2.5,  # from 2 to 2.5 
       esize = 37,  # from 23 to 37 
       cut = 0, 
       mar = c(4,4,4,4), 
       asize = 5, 
       legend = FALSE, 
       maximum = max(sastnetworkgroup$graph), 
       title = "Temporal")


#############################################################################################
# Person B  
# Worry, SE, and Warmth non stationary 

## Histogram + line figure nondetrended data (completed responses only)
# Into long format + only select nondetrended variables 
detrendsast_p4_long <- detrendsast_try1$data[[4]][, 1:6] %>% 
  pivot_longer(-c(alias, time), names_to = "variables", values_to = "value")


histp4 <- ggplot(detrendsast_p4_long, aes(x = value, color = variables, fill = variables)) + 
  geom_histogram(position = "dodge", binwidth = 0.8, alpha=.75) + 
  theme_bw() + 
  scale_fill_manual(values = c(bordercolorworrytheme[3], bordercolorworrytheme[2], 
                               bordercolorworrytheme[5], bordercolorworrytheme[4])) + 
  scale_color_manual(values = (c(bordercolorworrytheme[3], bordercolorworrytheme[2], 
                                 bordercolorworrytheme[5], bordercolorworrytheme[4]))) + 
  # scale_color_manual(values = (c("#D3D0CBFF", "#9FB1BCFF"))) + 
  facet_grid(~fct_inorder(variables)) + 
  xlab("Response") + 
  ylab("Count") + 
  theme(legend.position = "none", 
        legend.title = element_blank(), 
        axis.text=element_text(size=12), 
        strip.text = element_text(size = 14)) + 
  scale_x_continuous(breaks = 1:7) 

# Add additional time variable for plot 
detrendsast_p4_long$time2 <- rep(1:(nrow(detrendsast_p4_long)/4), each = 4)

# Over time 
timep4 <- ggplot(detrendsast_p4_long, aes(x = time2, y = value, color = variables, fill = variables, group = variables)) + 
  geom_line() + 
  geom_point() + 
  coord_cartesian(ylim = c(1, 7), xlim = c(1,64)) + 
  ylab("Score") + 
  xlab("\nCompleted Days") + 
  # ggtitle("A. Parental Worry") + 
  theme_bw() + 
  theme(panel.grid.major = element_line(color = "#ececec"), 
        legend.position = "none", 
        legend.title = element_blank()
  ) + 
  scale_color_manual(values = 
                       c(bordercolorworrytheme[3], bordercolorworrytheme[2], 
                         bordercolorworrytheme[5], bordercolorworrytheme[4])) + 
  scale_y_continuous(breaks = 1:7,
                     labels = c("1 \n (Not at all)", "2", "3", "4", "5", "6", "7 \n (Very)")) + 
  scale_x_continuous(breaks = seq(0, nrow(na.omit(detrendsast_try1$data[[1]])) + 5, by = 5)) + 
  facet_grid(~fct_inorder(variables), scales = "free") 

## 1: Estimate network on nondetrended data 
datasast_evening_sast_sub2_62_n1 <- detrendsast_try1$data[[4]][, c("Worry", "Self-efficacy", "Warmth", "Criticism")]
net62_var_n1 <- graphicalVAR::graphicalVAR(datasast_evening_sast_sub2_62_n1)
plot(net62_var_n1) 

qgraph(net62_var_n1$PCC, 
       layout = sastnetworkgroupfig$layout, 
       labels = c("Worry", "Self-efficacy", "Warmth", "Criticism"), 
       edge.labels = TRUE,
       edge.label.cex = 2,
       theme = "gray",
       color = c("white"), 
       border.color = c(bordercolorworrytheme[4], bordercolorworrytheme[2], 
                        bordercolorworrytheme[5], bordercolorworrytheme[3]),
       border.width = 9, 
       negDashed = TRUE,
       negCol = "#8a4d18",
       posCol = "black",
       edge.width = 0.75, 
       legend.cex = 0.8,
       vsize = 12,  # from 9 to 12
       label.cex = 2.5,  # from 2 to 2.5 
       esize = 37,  # from 23 to 37 
       cut = 0, 
       mar = c(4,4,4,4), 
       asize = 5, 
       legend = FALSE, 
       maximum = max(sastnetworkgroup$graph), 
       title = "Contemporaneous") 

qgraph(net62_var_n1$PDC, 
       layout = sastnetworkgroupfig$layout, 
       labels = c("Worry", "Self-efficacy", "Warmth", "Criticism"), 
       edge.labels = TRUE,
       edge.label.cex = 2,
       theme = "gray",
       color = c("white"), 
       border.color = c(bordercolorworrytheme[4], bordercolorworrytheme[2], 
                        bordercolorworrytheme[5], bordercolorworrytheme[3]),
       border.width = 9, 
       negDashed = TRUE,
       negCol = "#8a4d18",
       posCol = "black",
       edge.width = 0.75, 
       legend.cex = 0.8,
       vsize = 12,  # from 9 to 12
       label.cex = 2.5,  # from 2 to 2.5 
       esize = 37,  # from 23 to 37 
       cut = 0, 
       mar = c(4,4,4,4), 
       asize = 5, 
       legend = FALSE, 
       maximum = max(sastnetworkgroup$graph), 
       title = "Temporal")

## 2: Estimate network on partially detrended data (only variables detrended that are non stationary)
datasast_evening_sast_sub2_62_n2 <- detrendsast_try1$data[[4]][, c("Worry2", "SE2", "Warmth2", "Criticism")]
net62_var_n2 <- graphicalVAR::graphicalVAR(datasast_evening_sast_sub2_62_n2)
plot(net62_var_n2) 

qgraph(net62_var_n2$PCC, 
       layout = sastnetworkgroupfig$layout, 
       labels = c("Worry", "Self-efficacy", "Warmth", "Criticism"), 
       edge.labels = TRUE,
       edge.label.cex = 2,
       theme = "gray",
       color = c("white"), 
       border.color = c(bordercolorworrytheme[4], bordercolorworrytheme[2], 
                        bordercolorworrytheme[5], bordercolorworrytheme[3]),
       border.width = 9, 
       negDashed = TRUE,
       negCol = "#8a4d18",
       posCol = "black",
       edge.width = 0.75, 
       legend.cex = 0.8,
       vsize = 12,  # from 9 to 12
       label.cex = 2.5,  # from 2 to 2.5 
       esize = 37,  # from 23 to 37 
       cut = 0, 
       mar = c(4,4,4,4), 
       asize = 5, 
       legend = FALSE, 
       maximum = max(sastnetworkgroup$graph), 
       title = "Contemporaneous") 

# For main manuscript 
qgraph(net62_var_n2$PCC, 
       layout = sastnetworkgroupfig$layout, 
       labels = c("Worry", "Self-efficacy", "Warmth", "Criticism"), 
       edge.labels = FALSE,
       edge.label.cex = 3,
       theme = "gray",
       color = c("white"), 
       border.color = c(bordercolorworrytheme[4], bordercolorworrytheme[2], 
                        bordercolorworrytheme[5], bordercolorworrytheme[3]),
       border.width = 20, 
       negDashed = TRUE,
       negCol = "#8a4d18",
       posCol = "black",
       edge.width = 0.75, 
       legend.cex = 0.8,
       vsize = 12,  # from 9 to 12
       label.cex = 2.5,  # from 2 to 2.5 
       esize = 37,  # from 23 to 37 
       cut = 0, 
       mar = c(4,4,4,4), 
       asize = 5, 
       legend = FALSE, 
       maximum = max(sastnetworkgroup$graph), 
       title = "") 

# Export as 9 x 13 

qgraph(net62_var_n2$PDC, 
       layout = sastnetworkgroupfig$layout, 
       labels = c("Worry", "Self-efficacy", "Warmth", "Criticism"), 
       edge.labels = TRUE,
       edge.label.cex = 2,
       theme = "gray",
       color = c("white"), 
       border.color = c(bordercolorworrytheme[4], bordercolorworrytheme[2], 
                        bordercolorworrytheme[5], bordercolorworrytheme[3]),
       border.width = 9, 
       negDashed = TRUE,
       negCol = "#8a4d18",
       posCol = "black",
       edge.width = 0.75, 
       legend.cex = 0.8,
       vsize = 12,  # from 9 to 12
       label.cex = 2.5,  # from 2 to 2.5 
       esize = 37,  # from 23 to 37 
       cut = 0, 
       mar = c(4,4,4,4), 
       asize = 5, 
       legend = FALSE, 
       maximum = max(sastnetworkgroup$graph), 
       title = "Temporal")

## 3: Estimate network on detrended data (all variables detrended also ones that are considered stationary)
datasast_evening_sast_sub2_62_n3 <- detrendsast_try1$data[[4]][, c("Worry2", "SE2", "Warmth2", "Criticism2")]
net62_var_n3 <- graphicalVAR::graphicalVAR(datasast_evening_sast_sub2_62_n3)
plot(net62_var_n3) 

qgraph(net62_var_n3$PCC, 
       layout = sastnetworkgroupfig$layout, 
       labels = c("Worry", "Self-efficacy", "Warmth", "Criticism"), 
       edge.labels = TRUE,
       edge.label.cex = 2,
       theme = "gray",
       color = c("white"), 
       border.color = c(bordercolorworrytheme[4], bordercolorworrytheme[2], 
                        bordercolorworrytheme[5], bordercolorworrytheme[3]),
       border.width = 9, 
       negDashed = TRUE,
       negCol = "#8a4d18",
       posCol = "black",
       edge.width = 0.75, 
       legend.cex = 0.8,
       vsize = 12,  # from 9 to 12
       label.cex = 2.5,  # from 2 to 2.5 
       esize = 37,  # from 23 to 37 
       cut = 0, 
       mar = c(4,4,4,4), 
       asize = 5, 
       legend = FALSE, 
       maximum = max(sastnetworkgroup$graph), 
       title = "Contemporaneous") 

qgraph(net62_var_n3$PDC, 
       layout = sastnetworkgroupfig$layout, 
       labels = c("Worry", "Self-efficacy", "Warmth", "Criticism"), 
       edge.labels = TRUE,
       edge.label.cex = 2,
       theme = "gray",
       color = c("white"), 
       border.color = c(bordercolorworrytheme[4], bordercolorworrytheme[2], 
                        bordercolorworrytheme[5], bordercolorworrytheme[3]),
       border.width = 9, 
       negDashed = TRUE,
       negCol = "#8a4d18",
       posCol = "black",
       edge.width = 0.75, 
       legend.cex = 0.8,
       vsize = 12,  # from 9 to 12
       label.cex = 2.5,  # from 2 to 2.5 
       esize = 37,  # from 23 to 37 
       cut = 0, 
       mar = c(4,4,4,4), 
       asize = 5, 
       legend = FALSE, 
       maximum = max(sastnetworkgroup$graph), 
       title = "Temporal")



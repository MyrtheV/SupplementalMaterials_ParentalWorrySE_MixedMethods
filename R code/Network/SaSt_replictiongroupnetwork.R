################################################################################################
# Replication group network + individual networks 
################################################################################################

################################################################################################
# Load packages 
################################################################################################ 
library(data.table) 
library(psychonetrics)
library(qgraph)
library(tidyverse)

################################################################################################
# Load data - the data cannot be shared due to privacy restrictions 
################################################################################################
# Instead we will share the data processing steps: 
# 1. Read in data with read.csv and sep by ; 
# 2. Corrected one participant number (mistake made by participant)
# 3. Selected the participants for this study by participant number 

# The following steps we can share: 
# 4. Adjusting variable into data 
datasast_evening_sast_sub$date <- datasast_evening_sast_sub$timeStampStart |> as_datetime() |> force_tz("Europe/Brussels")  
datasast_evening_sast_sub$date2 <- format(datasast_evening_sast_sub$date, format = "%d/%m/%Y")

# 5. Selected variables of interest 
networkvarnames <- c("eve_worry_day_sliderNeutralPos", 
                     "eve_selfefficacy_sliderNeutralPos", 
                     "eve_warm_sliderNeutralPos", 
                     "eve_critic_sliderNeutralPos")

datasast_evening_sast_sub2 <- datasast_evening_sast_sub[, c("alias", "date2", networkvarnames)]

# Adjusted column names 
colnames(datasast_evening_sast_sub2) <- c("alias", "date2", "Worry", "Self-efficacy", "Warmth", "Criticism")

################################################################################################
# Histogram responses 
################################################################################################
datasast_evening_sast_sub2_long <- datasast_evening_sast_sub2 %>% 
  pivot_longer(-c(alias, date2), names_to = "variables", values_to = "value")

rowSums(table(datasast_evening_sast_sub2$alias,datasast_evening_sast_sub2$SE))
complete.cases(datasast_evening_sast_sub2)

datasast_evening_sast_sub3 <- na.omit(datasast_evening_sast_sub2)

number_observations_sast3 <- datasast_evening_sast_sub3 %>% 
  group_by(alias) %>% 
  summarise(count_number = n())

mean(number_observations_sast3$count_number)
sd(number_observations_sast3$count_number)


# Estimate means and sds and create dataframe of them 
# Group means 
groupmeansast3 <- datasast_evening_sast_sub3 %>% 
  summarise_at(c("Worry", "Self-efficacy", "Warmth", "Criticism"), mean, na.rm = TRUE)

# Group sds 
groupsdsast3 <- datasast_evening_sast_sub3 %>% 
  summarise_at(c("Worry", "Self-efficacy", "Warmth", "Criticism"), sd, na.rm = TRUE)


groupmeansdsast_df <- data.frame(variables = c("Worry", "Self-efficacy", "Warmth", "Criticism"), 
           mean = c(groupmeansast3$Worry, groupmeansast3$`Self-efficacy`, 
                    groupmeansast3$Warmth, groupmeansast3$Criticism), 
           sd = c(groupsdsast3$Worry, groupsdsast3$`Self-efficacy`, 
                  groupsdsast3$Warmth, groupsdsast3$Criticism), 
           value = c(3, 3, 3, 5))

# Figure 
ggplot(datasast_evening_sast_sub2_long, aes(x = as.numeric(value))) + 
  geom_histogram(position = "dodge", binwidth = 0.5, alpha=.75) + 
  theme_bw() + 
  scale_fill_manual(values = (c("#D3D0CBFF", "#9FB1BCFF"))) + 
  scale_color_manual(values = (c("darkgrey", "#6E8898FF"))) + 
  # scale_color_manual(values = (c("#D3D0CBFF", "#9FB1BCFF"))) + 
  facet_grid(~fct_inorder(variables), scales = "free") + 
  xlab("Response") + 
  ylab("Count") + 
  theme(legend.position = "right", 
        legend.title = element_blank()) + 
  scale_x_continuous(breaks = 1:7) + 
  geom_text(x = 5, y = 180, label = groupmeansast3$Worry)

ggplot(datasast_evening_sast_sub2_long, aes(x = as.numeric(value), color = variables, fill = variables)) + 
  geom_histogram(position = "dodge",binwidth = 0.5, alpha=.75) + 
  theme_bw() + 
  scale_fill_manual(values = c(bordercolorworrytheme[3], bordercolorworrytheme[2], 
                                bordercolorworrytheme[5], bordercolorworrytheme[4])) + 
  scale_color_manual(values = (c(bordercolorworrytheme[3], bordercolorworrytheme[2], 
                                 bordercolorworrytheme[5], bordercolorworrytheme[4]))) + 
  # scale_color_manual(values = (c("#D3D0CBFF", "#9FB1BCFF"))) + 
  facet_grid(~fct_inorder(variables), scales = "free") + 
  xlab("Response") + 
  ylab("Count") + 
  theme(legend.position = "none", 
        legend.title = element_blank(), 
        axis.text=element_text(size=12), 
        strip.text = element_text(size = 14)) + 
  scale_x_continuous(breaks = 1:7) + 
  geom_text(data = groupmeansdsast_df, y = 190, aes(label = paste("M = ",round(mean, 2))), size = 6) + 
  geom_text(data = groupmeansdsast_df, y = 180, aes(label = paste("SD = ",round(sd, 2))), size = 6)


# Export 6 x 8 

################################################################################################
# Replication: Group network 
################################################################################################
# Data prep
# Select right columns 
datasast_evening_sast_sub_net <- datasast_evening_sast_sub[, networkvarnames]
datasast_evening_sast_sub_net2 <- na.omit(datasast_evening_sast_sub_net)

# Network estimation 
sastnetworkgroup <- estimateNetwork(datasast_evening_sast_sub_net2, "EBICglasso")
saveRDS(sastnetworkgroup, "sastnetworkgroup_01082024.rds")
# sastnetworkgroup <- readRDS("sastnetworkgroup_01082024.rds") - cannnot be shared because contains data 

# stability 
sastnetworkgroup_ewaccur <- bootnet(sastnetworkgroup, bootstraps = 1000)
saveRDS(sastnetworkgroup_ewaccur, "sastnetworkgroup_ewaccur_01082024.rds")
# sastnetworkgroup_ewaccur <- readRDS("sastnetworkgroup_ewaccur_01082024.rds") - cannot be shared because contains data 

layoutsast <- matrix(c(0, 0.1, 
                       0.1, 0.1, 
                       0.1, -0.1, 
                       0, -0.1), nrow = 4, ncol =2, byrow = TRUE)


bordercolorworrytheme <- c("#FFDB6D", "#C4961A", "#D16103", "#52854C", "#4E84C4", "#293352")

# Plot network 
sastnetworkgroupfig <- plot(sastnetworkgroup, 
                            layout = layoutsast, 
                            labels = c("Worry", "Self-efficacy", 
                                       "Warmth", "Criticism"),
                            edge.labels = FALSE,
                            edge.label.cex = 1,
                            theme = "gray",
                            color = c("white"),
                            border.color = c(bordercolorworrytheme[4], bordercolorworrytheme[2], 
                                       bordercolorworrytheme[5], bordercolorworrytheme[3]),
                            border.width = 19, 
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
                            repulsion = 0.9, 
                            maximum = max(sastnetworkgroup$graph)  # for comparison with other network 
)

# Stability 
sastnetworkgroup_ewaccur_2 <- plot(sastnetworkgroup_ewaccur, order = "id", 
                                   sampleColor = "#6E8898FF", samplelwd = 0.4, labels = TRUE, 
                                   meanlwd = 0.5, meanColor = "darkgrey")



sastnetworkgroup_ewaccur_2plot <- ggplot(sastnetworkgroup_ewaccur_2$data, 
                                         aes(x = value, y = numericID, color = var, fill = var, group = var)) +
  geom_ribbon(aes(xmin = q2.5, xmax = q97.5), 
              color = NA, fill = "grey", alpha = 0.2) + 
  geom_path(aes(linetype = var), size = 0.7) + 
  geom_point(alpha = 0.6, size = 3) + 
  geom_point(size = 2, shape = 21, fill = NA, stroke = 1) + 
  scale_y_continuous(breaks = seq(1:length(unique(proj3_booted_amfnethc_res$data$numericID))),
                     labels = seq(1:length(unique(proj3_booted_amfnethc_res$data$numericID)))) +   # levels(proj3_booted_part2hc_res$data$id)
  scale_color_manual("", values = c("#494F55", "#6E8898FF"), labels = c("Bootstrap mean","Sample")) + 
  scale_fill_manual("", values = c("#494F55", "#6E8898FF"), labels = c("Bootstrap mean","Sample")) + 
  scale_linetype_manual("", values = c("solid","dashed"), labels = c("Bootstrap mean","Sample"))+ 
  theme_classic() + 
  theme(panel.grid.major = element_line(color = "grey80"),
        panel.grid.minor.x = element_line(color = "grey80"),
        strip.background = element_blank(), 
        strip.text = element_blank()) + 
  labs(title = "", subtitle = "", x = "Weights", y = "Edges") 


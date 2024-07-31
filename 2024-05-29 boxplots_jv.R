library(ggplot2) #plots
library(dplyr) #filter
library(xlsx)
library(furniture) #table1
library(summarytools) #descriptions
library(RColorBrewer) #color in figures
library(pROC)
library(caret)

#code to check version of R
R.Version() 

#Read in data

features1 <- read.csv("C:/Users/Jet/Dropbox/Manuscripts and Publications/1. In prep/1. First-authored/2022 MAC Speech Right vs left ATL/2024-05-10 SPSS Analysis/2024-05-06_subset_features.csv")

features1[features1 == -9] <- NA


#Only select n=62 PIDNs

# List of identifier numbers
PIDNs <- c(10744, 1619, 17733, 17735, 24884, 25694, 2679, 2743, 33609, 6866,
                 6867, 6868, 6905, 6970, 7261, 7853, 8497, 8542, 8545, 9333,
                 9606, 9822, 11704, 11773, 14062, 14878, 16825, 17020, 17797,
                 18304, 20225, 22324, 23393, 2522, 25977, 2662, 27593, 28989,
                 31792, 3426, 3690, 4747, 5284, 6600, 8704, 9283, 10177, 13976,
                 14285, 14731, 18137, 20458, 21153, 25178, 25252, 25660, 26752,
                 28619, 28704, 30130, 30180, 687)

# Filter the data frame to include only the rows with the specified identifiers
features <- features1[!is.na(match(features1$PIDN, PIDNs)), ]

features <- features %>%
  mutate(DX = ifelse(DX == "HC", "Control", DX))


#Descriptive stats to check data

#view(dfSummary(features))

#selected set of LINGUISTIC variables (adjust as needed)
language_cols <- select(features,"DX",
                        "CONTENT_UNITS",
                        "NVAA_FREQ_MEAN",
                        "NVAA_FAM_PREV_MEAN",
                        "ADP_TOTAL_WORDS_RATIO",
                        "AROUSAL_NVAA",
                        "AVERAGE_SYLL_PAUSE_DURATION",
                        "NUM_RESTARTS",
                        "THINGS_RATIO"
)

# Rename the column names 
language_cols <- rename(language_cols,
                        "Diagnosis" = DX,
                        "Content units" = CONTENT_UNITS,
                        "Lexical frequency" = NVAA_FREQ_MEAN,
                        "Familiarity" = NVAA_FAM_PREV_MEAN,
                        "Adpositions ratio" = ADP_TOTAL_WORDS_RATIO,
                        "Arousal" = AROUSAL_NVAA,
                        "Syllable pause duration" = AVERAGE_SYLL_PAUSE_DURATION,
                        "Restarts" = NUM_RESTARTS,
                        "Thing words" = THINGS_RATIO)
                        

#selected set of DEMOGRAPHIC variables (adjust as needed)
demographics <- select(features,"DX","AGE_AT_PICNIC","HAND","GENDER_LABEL","EDUC","RACE_LABEL")

# Rename the column names 
#demographics <- rename(demographics,                        
#                        "Diagnosis" = DX,
#                        "Age" = AGE_AT_PICNIC,
#                        "Handedness" = HAND,
#                        "Sex/gender" = GENDER_LABEL,
#                        "Education" = EDUC,
#                        "Race/ethnicity" = RACE_LABEL)




#selected set of TEST variables (adjust as needed)
cognitive_cols <- select(features,"DX","MODTRAILS_TIME","BENSON_MODREY","BENSON_REY10M",
                         "DIGITBW","STROOPCOR","VOSP_NUMBLOC","MMSE_TOT", "CDR_BOXSCORE",
                         "CVLT_CORR30","CVLT_CORR10", "NPI_SCORE", "BNT_CORR")



#Furniture Table1
#demographics table
demo_table_1_updated <- table1(
  demographics, 
  AGE_AT_PICNIC, 
  GENDER_LABEL, 
  EDUC, 
  RACE_LABEL,  
  HAND, 
  splitby = "DX", 
  test = TRUE, 
  na.rm = FALSE, 
  type = "condense", 
  FUN = function(x) {
    mean_val <- round(mean(x, na.rm = TRUE), digits = 2)
    sd_val <- round(sd(x, na.rm = TRUE), digits = 2)
    min_val <- round(min(x, na.rm = TRUE), digits = 2)
    max_val <- round(max(x, na.rm = TRUE), digits = 1)
    paste0(mean_val, " (", sd_val, ", ", min_val, "-", max_val, ")")
  }
)

cognitive_table_1_updated <- table1(
  features, 
  MMSE_TOT, 
  CDR_BOXSCORE, 
  BNT_CORR, 
  CVLT_CORR30,  
  CVLT_CORR10, 
  splitby = "DX", 
  test = TRUE, 
  na.rm = FALSE, 
  type = "condense", 
  FUN = function(x) {
    mean_val <- round(mean(x, na.rm = TRUE), digits = 2)
    sd_val <- round(sd(x, na.rm = TRUE), digits = 2)
    min_val <- round(min(x, na.rm = TRUE), digits = 2)
    max_val <- round(max(x, na.rm = TRUE), digits = 1)
    paste0(mean_val, " (", sd_val, ", ", min_val, "-", max_val, ")")
  }
)

write.csv(demo_table_1_updated, file = "C:/Users/Jet/Dropbox/Manuscripts and Publications/1. In prep/1. First-authored/2022 MAC Speech Right vs left ATL/2024-05-10 SPSS Analysis/2024-05-29 Figures_jv/T1_demo.csv")

write.csv(cognitive_table_1_updated, file = "C:/Users/Jet/Dropbox/Manuscripts and Publications/1. In prep/1. First-authored/2022 MAC Speech Right vs left ATL/2024-05-10 SPSS Analysis/2024-05-29 Figures_jv/T1_cog.csv")


# Define a custom label function to remove leading zeros
remove_leading_zero <- function(x) {
  sapply(x, function(y) {
    # Convert to character, ensuring that trailing zeros are retained
    y_char <- format(y, scientific = FALSE, trim = TRUE)
    # Apply the regex substitution to remove leading zero before the decimal
    gsub("^(-?)0\\.", "\\1.", y_char)
  })
}


###Boxplots


#loop for boxplots of all features
for(i in 2:ncol(language_cols)) {
  p = ggplot(language_cols, aes(x = factor(Diagnosis, levels = c("Control", "Frontal", "rATL")), y = language_cols[ , i], fill = Diagnosis)) + geom_boxplot(notch=FALSE)  +
    theme_bw() +
    scale_fill_manual(values = brewer.pal(9, "YlOrRd")[c(2,5,8)]) +
    theme(aspect.ratio = 1.35, legend.position = "none", axis.text.x = element_text(size=14), axis.text.y = element_text(size=14), axis.title.y = element_text(size=14), plot.margin=grid::unit(c(5,0,0,0), "mm")) +
    labs(x = "", y = colnames(language_cols)[i]) 
  print(p)
  
  ggsave(
    file=paste0("boxplot_",colnames(language_cols[i]),".tiff"), #can also be .png
    height = 3.5,
    width = 3,
    dpi = 300,
    path = "C:/Users/Jet/Dropbox/Manuscripts and Publications/1. In prep/1. First-authored/2022 MAC Speech Right vs left ATL/2024-05-10 SPSS Analysis/2024-05-29 Figures_jv")
  
}


#ROC curves and confusion matrix

models_n62 <- read.csv("C:/Users/Jet/Dropbox/Manuscripts and Publications/1. In prep/1. First-authored/2022 MAC Speech Right vs left ATL/2024-05-10 SPSS Analysis/2024-05-28_forward_models_n62.csv")

models_n62$DX_BI <- factor(models_n62$DX_BI, levels = c(0, 1), labels = c("Control", "FTD"))



######################################################
#ROC curves

###Control vs FTD

# Define the file path and name
file_path <- "C:/Users/Jet/Dropbox/Manuscripts and Publications/1. In prep/1. First-authored/2022 MAC Speech Right vs left ATL/2024-05-10 SPSS Analysis/2024-05-29 Figures_jv/roc_plotControlFTD.tiff"

# Open a TIFF device
tiff(file = file_path, height = 5, width = 5, units = "in", res = 300)

# Set graphical parameters to increase the font size of the axes text and numbers
par(cex.axis = 1.5, cex.lab = 1.5, mar = c(5, 5, 4, 2) + 0.1)

# Plot ROC curve for your model
rocobj1 <- plot.roc(models_n62$DX_BI, models_n62$PRED_HCvDEM_ENTER,
                    asp = NA,
                    percent = TRUE,
                    col = "#1c61b6")

# Add text annotations with AUC
text(60, 40, labels = paste("AUC =", round(auc(models_n62$DX_BI, models_n62$PRED_HCvDEM_ENTER), 3)), cex = 1.5, adj = c(0.1, 5))

# Close the TIFF device
dev.off()

###CONFUSION MATRIX

# Create a binary prediction based on a cutoff of 0.5
models_n62$PREDICTED_CLASS <- ifelse(models_n62$PRED_HCvDEM_ENTER > 0.5, "FTD", "Control")

# Confusion matrix
confusion_matrix <- confusionMatrix(factor(models_n62$PREDICTED_CLASS), factor(models_n62$DX_BI))
print(confusion_matrix)

# Prepare data for plotting the confusion matrix
confusion_matrix_data <- as.data.frame(confusion_matrix$table)
colnames(confusion_matrix_data) <- c("Reference", "Prediction", "Freq")

# Calculate percentages
confusion_matrix_data <- confusion_matrix_data %>%
  group_by(Reference) %>%
  mutate(Percent = Freq / sum(Freq) * 100)

# Create a new column to categorize the percentages into two groups
confusion_matrix_data$PercentGroup <- ifelse(confusion_matrix_data$Percent > mean(confusion_matrix_data$Percent), "Above Mean", "Below Mean")

# Define a fill color palette with two distinct colors
fill_colors <- c("Below Mean" = "#D6EAF8", "Above Mean" = "#2E86C1")

# Plot confusion matrix using ggplot2
cm_plotControlFTD <- ggplot(data = confusion_matrix_data, aes(x = Reference, y = Prediction, fill = PercentGroup)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.1f%%", Percent)), vjust = .5, size = 6) +  # Increase text size
  scale_fill_manual(values = fill_colors) +
  theme_minimal() +
  labs(x = "True class", y = "Predicted class") +
  theme(
    legend.position = "none",  # Optionally hide the legend if not needed
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  ) 
  + coord_fixed()

# Save the confusion matrix plot as a TIFF file
ggsave(
  filename = "C:/Users/Jet/Dropbox/Manuscripts and Publications/1. In prep/1. First-authored/2022 MAC Speech Right vs left ATL/2024-05-10 SPSS Analysis/2024-05-29 Figures_jv/cm_FTDControl.tiff",
  plot = cm_plotControlFTD,
  height = 3,
  width = 3,
  dpi = 300
)


###frontal vs rATL

models_n62_FTD <- models_n62 %>%
  filter(DX %in% c("Frontal", "rATL"))

# Define the file path and name
file_path <- "C:/Users/Jet/Dropbox/Manuscripts and Publications/1. In prep/1. First-authored/2022 MAC Speech Right vs left ATL/2024-05-10 SPSS Analysis/2024-05-29 Figures_jv/roc_plotfrontalrATL.tiff"

# Open a TIFF device
tiff(file = file_path, height = 5, width = 5, units = "in", res = 300)

# Set graphical parameters to increase the font size of the axes text and numbers
par(cex.axis = 1.5, cex.lab = 1.5, mar = c(5, 5, 4, 2) + 0.1)

# Plot ROC curve for your model
rocobj2 <- plot.roc(models_n62_FTD$DX, models_n62_FTD$PRED_RvF_ENTER,
                    asp = NA,
                    percent = TRUE,
                    col = "#1c61b6")

# Add text annotations with AUC
text(60, 40, labels = paste("AUC =", round(auc(models_n62_FTD$DX, models_n62_FTD$PRED_RvF_ENTER), 3)), cex = 1.5, adj = c(0.1, 5))

# Close the TIFF device
dev.off()

###CONFUSION MATRIX

# Create a binary prediction based on a cutoff of 0.5
models_n62_FTD$PREDICTED_CLASS <- ifelse(models_n62_FTD$PRED_RvF_ENTER > 0.5, "Frontal", "rATL")

# Confusion matrix
confusion_matrix2 <- confusionMatrix(factor(models_n62_FTD$PREDICTED_CLASS), factor(models_n62_FTD$DX))
print(confusion_matrix2)

# Prepare data for plotting the confusion matrix
confusion_matrix_data2 <- as.data.frame(confusion_matrix2$table)
colnames(confusion_matrix_data2) <- c("Reference", "Prediction", "Freq")

# Calculate percentages
confusion_matrix_data2 <- confusion_matrix_data2 %>%
  group_by(Reference) %>%
  mutate(Percent = Freq / sum(Freq) * 100)

# Create a new column to categorize the percentages into two groups
confusion_matrix_data2$PercentGroup <- ifelse(confusion_matrix_data2$Percent > mean(confusion_matrix_data2$Percent), "Above Mean", "Below Mean")

# Define a fill color palette with two distinct colors
fill_colors <- c("Below Mean" = "#D6EAF8", "Above Mean" = "#2E86C1")


# Plot confusion matrix using ggplot2
cm_plotfrontalrATL <- ggplot(data = confusion_matrix_data2, aes(x = Reference, y = Prediction, fill = PercentGroup)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.1f%%", Percent)), vjust = .5, size = 6) +  # Increase text size
  scale_fill_manual(values = fill_colors) +
  theme_minimal() +
  labs(x = "True class", y = "Predicted class") +
  theme(
    legend.position = "none",  # Optionally hide the legend if not needed
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  ) 
  + coord_fixed()

# Save the confusion matrix plot as a TIFF file
ggsave(
  filename = "C:/Users/Jet/Dropbox/Manuscripts and Publications/1. In prep/1. First-authored/2022 MAC Speech Right vs left ATL/2024-05-10 SPSS Analysis/2024-05-29 Figures_jv/cm_frontalrATL.tiff",
  plot = cm_plotfrontalrATL,
  height = 3,
  width = 3,
  dpi = 300
)



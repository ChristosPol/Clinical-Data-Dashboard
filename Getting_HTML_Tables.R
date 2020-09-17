rm(list=ls())
gc()
source(paste(getwd(), "Utils.R", sep ="/"))

# Vector of websites to scrape
url <- c("https://www.cochranelibrary.com/cdsr/doi/10.1002/14651858.CD012816.pub2/full",
         "https://www.cochranelibrary.com/cdsr/doi/10.1002/14651858.CD003506.pub2/full",
         "https://www.cochranelibrary.com/cdsr/doi/10.1002/14651858.CD009266.pub2/full")

# Websites scraped and ready to be edited
measures <- GET_HTML(url)

# Renaming all columns so we can rbind
colnames(measures[[1]])[grep("Relative", colnames(measures[[1]]))] <- "Relative_effect_95_CI"
colnames(measures[[2]])[grep("Relative", colnames(measures[[2]]))] <- "Relative_effect_95_CI"
colnames(measures[[3]])[grep("Relative", colnames(measures[[3]]))] <- "Relative_effect_95_CI"

colnames(measures[[1]])[grep("participants", colnames(measures[[1]]))] <- "Number_of_participants"
colnames(measures[[2]])[grep("participants", colnames(measures[[2]]))] <- "Number_of_participants"
colnames(measures[[3]])[grep("participants", colnames(measures[[3]]))] <- "Number_of_participants"

colnames(measures[[1]])[grep("GRADE", colnames(measures[[1]]))] <- "Quality_GRADE"
colnames(measures[[2]])[grep("GRADE", colnames(measures[[2]]))] <- "Quality_GRADE"
colnames(measures[[3]])[grep("GRADE", colnames(measures[[3]]))] <- "Quality_GRADE"

final_table <- do.call(rbind, measures)
final_table$Relative_effect_95_CI <- gsub(pattern = "  ", " ", final_table$Relative_effect_95_CI)

final_table$Statistic <- unlist(lapply(strsplit(final_table$Relative_effect_95_CI, " "), "[", 1))
final_table$Effect_size <- as.numeric(unlist(lapply(strsplit(final_table$Relative_effect_95_CI, " "), "[", 2)))
final_table$CI_Lower <- unlist(lapply(strsplit(final_table$Relative_effect_95_CI, " "), "[", 3))
final_table$CI_Lower <- as.numeric(gsub("\\(", "", final_table$CI_Lower))
final_table$CI_Upper <- unlist(lapply(strsplit(final_table$Relative_effect_95_CI, " "), "[", 5))
final_table$CI_Upper <- as.numeric(gsub("\\)", "", final_table$CI_Upper))
final_table$N <- as.numeric( gsub(",","", gsub( "\\s.*", "", final_table$Number_of_participants)))
final_table$Quality_GRADE_act <- trimws(substr(final_table$Quality_GRADE,5, nchar(final_table$Quality_GRADE)), "both")
final_table$Quality_GRADE_act <-toupper(trimws(gsub('[[:digit:]]+', '', final_table$Quality_GRADE_act), "both"))
final_table$Quality_GRADE_act <- gsub(",", "", final_table$Quality_GRADE_act)

# Plotting
final_table_plot <- subset(final_table, final_table$Effect_size<50)
  
p1 <- ggplot(data = final_table_plot, aes(x = N, y = Effect_size, color = factor(Quality_GRADE_act))) +
      geom_errorbar(aes(ymin = Effect_size-CI_Lower, ymax = Effect_size+CI_Upper),width = 500,size = 0.3, color = "black")+
      geom_point(size = 5) + theme_bw()+ ggtitle("Prostate Cancer Example")+
      labs(y = "Effect Size (RR/HR)", x = "Number of Participants")+
      labs(colour = "GRADE")+
      theme(legend.position = c(0.9, 0.9),legend.key=element_blank(),legend.background=element_blank());p1

p1 + scale_color_brewer(palette="Dark2")

p2 <- ggplot(data = final_table_plot, aes(x = Quality_GRADE_act, y = N, size = Effect_size, color = factor(Statistic))) +
  geom_point(alpha=0.7)+ scale_size_continuous(range = c(3, 8))+ ggtitle("Prostate Cancer Example")+
  labs(y = "Number of Participants", x = "GRADE")+
  labs(colour= "Statistic",size = "Effect size")+
  theme(legend.position = c(0.9, 0.8),legend.key=element_blank(),legend.background=element_blank());p2

p2 + scale_color_brewer(palette="Dark2")

write.csv(final_table, "/media/chris/DATA/Documents/Clinical_Data_Dashboard/Data.csv")

#read the file into R to analyze
AI_data = read.csv(file.choose(), header=TRUE)
# install corrplot
install.packages("corrplot")
library(corrplot)
variables <- c(
  "StudAge",
  "Sex",
  "HS_Type",
  "Scholar",
  "Add_Work",
  "Art_Sport",
  "Partner",
  "Salary",
  "Trans_Uni",
  "Accom_Cyp",
  "Mom_Edu",
  "Dad_Edu",
  "Siblings",
  "Par_Stat",
  "Mom_Occ",
  "Dad_Occ",
  "Study_Hrs",
  "Read_NSBk",
  "Read_SBk",
  "Attend_Sem",
  "Proj_Imp",
  "Class_Att",
  "Prep_Mid1",
  "Prep_Mid2",
  "Notes",
  "Listen",
  "Discuss",
  "Flip_Class",
  "GPA_Last",
  "GPA_Grad"
)
#raplace names
colnames(AI_data)[1:30] = variables

AI_data <- AI_data[sapply(AI_data, is.numeric)]
correlation = cor(AI_data)
corrplot(correlation, method="number", tl.cex=0.7, number.cex=0.55, type = 'lower')

#the graph
png("correlation_plot.png", width = 1200, height = 1200)
corrplot(correlation, method = "number", type = "lower", tl.cex = 1, number.cex = 0.9)
dev.off()

# discover the summery stats to get an overview
summery(AI_data)
#identify each variable
sapply(AI_data, unique)
# demographics (male vs female) for variable 2, which is gender
table(AI_data$Sex) # 58 females and 87 males
# salary (variable 8) distribution
table(AI_data$Salary) # reference: (1: USD 135-200, 2: USD 201-270, 3: USD 271-340, 
#4: USD 341-410)
# Cooralation/relationship between  total salary (variabel 8) and last_GPA (variable 29)
cor(AI_data$Salary, AI_data$GPA_Last, use = "complete.obs") # there is weak relationship between these two vaiables
# barplot Last GPA ((/4.00): (1: <2.00, 2: 2.00-2.49, 3: 2.50-2.99, 4: 3.00-3.49, 5: above 3.49))
barplot(table(AI_data$GPA_Last))
barplot(as.matrix(table(AI_data$Partner, AI_data$GPA_Last)), beside = TRUE, legend = TRUE, col = c("skyblue", "orange"), xlab = "GPA Last", ylab = "Count", main = "Distribution of GPA by Partner")


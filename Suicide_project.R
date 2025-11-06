library(data.table)
library(ggplot2)
crimestats<-fread("/Users/ushamunshi/Desktop/R Project WB/crimedata_suicides.csv")
wb_suicides <- crimestats[STATE.UT == "WEST BENGAL"]
wb_suicides <- wb_suicides[CAUSE!="Total"]

#Sort Data, and selevt using Data.Table
yearly_totals <-wb_suicides[STATE.UT == "WEST BENGAL"| Year!= "2012", .(TotalSuicides = sum(Grand.Total, na.rm = TRUE)), by = Year]
# Sort the results by Year
yearly_totals <- yearly_totals[order(Year)] [Year != "2012"]
#Data missing for 2012, hence the dip in the grapj.
plot <- ggplot(yearly_totals, aes(x = factor(Year), y = TotalSuicides)) +
  geom_line(aes(group = 1), color = "dodgerblue") +
  geom_point(color = "dodgerblue", size = 2) + 
  labs(
  title = "Total Suicides in West Bengal by Year", # <-- Updated title
  subtitle = "Based on NCRB data",x = "Year",y = "Total Number of Suicides") +
  theme_classic() 
print(plot)

gender_data <- wb_suicides[,.(
  Total_Male = sum(Total.Male, na.rm = TRUE),
    Total_Female = sum(Total.Female, na.rm = TRUE)),
  by=Year] [Year != "2012"]
gender_data <- melt(gender_data, id.vars = "Year", 
                     variable.name = "Gender", 
                    value.name = "Suicides")
ggplot(data = gender_data, aes(x= factor(Year), y= Suicides, fill = Gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Male vs Female Suicides In West Bengal",
       x = "Year",
       y= "Number Of Suicides",
       fill = "Gender") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
gender_table <- wb_suicides[,.(
  Male_Suicides = sum(Total.Male, na.rm = TRUE),
  Female_Suicides = sum(Total.Female, na.rm = TRUE)
), by=Year]
print(gender_table)
wb_suicides[,.(
  Mean_Male = mean(Total.Male, na.rm = TRUE),
  Mean_female = mean(Total.Female, na.rn = TRUE),
  Median_male = median(Total.Male, na.rm = TRUE),
  Median_female = median(Total.Female, na.rm = TRUE),
  SD_Male = sd(Total.Male, na.rm = TRUE),
  SD_Female = sd(Total.Female, na.rm = TRUE),
  Variance_Male = var(Total.Male, na.rm = TRUE),
  Variance_Female = var(Total.Female, na.rm = TRUE)
)]
occupation_wb <- wb_suicides[,.(
  Total_suicides = sum(Total.Female + Total.Male)
), by = CAUSE][Total_suicides > 12000]
ggplot(data = occupation_wb, aes(x=CAUSE, y = Total_suicides)) +
  geom_col() +
  labs(title = "Top 6 Causes Of Suicide In West Bengal",
       x = "Cause", y = "Number Of Suicides") +
  coord_flip()
rank_suicide <- crimestats[STATE.UT != "TOTAL (STATES)",
                         .(Total_Suicides = sum(Grand.Total, na.rm = TRUE)),
                         by = STATE.UT] 
print(rank_suicide)



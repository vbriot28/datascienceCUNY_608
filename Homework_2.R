# DAT608 - valerie Briot - Homework#2

library(RCurl)
library(ggplot2)
library(dplyr)
library(scales)

my_url <- getURL("https://raw.githubusercontent.com/charleyferrari/CUNY_DATA608/master/lecture1/Data/inc5000_data.csv")
my_data <- read.csv(text = my_url)

head(my_data)

str(my_data)

summary(my_data)

# Transformation of the data, visualization# 1

dt_st <- my_data %>% group_by(State) %>% summarise(count_companies = n())

dt_st_ordered <- arrange(dt_st, desc(count_companies))

# Transformation of the data, visualization# 2


third_state <- as.character(unlist(dt_st_ordered[3,1]))


dt_complete <- my_data[complete.cases(my_data),]

dt_industry <- dt_complete %>% filter(State == third_state) %>%
                               group_by(Industry) %>% 
                               summarise(avg_employees = round(mean(Employees),2), total_employees = sum(Employees), total_revenue = round(sum(Revenue),2), number_companies = n())

dt_industry = mutate(dt_industry, revenue_per_employee = round(total_revenue/total_employees,2))

str(dt_industry)


# figure 1, Companies distribution

g <- ggplot(dt_st_ordered, aes(x=reorder(State, count_companies), y=count_companies)) +
           geom_bar(stat="identity", aes(fill = count_companies), width = 0.8) +
           coord_flip()

g <- g + ggtitle("Distribution of Companies by States")

g <- g + theme(plot.title = element_text(size = 18, face = "bold", margin = margin(20,0,20,0)))

g <- g + xlab("States") + ylab("Number of Companies")

g <- g + theme( axis.title.x = element_text(face ="italic", margin = margin(20,0,0,0), colour = "darkblue", size = 14), axis.title.y = element_text(angle = 0, margin = margin(0,20,0,5), face = "italic", colour = "darkblue", size = 14))

g <- g + theme( axis.line = element_line( colour = "darkblue"))

g <- g + scale_fill_gradient2(name = "Number of\nCompanies")

g 

# figure 2, average employees by industry for 3rd state
x_ticks = c(0, 50, 100, 150, 200, 300, 400, 500, 600, 1000, 1500)
g2 <- ggplot(dt_industry, aes(x=avg_employees, y=reorder(Industry, avg_employees))) + geom_point(aes(size = number_companies), colour="blue")

g2 <- g2 + ggtitle("Average number of Employees by Industry in the State")

g2 <- g2 + theme(plot.title = element_text(size = 18, face = "bold", margin = margin(20,0,20,0)))

g2 <- g2 + scale_x_continuous(name="Average number of Employees", breaks = x_ticks, limits=c(0, 1500))

g2 <- g2 + theme(axis.title.x = element_text(face ="italic", margin = margin(20,0,0,0), colour = "darkblue", size = 14))
  
g2 <- g2 + theme(axis.title.y=element_blank()) 

g2 <- g2 + scale_size_continuous(name = "Number of\nCompanies")

g2

# figure 3, Most revenue by Employee by Industries for 3rd State
g3 <- ggplot(dt_industry, aes(x=revenue_per_employee, y=reorder(Industry, revenue_per_employee))) + geom_point(aes(size = number_companies), colour="blue")

g3 <- g3 + ggtitle("Revenue by Employee by Industry in the State")

g3 <- g3 + theme(plot.title = element_text(size = 18, face = "bold", margin = margin(20,0,20,0)))

g3 <- g3 + scale_x_continuous(name="Average number of Employees")

g3 <- g3 + theme(axis.title.x = element_text(face ="italic", margin = margin(20,0,0,0), colour = "darkblue", size = 14))

g3 <- g3 + theme(axis.title.y=element_blank()) 

g3 <- g3 + scale_x_continuous(labels = comma)

g3 <- g3 + scale_size_continuous(name = "Number of\nCompanies")

g3         
# Whole Game Project

library(tidyverse)
library(palmerpenguins)
library(ggthemes)
library(ggrepel)
library(ggplot2)
library(rlang)
library(tinytex)
library(readxl)
library(ggpubr)



file.create("Whole Game Project Script - Mark Addison.Rmd")
rmarkdown::render("Whole Game Project Script - Mark Addison.Rmd", output_format = "pdf_document")

file_path <- "C:\\Users\\MAddi\\Documents\\Whole Game Raw Data (2).xlsx"

original_data_dataframe <- read_excel(file_path)
rename_df <- original_data_dataframe |> 
  rename(Invoice_number = number,
         Length = L,
         Width = W,
         Height = H,
         Weight = Wie,
         Carrier = carier,
         Distance = dist.,
         Days = days,
         Longest_diagonal = `longest diagonal`,
         Volume = volume
         )
clean_dataframe <- rename_df |> 
  filter(!is.na(Invoice_number)) |> 
  mutate(Invoice_number = as.integer(Invoice_number),
                                  Carrier = as.factor(Carrier))

clean_dataframe
View(clean_dataframe)

data_dictionary <- list()


for (colname in names(original_data_dataframe)) {
  column_data <- original_data_dataframe[[colname]]
  title <- colname
  data_type <- class(column_data)
  
  stat_summary <- summary(column_data)
  
  missing_count <- sum(is.na(column_data))
  
  if (is.numeric(column_data)) {
    # boxplot.stats returns a list that includes outliers
    outlier_values <- boxplot.stats(column_data)$out
  } else {
    outlier_values <- NA
  }
  
  data_dictionary[[colname]] <- list(
    "Column Title" = title,
    "Data Type" = data_type,
    "Statistical Summary" = stat_summary,
    "Missing Data Count" = missing_count,
    "Outlier Data" = outlier_values
  )
}

#1: Are shorter delivery times actually more expensive?
clean_dataframe |> 
  arrange(Days)

ggplot(clean_dataframe, aes(x = Days, y = Cost)) +
  geom_point(mapping = aes(color = Carrier, shape = Carrier)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Relationship Between Cost and Delivery Time",
    x = "Delivery Time (Days)",
    y = "Cost ($)"
  )

#2: How many shipments are completed by each Carrier?
clean_dataframe |> 
  group_by(Carrier) |> 
  summarize(count = n())

ggplot(clean_dataframe, aes(x = Carrier)) +
  geom_bar(mapping = aes(fill = Carrier)) +
  labs(
    title = "Completed Shipments by Carrier",
    x = "Carrier",
    y = "Number of Shipments"
  )


#3: What is the average cost per shipment from each Carrier?
avg_cost_per_shipment <- clean_dataframe |> 
  group_by(Carrier) |> 
  summarize(
    avg_cost_per_shipment = mean(Cost, na.rm = TRUE)
  )

ggplot(avg_cost_per_shipment, aes(x = Carrier, y = avg_cost_per_shipment)) +
  geom_col(mapping = aes(fill = Carrier)) +
  labs(
    title = "Average Cost of a Shipment by Carrier",
    x = "Carrier",
    y = "Average Cost"
  )

#4: What is the cost per KG-KM for all Carriers?
avg_cost_per_KG_KM <- clean_dataframe |> 
  filter(`KG-KM` > 0) |> 
  summarize(
    total_cost = sum(Cost, na.rm = TRUE),
    total_KG_KM = sum(`KG-KM`, na.rm = TRUE),
    cost_KG_KM = total_cost / total_KG_KM,
  )

#5: What is the rank for each Carrier by cost per KG-KM
rank_cost_KG_KM <- clean_dataframe |> 
  filter(`KG-KM` > 0) |> 
  group_by(Carrier) |> 
  summarize(
    total_cost = sum(Cost, na.rm = TRUE),
    total_KG_KM = sum(`KG-KM`, na.rm = TRUE),
    cost_KG_KM = total_cost / total_KG_KM
  ) |> 
  arrange(cost_KG_KM)

ggplot(rank_cost_KG_KM, aes(x = cost_KG_KM, y = Carrier)) +
  geom_col(aes(fill = Carrier)) +
  labs(
    title = "Rank of Each Carrier by Cost per KG-KM",
    x = "Cost per KG-KM",
    y = "Carrier"
  )


#6: How does Longest Diagonal affect Cost (Use a Box and Whiskers)
summary(clean_dataframe$Longest_diagonal)
summary(clean_dataframe$Cost)

ggplot(clean_dataframe, aes(x = Longest_diagonal, y = Cost)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Relationship between Cost and Longest Diagonal",
    x = "Longest Diagonal",
    y = "Cost"
  )

test <- cor.test(clean_dataframe$Longest_diagonal, clean_dataframe$Cost)

clean_dataframe
colnames(clean_dataframe)
str(clean_dataframe)


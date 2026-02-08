library(tidyverse)
library(scales)
library(ggthemes)
library(viridis)

file.create("Import and Transform - Mark Addison.Rmd")
rmarkdown::render("Import and Transform - Mark Addison.Rmd", output_format = "pdf_document")


# import the data while changing the data types
data <- read_csv("C:\\Users\\MAddi\\Documents\\Data for Importation.csv",
                 col_types = list(Compensation = col_double()))
# cleaned names to make it easier to manipulate
data <- data |> janitor::clean_names()
View(data)

# change the hire_date column to actual dates
data <- data |> 
  mutate(
    hire_date = as.Date(hire_date, origin = "1899-12-30")
  )

# separates the names into first and last name
clean_data <- data |> 
  separate_wider_delim(
    employee_name,
    delim = ", ", 
    names = c("last_name", "first_name")
    )
  )
View(clean_data)

clean_data <- clean_data |> 
  mutate(
    tenure = round(as.numeric(today() - hire_date) / 365, 0),
    tenure_rounded = round(as.numeric(today() - hire_date) / 365, 2)
  ) |> 
  select(-c(x12, x13, x14, x15))

clean_data <- clean_data |> 
  mutate(
    compensation_percentage_increase = ((new_comp - compensation) / compensation)
  )

# not sure how to format to USD without converting it to character type
clean_data_currency <- clean_data |>
  mutate(
    compensation = dollar(compensation, accuracy = 0.01),
    new_comp = dollar(new_comp)
  )

# The relationship of department between tenure and compensation
ggplot(clean_data, aes(x = tenure, y = compensation, color = department)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~department) +
  scale_y_continuous(labels = dollar_format(accuracy = 1)) +
  labs(
    title = "Relationship of Department Tenure and Compensation",
    x = "Tenure (years)",
    y = "Compensation ($)"
  )

#  The relationship of department between tenure and compensation increase
ggplot(clean_data, aes(x = tenure, y = compensation_percentage_increase, color = department)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~department) +
  labs(
    title = "Relationship of Department Tenure and Compensation % Increase",
    x = "Tenure (years)",
    y = "Compensation Increase (%)"
  )

ggplot(clean_data, aes(x = tenure, y = compensation, color = department)) +
  geom_smooth(se = FALSE) +
  scale_y_continuous(labels = dollar_format(accuracy = 1)) +
  labs(
    title = "Relationship between Department Tenure and Compensation",
    subtitle = "With position = jitter",
    x = "Tenure (years)",
    y = "Compensation ($)"
  )

ggplot(clean_data, aes(x = tenure, y = compensation_percentage_increase, color = department)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~department)

ggplot(clean_data, aes(x = tenure, y = compensation, color = department)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~department) +
  scale_y_continuous(labels = dollar_format(accuracy = 1))


ggplot(clean_data, aes(x = tenure, y = compensation, color = department)) +
  geom_point(position = "jitter") +
  facet_grid(~department) +
  scale_y_continuous(labels = dollar_format(accuracy = 1)) +
  labs(
    title = "Faceted Scatter plot of Departments Tenure and Compensation",
    x = "Tenure (years)",
    y = "Compensation ($)"
  )


ggplot(clean_data, aes(x = tenure, y = compensation, color = department)) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(labels = dollar_format(accuracy = 1)) +
  labs(
    title = "Relationship between Tenure and Compensation",
    subtitle = "Without position = jitter",
    x = "Tenure (years)",
    y = "Compensation ($)"
  )


# Demonstrating the relationship between tenure and compensation
ggplot(clean_data, aes(x = tenure, y = compensation)) +
  geom_point(position = "jitter")

ggplot(clean_data, aes(x = tenure, y = compensation)) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(labels = dollar_format(accuracy = 1)) +
  labs(
    title = "Relationship between Tenure and Compensation",
    subtitle = "Without position = jitter",
    x = "Tenure (years)",
    y = "Compensation ($)"
  )
ggplot(clean_data, aes(x = tenure, y = compensation)) +
  geom_point(position = "jitter") +
  geom_smooth() +
  scale_y_continuous(labels = dollar_format(accuracy = 1)) +
  labs(
    title = "Relationship between Tenure and Compensation",
    subtitle = "With position = jitter",
    x = "Tenure (years)",
    y = "Compensation ($)"
  )

ggplot(clean_data, aes(x = tenure, y = compensation)) +
  geom_boxplot()

  
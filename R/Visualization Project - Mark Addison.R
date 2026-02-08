# Visualization Project

library(tidyverse)
library(palmerpenguins)
library(ggthemes)
library(ggrepel)
library(ggplot2)
library(rlang)
library(tinytex)
library(readxl)
library(ggpubr)
library(scales)
library(patchwork)

file.create("Visualization Project - Mark Addison.Rmd")
rmarkdown::render("Visualization Project - Mark Addison.Rmd", output_format = "pdf_document")

file_path <- "C:\\Users\\MAddi\\Documents\\Visualization_Project_Video_data.xlsx"

original_df <- read_excel(file_path)
df <- original_df |>
  mutate(trending_date = as.Date(trending_date, format = "%y.%d.%m"))
df
View(df)
View(original_df)

# Question 1
trending_count <- df |> 
  group_by(trending_date) |> 
  summarise(video_count = n()) |> 
  arrange(trending_date)
ggplot(trending_count, aes(x = trending_date, y = video_count)) +
  geom_col(fill = "blue", color = "red") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  labs(
    title = "Video count for each trending date",
    x = "Trending date",
    y = "Video Count"
  )
ggplot(trending_count, aes(x = trending_date, y = video_count)) +
  geom_line(color = "purple") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  labs(
    title = "Video count for each trending date",
    x = "Trending Date",
    y = "Video Count"
  )

# Question 2
most_viewed_channel <- df |> 
  group_by(channel_title) |> 
  summarise(video_count = n()) |> 
  arrange(desc(video_count)) |> 
  slice(1) |> 
  pull(channel_title)
print(most_viewed_channel)

second_trending <- df |>
  filter(channel_title == most_viewed_channel) |> 
  group_by(trending_date) |> 
  summarise(video_count = n()) |> 
  arrange(trending_date)

ggplot(second_trending, aes(x = trending_date, y = video_count)) +
  geom_line(color = "red") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  labs(
    title = "ESPN's Trending Video Data",
    x = "Trending Date",
    y = "Video Count"
  )

# Question 3
ratio_like_views <- df |> 
  mutate(
    ratio = likes / views
  )
ggplot(ratio_like_views, aes(x = reorder(channel_title, -ratio), y = ratio)) +
  geom_col() +
  labs(
    title = "Rank of Channels by Ratio of Likes to Views",
    x = "Channel Title",
    y = "Ratio of Likes to Views"
  )

ggplot(ratio_like_views, aes(x = reorder(channel_title, -ratio), y = ratio)) +
  geom_boxplot(fill = "black", color = "green", outlier.color = "red") +
  labs(
    title = "Box Plot of Likes to Views Ratio by Channel Title",
    x = "Channel Title",
    y = "Ratio of Likes to Views"
  )
  
# Question 4
ratio_like_total <- df |> 
  mutate(ratio = likes / (likes + dislikes)
  ) |> 
  arrange(ratio)

ggplot(ratio_like_total, aes(x = reorder(title, -ratio), y = ratio)) +
  geom_col() +
  labs(
    title = "Rank of Titles by ratio of likes divided by total likes and dislikes",
    x = "Title",
    y = "Ratio of likes divded by like plus dislikes"
  )
  
# Question 5
disabled_videos <- df |> 
  filter(ratings_disabled == TRUE) |> 
  group_by(channel_title) |> 
  summarise(video_count = n())
print(disabled_videos)
View(disabled_videos)
ggplot(disabled_videos, aes(x = reorder(channel_title, -video_count), y = video_count)) +
  geom_col(fill = "blue") +
  coord_flip() +
  labs(
    title = "Amount of Disabled Ratings for Channels",
    x = "Video Count",
    y = "Channel Title"
  )

# Question 6
Q6_with_outliers <- ggplot(df, aes(x = comments_disabled, y = likes)) +
  geom_boxplot(outlier.color = "red") +
  scale_y_continuous(
    labels = label_number(scale = 1e-6, suffix = " mil"),
    breaks = seq(0, 6000000, by = 1000000)
  ) +
  scale_x_discrete(labels = c("FALSE" = "Enabled", "TRUE" = "Disabled")) +
  labs(
    title = "Difference Between in Video Likes with Comments Setting",
    x = "Comment Section Availability",
    y = "Likes"
  )

Q6_no_outliers <- ggplot(df, aes(x = comments_disabled, y = likes)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(
    breaks = seq(0, 100000, by = 10000),
    limits = c(0, 50000),
    labels = label_comma()
  ) +
  scale_x_discrete(labels = c("FALSE" = "Enabled", "TRUE" = "Disabled")) +
  labs(
    title = "Difference Between in Video Likes with Comments Setting",
    x = "Comment Section Availability",
    y = "Likes"
  )
Q6_no_outliers + Q6_with_outliers

# Question 7
funny_tags <- df |> 
  mutate(funny_tag = if_else(grepl("funny", tags, ignore.case = TRUE), "Funny Tag", "Not Funny Tag"))

ggplot(funny_tags, aes(x = funny_tag, y = likes)) +
  geom_col(fill = "green") +
  scale_y_continuous(
    labels = label_number(scale = 1e-6, suffix = " mil")
  ) +
  labs(
    title = "Difference in Likes between Funny and Not Funny Tags",
    x = "Tags",
    y = "Likes"
  )

ggplot(funny_tags, aes(x = likes, y = funny_tag)) +
  geom_boxplot(outlier.shape = NA) +
  scale_x_continuous(
    breaks = seq(0, 100000, by = 10000),
    limits = c(0, 50000),
    labels = label_comma()
  ) +
  labs(
    title = "Difference in Likes between Funny and Not Funny Tags",
    x = "Likes",
    y = "Tags"
  )

# Question 8
ratio_dislikes_views <- df |> 
  mutate(
    ratio = dislikes / views
  )
ratio_like_less_dislikes <- df |> 
  mutate(
    ratio = ((likes - dislikes) / views)
  ) 
  
Q7a <- ggplot(ratio_like_views, aes(x = reorder(channel_title, ratio), y = ratio)) +
  geom_col(fill = "red") +
  labs(
    title = "Rank of Channels by Ratio of Likes to Views",
    x = "Channel Title",
    y = "Ratio of Likes to Views"
  ) +
  coord_flip()
Q7b <- ggplot(ratio_dislikes_views, aes(x = reorder(channel_title, ratio), y = ratio)) +
  geom_col(fill = "purple") +
  labs(
    title = "Channel Rank by Ratio of Dislikes to Views",
    x = "Channel Titles",
    y = "Ratio of Dislikes to Views"
  ) +
  coord_flip()

Q7c <- ggplot(ratio_like_less_dislikes, aes(x = reorder(channel_title, ratio), y = ratio)) +
  geom_col(fill = "blue") +
  labs(
    title = "Channel Rank by Ratio of (Likes - Dislikes)/Views",
    x = "Channel Titles",
    y = "Ratio of Likes minus Dislikes Divded by Views"
  ) +
  coord_flip()

(Q7a | Q7b) / Q7c



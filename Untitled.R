## Outliers in the Total Volume of food items 

```{r, message=FALSE, collapse=TRUE,  warning=FALSE, results='asis', cache=FALSE}
outlier_points = baseline_data |>
  select(participant_id, food_item_proc_std, total_food_volume) |>
  distinct() |>
  group_by(food_item_proc_std) |>
  mutate(Q1 = quantile(total_food_volume, 0.25, na.rm = TRUE),
         Q3 = quantile(total_food_volume, 0.75, na.rm = TRUE),
         IQR = Q3 - Q1,
         lower_bound = Q1 - 2 * IQR,
         upper_bound = Q3 + 2 * IQR,
         `median total_food_volume` = median(total_food_volume, na.rm = TRUE) |> round(2),
         `sd total_food_volume` = sd(total_food_volume, na.rm = TRUE)  |> round(2),
         `total records` = n()) |>
  ungroup() |>left_join(
    baseline_data |> select(participant_id, ingredient_name, ingredient_name_simplified)
  )

outlier_points = outlier_points |>
  filter(total_food_volume < lower_bound | total_food_volume > upper_bound) |>
  group_by(food_item_proc_std) |>
  mutate(
    `no. of outliers` = n(),
    `% outliers` = (100 * `no. of outliers` / `total records`) |> round(2)
  ) |>
  ungroup() |>
  select(participant_id, food_item_proc_std, ingredient_name, ingredient_name_simplified, 
         total_food_volume, `median total_food_volume`, `sd total_food_volume`,
         `no. of outliers`, `total records`, `% outliers`) |>
  arrange(food_item_proc_std) |>
  distinct()

p = ggplot(outlier_points, aes(x = participant_id)) +
  geom_bar(stat = "count", fill = "lightblue", col = "black") +
  labs(title = "Histogram of Participant IDs and their Recorded Outliers", 
       x = "Participant ID", 
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plotly_obj = ggplotly(p)
html_output = tags$div(plotly_obj)
browsable(tagList(html_output))
```

There were `r outlier_points |> nrow()` records where the total food volume varied significantly from similar food items. These records were from `r outlier_points$participant_id |> unique() |> length()` participants.

## Remove participants with inconsistent food volumes 

```{r, message=FALSE, collapse=TRUE,  warning=FALSE, results='asis', cache=FALSE}
p = outlier_points$participant_id |> table() |> 
  as.data.frame() |> 
  rename(participant_id = Var1, `No. of Meals` = Freq) |>
  ggplot(aes(x = `No. of Meals`)) +
  ggtitle("Distribution of No. of Meals with Outlier Volumes") +
  geom_histogram(bins = 9, fill = "lightblue", col = "black") +
  theme_minimal()

plotly_obj = ggplotly(p)
html_output = tags$div(plotly_obj)
browsable(tagList(html_output))

outlier_points |>
  datatable(options = list(pageLength = 15, 
                           scrollX = TRUE,
                           dom = 'Bfrtip'), filter = 'top',  
            extensions = 'Buttons',
            caption = "Outlier records for Total food volume",) 

baseline_data = baseline_data |>
  filter(!participant_id %in% outlier_points$participant_id)
```

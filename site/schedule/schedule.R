library(lubridate)
library(dplyr)
library(tibble)
library(tidyverse)
library(glue)
library(knitr)
library(purrr)

# Parameters
start_date <- ymd("2026-01-05")
end_date <- ymd("2026-04-10")
reading_week <- interval(ymd("2026-02-16"), ymd("2026-02-21"))  # Feb 16–20

# Holidays (specific dates)
holidays <- ymd(c("2026-04-03", "2026-04-06"))

# Generate class dates (Mon & Wed)
class_dates <- seq(start_date, end_date, by = "1 day") %>%
	keep(~ wday(.x, label = TRUE) %in% c("Mon", "Wed"))

# Assign slide titles based on special dates
slide_titles <- case_when(
	class_dates %within% reading_week ~ "READING WEEK",
	class_dates %in% holidays ~ "HOLIDAY",
	TRUE ~ "Slides"
)

# Create slide links only for regular topics
slide_paths <- if_else(
	slide_titles %in% c("READING WEEK", "HOLIDAY"),
	slide_titles,
  glue("[{slide_titles}]()")
)

# Build schedule table
schedule <- tibble(
	Date = format(class_dates, "%d %b %y"),
  Topic = " ",
	Material = slide_paths,
	Deadlines = ""
)

# Output as markdown table
schedule_md <- knitr::kable(schedule, format = "markdown")
writeLines(schedule_md, "schedule/schedule-table.md")


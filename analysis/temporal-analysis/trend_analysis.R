library(dplyr)
library(ggplot2)
library(lubridate)

# ==========================================================
# LOADING DATA
# ==========================================================
# we are looking at the data by year and considering popularity as kudos and comments
# popularity is kudos + comments because these statistics typically mean that the user has 
# finished reading the story and actively engaged with it, compared to other statistics which
# are less reliable and don't imply popularity as strongly

tag_stories <- read.csv("ao3_filtered_2015_2020.csv") %>%
  mutate(
    date = as.Date(date),
    year = year(date),
    popularity = kudos + comments
  )
# ==========================================================
# YEARLY AGGREGATION
# ==========================================================
# calculate yearly stats for each tag
# only consider tags with at least 50 stories in that year for meaningful analysis
# these stats are necessary for growth calculations
yearly_stats <- tag_stories %>%
  group_by(TagName, TagType, year) %>%
  filter(n_distinct(storyId) >= 50) %>%
  group_by(TagName, TagType, year, .add = FALSE) %>%
  summarize(
    total_stories = n_distinct(storyId),
    total_popularity = sum(popularity, na.rm = TRUE),
    avg_popularity = mean(popularity, na.rm = TRUE),
    total_kudos = sum(kudos, na.rm = TRUE),
    total_comments = sum(comments, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(TagName, year)
# ==========================================================
# YEAR-OVER-YEAR GROWTH METRICS
# ==========================================================
# calculate growth metrics (year-over-year)
# year-over-year is calculated as (current year - previous year) / previous year
# and is used to assess how tags are trending over time
# lag function is used to get previous year's values for comparison
# filter out first year for each tag since growth cannot be calculated
yearly_growth <- yearly_stats %>%
  group_by(TagName) %>%
  arrange(year) %>%
  mutate(
    lag_total_popularity = lag(total_popularity),
    lag_total_stories = lag(total_stories),

    # year-over-year growth rates
    popularity_growth = ifelse(!is.na(lag_total_popularity) & lag_total_popularity > 0,
                               (total_popularity - lag_total_popularity) / lag_total_popularity,
                               NA_real_),
    story_growth = ifelse(!is.na(lag_total_stories) & lag_total_stories > 0,
                          (total_stories - lag_total_stories) / lag_total_stories,
                          NA_real_),

    # absolute changes
    popularity_change = total_popularity - lag(total_popularity),
    story_change = total_stories - lag(total_stories),

    # relative to average growth
    avg_growth = mean(popularity_growth, na.rm = TRUE), 
    growth_vs_avg = ifelse(!is.na(avg_growth) & avg_growth != 0,
                           popularity_growth / avg_growth,
                           NA_real_),
  ) %>%
  filter(!is.na(popularity_growth)) %>%
  ungroup() %>%
  select(-lag_total_popularity, -lag_total_stories)
# ==========================================================
# RANKING OF TAGS PER YEAR BASED ON TRENDING METRICS
# ==========================================================
yearly_trending <- yearly_growth %>%
  group_by(year) %>%
  # rank within each year
  mutate(
    growth_rank = rank(-popularity_growth),
    popularity_rank = rank(-total_popularity),

    # combined trending score (weights growth more for trending)
    # 65% growth, 35% popularity
    trending_score = (0.65 * (1/growth_rank))
                   + (0.35 * (1/popularity_rank))
  ) %>%
  # get top N trending per year
  arrange(year, desc(trending_score)) %>%
  group_by(year) %>%
  mutate(
    yearly_rank = row_number(),
    is_top_trending = yearly_rank <= 20  # top 20 per year
  ) %>%
  ungroup()
# ==========================================================
# TAG-LEVEL TREND SUMMARY (ACROSS YEARS)
# ==========================================================
tag_trends <- yearly_growth %>%
  group_by(TagName, TagType) %>%
  summarize(
    most_recent_year = max(year, na.rm = TRUE),

    # current stats - use most recent year as "current"
    current_popularity = total_popularity[which.max(year)],
    current_stories = total_stories[which.max(year)],
    
    # growth metrics across years
    avg_popularity_growth = mean(popularity_growth, na.rm = TRUE),
    recent_growth = popularity_growth[which.max(year)],
    growth_volatility = sd(popularity_growth, na.rm = TRUE),
    
    avg_absolute_growth = mean(popularity_change, na.rm = TRUE),

    # consistency
    positive_years = sum(popularity_growth > 0, na.rm = TRUE),
    total_years = n(),
    consistency_score = positive_years / total_years,
    
    # peak stasts
    peak_popularity = max(total_popularity, na.rm = TRUE),
    peak_year = year[which.max(total_popularity)],

    # growth significance (growth rate * log of size)
    growth_significance = ifelse(
      avg_popularity_growth > 0,
      log1p(pmin(avg_popularity_growth, 5)) * log10(current_popularity + 1),  # Cap growth at 500%
      0
    ),
    
    .groups = 'drop'
  ) %>%
  # filter for meaningful analysis (need at least 2 years of data)
  filter(total_years >= 2, current_popularity > 0)
# ==========================================================
# TAG CLASSIFICATION BASED ON TRENDS PATTERNS
# ==========================================================
tag_classification <- tag_trends %>%
  mutate(
    trend_category = case_when(
      # Viral: very high growth (>100%) with high popularity (>10k)
      avg_popularity_growth > 1.0 & current_popularity > 30000 ~ "Viral",
      
      # Growing: good growth (>30%) with big popularity
      avg_popularity_growth > 0.3 & current_popularity > 5000 ~ "Strong Growth",
      avg_popularity_growth > 0.2 & current_popularity > 2000 ~ "Moderate Growth",
      avg_popularity_growth > 0.1 & current_popularity > 1000 ~ "Slow Growth",
      
      # Established: large and stable with low growth
      current_popularity > 100000 & abs(avg_popularity_growth) < 0.15 ~ "Extremely Established",
      current_popularity > 50000 & abs(avg_popularity_growth) < 0.2 ~ "Established",
      current_popularity > 10000 & abs(avg_popularity_growth) < 0.25 ~ "Established Niche",
      
      # Declining: negative growth
      avg_popularity_growth < -0.2 & current_popularity > 5000 ~ "Major Decline",
      avg_popularity_growth < -0.1 & current_popularity > 2000 ~ "Declining",
      
      # New: only recent years with positive growth
      total_years <= 3 & avg_popularity_growth > 0.2 ~ "New",

      # Volatile: high growth volatility
      growth_volatility > 0.5 & total_years >= 3 ~ "Volatile",
      
      # default category
      TRUE ~ "Stable"
    ),

    # composite trend score
    growth_percentile = percent_rank(avg_popularity_growth),
    popularity_percentile = percent_rank(current_popularity),
    consistency_percentile = percent_rank(consistency_score),
    
    # 50% growth, 30% popularity, 20% consistency
    trend_score = (
      growth_percentile * 50 +
      popularity_percentile * 30 +
      consistency_percentile * 20
    )
  ) %>%
  arrange(desc(trend_score))
# ==========================================================
# RECOMMENDATION FUNCTION
# ==========================================================

# function to recommend tags based on temporal patterns
# strategy can be "trending", "similar_trajectory", or "growing"
recommend_by_temporal <- function(seed_tags, 
                                 n_recommendations = 10,
                                 strategy = "trending",  # "trending", "similar_trajectory", "growing"
                                 recency_weight = 2) {
  
  # gather stories that include the seed tags
  seed_stories <- tag_stories %>%
    filter(TagName %in% seed_tags) %>%
    pull(storyId) %>%
    unique()
  
  if(length(seed_stories) == 0) {
    message("No stories found with those tags")
    return(NULL)
  }
  
  # find co-occurring tags in those stories
  candidate_tags <- tag_stories %>%
    filter(storyId %in% seed_stories,
           !TagName %in% seed_tags) %>%
    group_by(TagName, TagType) %>%
    summarize(
      cooccurrence = n_distinct(storyId),
      recent_count = sum(year >= 2019, na.rm = TRUE),
      avg_year = mean(year, na.rm = TRUE),
      avg_popularity = mean(popularity, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    filter(cooccurrence >= 5)
  
  # join with tag classification to get trend info
  recommendations <- candidate_tags %>%
    left_join(
      tag_classification %>% 
        select(TagName, TagType, trend_category, trend_score, 
               recent_growth, consistency_score, current_popularity),
      by = c("TagName", "TagType")
    ) %>%
    filter(!is.na(trend_category))
  
  # apply strategy-specific scoring
  if(strategy == "trending") {
    # prioritize viral and growing tags
    recommendations <- recommendations %>%
      mutate(
        strategy_score = case_when(
          trend_category %in% c("Viral", "Strong Growth", "Moderate Growth", "Slow Growth") ~ trend_score * 2,
          trend_category == "New" ~ trend_score * 1.5,
          TRUE ~ trend_score
        ) + (recent_count * recency_weight)
      )
  } else if(strategy == "similar_trajectory") {
    # find tags with similar growth patterns to seed tags
    seed_patterns <- tag_classification %>%
      filter(TagName %in% seed_tags)
    
    recommendations <- recommendations %>%
      mutate(
        # higher score if trend category matches any of the seed tags
        strategy_score = cooccurrence * 
          ifelse(trend_category %in% seed_patterns$trend_category, 2, 1)
      )
  } else if(strategy == "growing") {
    # focus on consistently growing tags
    recommendations <- recommendations %>%
      filter(trend_category %in% c("Strong Growth", "Moderate Growth", "Slow Growth", "Viral", "New")) %>%
      mutate(
        strategy_score = consistency_score * trend_score + (recent_count * recency_weight)
      )
  }
  
  # return top recommendations
  recommendations %>%
    arrange(desc(strategy_score)) %>%
    select(TagName, TagType, trend_category, cooccurrence, 
           recent_count, trend_score, strategy_score) %>%
    head(n_recommendations)
}
# ==========================================================
# TESTING RECOMMENDATION FUNCTIONS
# ==========================================================
test_tags <- c("Harry Potter", "Draco Malfoy Travel")

recs_all <- list(
  Trending = recommend_by_temporal(test_tags, strategy = "trending"),
  Growing = recommend_by_temporal(test_tags, strategy = "growing"),
  SimilarTrajectory = recommend_by_temporal(test_tags, strategy = "similar_trajectory")
)

recs_all
# ==========================================================
# VISUALIZATIONS
# ==========================================================
# distribution of tag trend categories
p1 <- tag_classification %>%
  count(trend_category) %>%
  ggplot(aes(x = reorder(trend_category, n), y = n, fill = trend_category)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Distribution of Tag Trend Categories",
    x = "Category",
    y = "Number of Tags"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(p1)

# top trending tags over time
top_tags <- tag_classification %>%
  slice_max(order_by = trend_score, n = 10) %>%
  pull(TagName)

p2 <- yearly_stats %>%
  filter(TagName %in% top_tags) %>%
  ggplot(aes(x = year, y = total_popularity, color = TagName)) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  labs(
    title = "Top 10 Trending Tags: Popularity Over Time",
    x = "Year",
    y = "Total Popularity"
  ) +
  theme_minimal()

print(p2)

# growth rate vs consistency
p3 <- tag_trends %>%
  filter(total_years >= 3) %>%
  ggplot(aes(x = consistency_score, y = avg_popularity_growth, 
             color = log(current_popularity), size = current_stories)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Tag Growth Patterns: Consistency vs Growth Rate",
    x = "Consistency Score (% years with positive growth)",
    y = "Average Popularity Growth Rate",
    color = "Log(Current\nPopularity)",
    size = "Current\nStories"
  ) +
  theme_minimal()

print(p3)

# composition of top 20 trending tags by tag type each year
p4 <- yearly_trending %>%
  filter(is_top_trending) %>%
  count(year, TagType) %>%
  ggplot(aes(x = year, y = n, fill = TagType)) +
  geom_col(position = "stack") +
  labs(
    title = "Composition of Top 20 Trending Tags by Type",
    x = "Year",
    y = "Number of Tags",
    fill = "Tag Type"
  ) +
  theme_minimal()

print(p4)

# top 5 trending tags per year
top_trending <- yearly_trending %>%
  filter(is_top_trending) %>%
  select(year, TagName, TagType, trending_score, popularity_growth) %>%
  arrange(year, desc(trending_score))

top5_per_year <- top_trending %>%
  group_by(year) %>%
  slice_max(order_by = trending_score, n = 5) %>%
  ungroup()

p5 <- top5_per_year %>%
  ggplot(aes(x = reorder(TagName, trending_score), 
             y = trending_score, 
             fill = TagName)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~year, scales = "free_y") +
  labs(
    title = "Top 5 Trending Tags per Year",
    x = "Tag",
    y = "Trending Score"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(p5)

# ==========================================================
# SUMMARY STATISTICS
# ==========================================================
# summary table of tag classifications, sorted by trend score
trend_summary_table <- tag_classification %>%
  select(TagName, TagType, trend_category, trend_score,
         avg_popularity_growth, recent_growth, current_popularity, growth_volatility) %>%
  arrange(desc(trend_score))
print(trend_summary_table, n = 10)

# shows which tags have been top trending most frequently
yearly_trending %>%
  filter(is_top_trending) %>%
  count(TagName, sort = TRUE) %>%
  head(15)
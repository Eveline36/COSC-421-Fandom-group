library(data.table)
library(igraph)

tags_df <- fread("filtered_tags.csv")
links_df <- fread("filtered_taglinks.csv")
stories_df <- fread("filtered_stories.csv")


# Reasoning behind the eigen centrality. Eigen centrality is the second most useful centrality measure that gives us key insights.
# Eigen centrality measures how influential the neighbors are, and in the context of AO3 through exploration of data, 
# we discovered that few Tags are shared across large number of stories. We suspect that authors of stories may use highly popular tags
# to attract attention to their stories. Given our graph, and eigen centrality of stories in our graph, it means that the algorithm highly favors 'mainstream core'
# which can be thought of as stories having a very popular Tag. One major downside of this approach is that for stories without 'highly popular'
# or 'spammy' tags, eigen centrality assigns a very low score, thus highly favoring the mainstream stories, and supressing the 'hidden gems'.
# Nonetheless, eigen centrality provides much more accurate representation for the state of our network
# when compared to other centrality measures such as Closeness, Betweenness and Degree Centrality



# Since R doesn't explicitly define Bi-Partite graphs, I set Stories as S_ nodes 
# Tags with T_ nodes and load them into dataframe
stories_df[, node_id := paste0("S_", id)]
tags_df[, node_id    := paste0("T_", id)]

# We so the same for the TagLink file so they match
links_df[, story_node := paste0("S_", storyId)]
links_df[, tag_node   := paste0("T_", tagId)]

# We need to create edges, and it makes sense to only use TagLinks file, since
# We don't care about attributes such as comments, kudos, tagName to create our graph initially
# We only use the new prefixed columns, ie T_ and S_
edges <- links_df[, .(story_node, tag_node)]

# We create our graph
g <- graph_from_data_frame(d = edges, directed = FALSE)

# We can identify Tags easily because we added the "T_" prefix.
# TRUE = Tag, FALSE = Story (Sorry Nat for reversing the logic)
V(g)$type <- grepl("^T_", V(g)$name)

deg <- degree(g)

# Now we only care about Tag Degrees
# (It's okay if a Story has many tags, but a Tag shouldn't have too many stories).
tag_nodes_indices <- which(V(g)$type == TRUE)
tag_degrees <- deg[tag_nodes_indices]

# Tags appearing more than 10000 times or less than 3 times are removed.
cutoff_value_high <- 10000
cutoff_value_low <- 2

# We look for nodes that are TAGS AND have degree > cutoff
godzilla_nodes <- V(g)[V(g)$type == TRUE & (deg > cutoff_value_high | deg <= cutoff_value_low)]

# 5. Now g_clean is a cleaned version of our graph, after removing highly popular Tags
g_clean <- delete_vertices(g, godzilla_nodes)

# We calculate Eigen centrality for the whole graph
eig_result <- eigen_centrality(g_clean, directed = FALSE, scale = TRUE)
eig_scores <- eig_result$vector
# We are only concerned with Story nodes when it comes to Eigen centrality
story_indices <- grep("^S_", names(eig_scores))
story_eig_values <- eig_scores[story_indices]
story_eig_ids <- names(story_eig_values)
analysis_dt <- data.table(
  node_id = story_eig_ids,
  eigen_centrality = story_eig_values
)
# this is where we merge kudos, comments, story name with its centrality and id
analysis_dt[, clean_id := gsub("^S_", "", node_id)]
stories_df[, id := as.character(id)]
analysis_dt[, clean_id := as.character(clean_id)]
full_data <- merge(analysis_dt, stories_df, by.x = "clean_id", by.y = "id")

# Here we calculate the popularity score
# Popularity = Log of (comments and kudos + 1) + 1
full_data[, pop_score := log(kudos + comments + 1) + 1]
# We define hidden gems as Popularity / Eigen structure
# we added a tiny epsilon to eigen to avoid division by zero if a story is totally isolated
full_data[, gem_score := pop_score / (eigen_centrality + 1e-6)]
# this part was done using trial and error where we decided to take medians of both the centrality scores
# and popularity scores. However, it holds true only for cleaned version of our graph where extreme high degree Tags
# and very low degree Tags are removed. 
med_eig <- median(full_data$eigen_centrality)
med_pop <- median(full_data$pop_score)
# We have 4 categories: 
# Fandom Classic: High Centrality and High Popularity: These are Hits. So these stories are safe for us
# to recommend to newly joined users without any knowledge about their preferences
# Hidden Gem: Low Centrality and High Popularity: These stories are niche, and cater to specific communities who may be not well connnected with the mainstream.
# Mainstream/Generic: High Centrality and Low Popularity: These stories can be thought of as alternative versions of popular stories, or their 'knock-offs'.
# We wouldn't recommend these stories as witnessed by their low popularity and their 'averageness'.
# The Ghost: These stories seem to be the majority as can be seen by their degree distributions.
# they are Low Centrality and Low Popularity: These stories can be due to 3 reasons. Either they are newly added, which offers explanation to 'cold-start' problem.
# They can also be 'not-popular' because of the inherent 'power-law' that AO3 Tags and Stories share. 


full_data[, category := fcase(
  eigen_centrality >= med_eig & pop_score >= med_pop, "Fandom Classic",
  eigen_centrality < med_eig  & pop_score >= med_pop, "Hidden Gem",
  eigen_centrality >= med_eig & pop_score < med_pop,  "Mainstream/Generic",
  eigen_centrality < med_eig  & pop_score < med_pop,  "The Ghost"
)]


# this is a helper function to print pretty results
print_category_top_5 <- function(data, cat_name, sort_by_col) {
  cat("\n=======================================================\n")
  cat("CATEGORY:", cat_name, "\n")
  cat("-------------------------------------------------------\n")
  subset <- data[category == cat_name]
  subset <- subset[order(-get(sort_by_col))]
  top_5 <- head(subset, 5)
  print(top_5[, .(title, kudos, eigen_centrality, pop_score, gem_score)])
}

# --- 1. THE HIDDEN GEMS (Low Eigen, High Pop) ---
# Sorted by Gem Score (Highest ratio of love to isolation)
print_category_top_5(full_data, "Hidden Gem", "gem_score")
# Interpretation: These are unique stories (rare tags) that people love.

# --- 2. THE FANDOM CLASSICS (High Eigen, High Pop) ---
# Sorted by Popularity (The biggest hits)
print_category_top_5(full_data, "Fandom Classic", "pop_score")
# Interpretation: Ideally positioned in the network AND highly rated.

# --- 3. THE MAINSTREAM / GENERIC (High Eigen, Low Pop) ---
# Sorted by Eigenvector (Most central)
print_category_top_5(full_data, "Mainstream/Generic", "eigen_centrality")
# Interpretation: Stories that use "perfect" tags but have few readers.
# Often new stories, or "Tag Spammers".

# --- 4. THE GHOSTS (Low Eigen, Low Pop) ---
# Sorted by Gem Score (ascending - looking for the bottom)
# These have low quality AND low structure.
print_category_top_5(full_data, "The Ghost", "eigen_centrality")
plot_sample <- full_data[sample(.N, min(1000, .N))]

# Color based on category
colors <- c("Fandom Classic" = "red", 
            "Hidden Gem" = "blue", 
            "Mainstream/Generic" = "orange", 
            "The Ghost" = "grey")

plot(
  x = plot_sample$eigen_centrality, 
  y = plot_sample$pop_score,
  col = colors[plot_sample$category],
  pch = 19, # Solid dots
  cex = 0.6, # Dot size
  main = "Story Landscape: Structure vs. Popularity",
  xlab = "Eigenvector Centrality (Mainstream-ness)",
  ylab = "Popularity Score (Log Kudos)"
)
abline(v = med_eig, col = "black", lty = 2) # Vertical line
abline(h = med_pop, col = "black", lty = 2) # Horizontal line

legend("topright", legend = names(colors), col = colors, pch = 19, cex=0.8)


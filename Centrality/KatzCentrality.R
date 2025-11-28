# Katz Centrality

# As far as Centrality Measures are concerned, we found the Katz Centrality to have the most accurate performance for exploration of our graph,
# As well as, for the basis of our recommendation algorithm
# Compared to Eigen centrality, Katz Centrality assigns lower weights to neighboring nodes with high degrees. Therefore, based on the value of alpha (a),
# We noticed that it represents larger population of Stories that are not associated with Tags with very high degrees.
# Thus we had an idea for a small easter egg as part of our recommender algorithm that also provides some insight on the distribution of stories in AO3.

# Our recommender algorithm consists of 2 simple components: Popularity, and Katz centrality
# Initially, we clean our bipartite graph by removing Tags with more than 10 000 degrees, and Tags with less than or equal to 2 degrees. 
# Then we define the popularity as Logarithm of Kudos and Comments combined + 1, since we don't want errors associated with Log(0),
# and we add 1 to avoid 0 value. 
# For Katz centrality, we decided to use a dataframe, to iteratively look for degrees of neighbors at each step to avoid RAM overload issue
# that we faced with the traditional implementation in a function.

# Therefore, after retrieving values for both the Katz centrality of Story nodes, and their respective popularity, as decsribed above, we
# built a confusion matrix. In this matrix, we define categories as follows:
# High Centrality and High Popularity: These are Hits, or Stories that are considered AO3 Fandom Classics. So these stories are safe for us
# to recommend to newly joined users without any knowledge about their preferences
# High Centrality and Low Popularity: These stories can be thought of as alternative versions of popular stories, or their 'knock-offs'.
# We wouldn't recommend these stories as witnessed by their low popularity.
# Low Centrality and High Popularity: These stories are niche, and cater to specific communities who may be not well connnected with the mainstream.
# They can be thought of as 'hidden jems', however, more research needs to be done to make strong conclusions.
# Low Centrality and Low Popularity: These stories can be due to 3 reasons. Either they are newly added, which offers explanation to 'cold-start' problem.
# They can also be 'not-popular' because of the inherent 'power-law' that AO3 Tags and Stories share. 


# Our goal as can be seen above is to recommend hidden gems, and Fandom Classics by using Katz Centrality. 
# As a result of running Katz centrality, we noticed that some of the recommendations were Fandoms titled in other languages than English.
# Whereas Eigen centrality output for recommendations were all in English, and highly popular stories.

library(Matrix)
library(data.table)
library(igraph)

# We load CSV files
tags_df <- fread("filtered_tags.csv")
links_df <- fread("filtered_taglinks.csv")
stories_df <- fread("filtered_stories.csv")


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


# Here as described above, we realized that computing Katz centrality manually, and iteratively from
# a Matrix was the only solution that would work within confines of an available RAM. 

# Therefore, we setup the sparse matrix
A <- as_adjacency_matrix(g_clean, sparse = TRUE)
# We need to calculate safe value for alpha so that our algorithm converges
# Here the constraint for alpha is < 1/lambda_max
eigen_val <- eigen_centrality(g_clean, directed = FALSE, scale = FALSE)$value
alpha_val <- 0.85 * (1 / eigen_val)

# We assign a start to everyone at 1.0, this is our beta
n_nodes <- vcount(g_clean)
katz_scores <- rep(1, n_nodes)
beta <- 1 
# For now we decided to limit with 20 iterations
for (i in 1:20) {
  # Formula: New = Alpha * (Neighbors) + Beta
  katz_scores <- alpha_val * (A %*% katz_scores) + beta
  katz_scores <- as.vector(katz_scores)
}
# 4. Now we map names of stories back to their respective Katz scores
names(katz_scores) <- V(g_clean)$name

# Since our stories start with S_ prefix, we can filter them out 
story_indices <- grep("^S_", names(katz_scores))
story_katz_values <- katz_scores[story_indices]
story_node_ids <- names(story_katz_values)
# this is where we create our 'confusion matrix' but instead of classifying,
# we assign quadrants to 2 values of Katz Centrality and 2 values of Popularity 
analysis_dt <- data.table(
  node_id = story_node_ids,
  katz_centrality = story_katz_values
)
# following steps are for data cleaning
analysis_dt[, clean_id := gsub("^S_", "", node_id)]
stories_df[, id := as.character(id)]
full_data <- merge(analysis_dt, stories_df, by.x = "clean_id", by.y = "id")
# Popularity = Log of (comments and kudos + 1) + 1
full_data[, pop_score := log(kudos + comments + 1) + 1]

# Gem Score = Popularity / Centrality
# High Score = Loved by users, but "ignored" by the main graph structure
full_data[, gem_score := pop_score / katz_centrality]
# this part was done using trial and error where we decided to take medians of both the centrality scores
# and popularity scores. However, it holds true only for cleaned version of our graph where extreme high degree Tags
# and very low degree Tags are removed. 
med_katz <- median(full_data$katz_centrality)
med_pop  <- median(full_data$pop_score)
full_data[, category := fcase(
  katz_centrality >= med_katz & pop_score >= med_pop, "Fandom Classic",
  katz_centrality < med_katz  & pop_score >= med_pop, "Hidden Gem",
  katz_centrality >= med_katz & pop_score < med_pop,  "Mainstream/Generic",
  katz_centrality < med_katz  & pop_score < med_pop,  "The Ghost"
)]
# We define Ghost as stories which have low Katz Centrality and Popularity. 
# Also, one of the limitations of this research is that it is static instead of dynamic. Therefore,
# we do not have historic timeline to investigate how a certain story gained in popularity/lost in popularity
# over time. However, we may use groups or genres of stories as in how saturated/popular the clusters have become over some time period.
# These time related aspect will be discussed on the last page that is dedicated to changes in timeline. 



# this is a helper function to print results cleanly
print_quadrant <- function(data, cat_name, sort_col) {
  cat("\n=======================================================\n")
  cat("CATEGORY:", cat_name, "\n")
  cat("-------------------------------------------------------\n")
  subset <- data[category == cat_name]
  subset <- subset[order(-get(sort_col))]
  print(head(subset, 5)[, .(title, kudos, katz_centrality, gem_score)])
}

# 1. HIDDEN GEMS (The Recommendations)
# Low Katz (Niche/Isolated) + High Kudos
print_quadrant(full_data, "Hidden Gem", "gem_score")

# 2. FANDOM CLASSICS
# High Katz (Central) + High Kudos
print_quadrant(full_data, "Fandom Classic", "pop_score")

# 3. MAINSTREAM / GENERIC
# High Katz (Central) + Low Kudos (The "Tag Salads" or "New Arrivals")
print_quadrant(full_data, "Mainstream/Generic", "katz_centrality")

# 4. THE GHOSTS
# Low Katz + Low Kudos (Truly obscure)
# Sorted by Katz (lowest first, so we reverse the sort order logic slightly)
ghosts <- full_data[category == "The Ghost"][order(katz_centrality)]
cat("\nCATEGORY: The Ghost (Deepest Cuts)\n")
print(head(ghosts, 5)[, .(title, kudos, katz_centrality)])
# this is a sample plot for visualiazation purposes
plot_sample <- full_data[sample(.N, min(2000, .N))]

# Define Colors
colors <- c("Fandom Classic" = "red",       
            "Hidden Gem" = "blue",         
            "Mainstream/Generic" = "orange",   
            "The Ghost" = "grey")          

# main plot
plot(
  x = log(plot_sample$katz_centrality),
  y = plot_sample$pop_score,
  col = colors[plot_sample$category],
  pch = 19, cex = 0.6,
  main = "Story Landscape (Katz Edition)",
  xlab = "Log(Katz Centrality) - Structural Reach",
  ylab = "Popularity Score (Log Kudos)"
)
abline(v = log(med_katz), lty=2)
abline(h = med_pop, lty=2)
legend("topleft", legend = names(colors), col = colors, pch=19, cex=0.8)


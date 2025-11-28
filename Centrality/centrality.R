# Motivation behind centrality measures for our topic.
# Since our topic is studying how stories and tags hosted on AO3 are related to one another, we decided
# to use multiple quantitative approaches to give a qualitative evaluation to our dataset.
# Since we do not have user-specific data, we decided to derive characteristics of individual stories,
# as well as genres/communities that stories form based on structural data.
# Our main goal is to use centrality measures, community detection algorithms, Random Walk to understand which one of them gives us a better insight.
# However, it also means that we will not limit ourselves with only 1 approach, but rather would use combination of approaches such that they would complement each other
# In this portion we will be using Centrality measures, including Degree Centrality, Closeness, and Eigen centrality
# Separate section is dedicated to eigen centrality and Katz centrality since in our opinion, they give key insights about user preferences
# in AO3.
# First centrality measure is Degree Centrality. For this it is important to note that our graph is bipartite
# Consisting of Stories, and Tags. Therefore, we decided to explore the distribution of degrees for both Stories and Tags
# Upon closer inspection, we found that both Stories and moreso Tags follow a power law distribution when it comes to degrees. Majority of Tags have 1
# or 0 degrees, and very few have >60 000 degrees in a dataset with roughly 1 million Tags, and 800 000 stories. Therefore, we decided to clean our datset further
# by considering Tags that have less than 10 000 degrees, and more than or equal to 3 degrees. It is important to note a cascading effect of this cleaning.
# By removing low degree tags that may or may not be isolated from the rest of the graph, we also may have removed tags that previously had 4 degrees.
# However, we justified this decision by gaining significant boost in computational effiiency, and since our interest lies mainly in studying larger groups of 
# stories, we can sacrifice few isolated tags. 
# We didn't include betweenness metric in our final product since computing a true betweenness for such a large network was not possible on a personal
# laptop with 16 GB of RAM. An alternative solution was to limit betweenness among 5 nodes, which may bring inherent inaccuracies to our results.

library(data.table)
library(igraph)



# 1. Load your CSV files
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
cutoff_value_low <- 10

# We look for nodes that are TAGS AND have degree > cutoff
godzilla_nodes <- V(g)[V(g)$type == TRUE & (deg > cutoff_value_high | deg <= cutoff_value_low)]

# 5. Now g_clean is a cleaned version of our graph, after removing highly popular Tags
g_clean <- delete_vertices(g, godzilla_nodes)



#---------------------------------------------------------------------------------------



# their total degree
deg <- degree(g_clean)
# specific degrees
story_deg <- deg[V(g_clean)$type == FALSE]
tag_deg   <- deg[V(g_clean)$type == TRUE]


# As it can be seen, histogram of degree distribution of Tags follows a power law. 
# Therefore, it is in our best interests to remove Tags that appear in more than 10 000 stories, or only appear
# in 0, 1, 2 stories, to study larger components of AO3 more efficiently

eig <- eigen_centrality(g_clean, directed = FALSE)$vector

# seeing top stories so to make sense of our data
top_eig_stories <- sort(eig[V(g_clean)$type == FALSE], decreasing = TRUE)[1:5]
print(top_eig_stories)


# Betweenness seems to be computationally very heavy, lets try exploring closeness if it is possible to
# compute

# Closeness

# harmonic closeness is faster and safer for disconnected components, whereas regular closeness
# occupies to much RAM and leads to crashes
close <- harmonic_centrality(g_clean)
# Stories with highest closeness can be thought of as the most "accessible" stories
top_mainstream <- sort(close[V(g_clean)$type == FALSE], decreasing = TRUE)[1:5]
print(top_mainstream)



# Here we decided to see how different the regular degree distribution and Katz Centrality are
# The intuition behind it was that Degree distribution followed a power law, and literally,
# it looked at degrees of the node as a way of saying it is connected to N other nodes
# Whereas Katz centrality looked at neighbors, and neighbors of neighbors and so on to measure
# How important a particular node was. The result confirmed what we expected, where Katz centrality
# graph seemed to follow an inverse log distribution

# the following code is copied from Katz Centrality section
library(Matrix)
A <- as_adjacency_matrix(g_clean, sparse = TRUE)
eigen_val <- eigen_centrality(g_clean, directed = FALSE, scale = FALSE)$value
alpha_val <- 0.85 * (1 / eigen_val)
n_nodes <- vcount(g_clean)
katz_scores <- rep(1, n_nodes)
beta <- 1 

for (i in 1:20) {
  prev_scores <- katz_scores
  katz_scores <- alpha_val * (A %*% katz_scores) + beta
  katz_scores <- as.vector(katz_scores)
  change <- sum(abs(katz_scores - prev_scores))
  if (change < 1e-6) {
    print(paste("Converged at iteration", i))
    break
  }
}

names(katz_scores) <- V(g_clean)$name
story_katz <- katz_scores[V(g_clean)$type == FALSE]
top_katz <- sort(story_katz, decreasing = TRUE)[1:10]
print("Top Stories by Katz:")
print(names(top_katz))

par(mfrow=c(1,2)) 
hist(degree(g_clean), main="Degree (Local)", col="blue", breaks=50)
hist(log(story_katz), main="Log Katz (Global)", col="red", breaks=50) 
# Note: I added log() to Katz because the values can get large





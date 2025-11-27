
library(data.table)
library(igraph)


# General overview of a recommendation algorithm based on a modified type of Random Walker or PageRank algorithm
# The task is to group Stories, and Tags by similarity into their communities, and Louvaine community detection algorithm is used
# Specific details are noted above each function, this header serves as an overview of our approach
# We need a recommender system for a user who reads stories on AO3 (Archive of Our Own), thus it is reasonable to assume
# that we would like to recommend alike stories to the story that user is reading currently
# However, there are several inherent limitations posed by our data
# We do not have user-specific data, meaning we only have contents (stories) that users read, and some metadata (Tags) that 
# each story is associated with.
# Therefore, we use content based grouping to infer some insight


# 1. Load your CSV files
tags_df <- fread("sample_tags.csv")
links_df <- fread("sample_taglinks.csv")
stories_df <- fread("sample_stories.csv")


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

# Here as suggested by Nat, we want to remove some Tags that are too popular.
# During one mode projection onto Stories, we ran into memory overuse issue, since some Tags were associated with nearly every 
# other story, therefore, R created a clique and made our one-mode network very heavy. Therefore,we delete Tags that are at 99%
# by popularity
# Also, another reason to remove popular tags is intuitive. If a Tag appears in nearly every story, it doesn't help us 
# differentiate stories, however, a Tag that appears in few stories can tell us unique information about these stories.
# to accomplish that, we calculate Degree for ALL nodes
deg <- degree(g)

# Now we only care about Tag Degrees
# (It's okay if a Story has many tags, but a Tag shouldn't have too many stories).
tag_nodes_indices <- which(V(g)$type == TRUE)
tag_degrees <- deg[tag_nodes_indices]

# Tags appearing in the top 1% of frequency are removed.
cutoff_value <- quantile(tag_degrees, 0.99)

# We look for nodes that are TAGS AND have degree > cutoff
godzilla_nodes <- V(g)[V(g)$type == TRUE & deg > cutoff_value]

# 5. Now g_clean is a cleaned version of our graph, after removing highly popular Tags
g_clean <- delete_vertices(g, godzilla_nodes)

# Here we switch from Tags onto Stories and want to calculate how popular stories are to improve our recommendations
# Ideally, we also want to recommend stories that are kudoed/commented by other users, partially because we believe in the wisdom of the crowd
# Another reason is we want to make our recommendation algorithm deterministic to a certain degree. Since we don't have any user data,
# we would want to recommend similar stories to users who decided to start with similar stories. Therefore, weighting by popularity
# of Stories is a straightforward, and easy to implement way. 
# We add +1 inside log to avoid log(0), and +1 at the end to ensure minimum weight is 1.
stories_df[, popularity_score := log(kudos + comments + 1) + 1]

# 2. Here we create a vector where we can lookup popularity by inputting its id
# Key = "S_123", Value = 5.4 (score)
pop_lookup <- setNames(stories_df$popularity_score, paste0("S_", stories_df$id))

# Initialize all nodes with weight 1 (Default for Tags), here we assumed that all Tags are equal (equally important)
# since value of each Tag is hard to represent numerically
V(g_clean)$weight <- 1

# Overwrite weights for Story nodes, the exact implementation of weights is done later in this code
# We find which graph nodes are stories
story_node_indices <- which(V(g_clean)$type == FALSE) 
story_names <- V(g_clean)$name[story_node_indices]

# Match scores to the graph nodes
# If a story has no score (NA), keep it as 1 to avoid having log(score) = 0 problem
matched_scores <- pop_lookup[story_names]
matched_scores[is.na(matched_scores)] <- 1

V(g_clean)$weight[story_node_indices] <- matched_scores

# This is the part where we detect communities in our bipartite graph using Louvaine algorithm
comm <- cluster_louvain(g_clean)

# We assign each node in a graph to its community
# Now every node has a "membership" ID (1, 2, 3, and etc)
V(g_clean)$community <- membership(comm)

# this function is the most simple implementation of Random Walker that uses built in page_rank function
# We call it iteration 0, however, it can be used such that we can compare performances of 2 functions against each other
# Key assumption here is that user specifies the node (Story) that they start with, and it can output number of recommendations
# controlled by top_n parameter.
# Arguments: graph = our clean graph, g_clean, raw_story_id = user specified story in char format, and top_n is int
get_recommendations <- function(graph, raw_story_id, top_n = 5) {
  
  # We need to add the prefix to match the graph nodes
  target_node <- paste0("S_", raw_story_id)
  # just a check
  if (!target_node %in% V(graph)$name) {
    stop("Story ID not found in the graph (it might have been isolated/cleaned).")
  }
  
  # Here the random walker works as follows. We start at a Story node, and we choose from Tag nodes that have an edge with
  # the Story node we are at. damping parameter specifies that a walker will keep walking in the network with probability p
  # Whereas 1-damping would mean our walker will return to a pre-defined Node (Story), which simulates real-world user 
  # who might get bored and goes to homepage.
  # This tells the walker: "Start here, and if you restart, come back here."
  p_vec <- setNames(rep(0, vcount(graph)), V(graph)$name)
  p_vec[target_node] <- 1
  
  # 4. Run Random Walk (PageRank)
  pr_result <- page_rank(
    graph, 
    directed = FALSE, 
    damping = 0.85, 
    personalized = p_vec
  )
  
  # important counding conditions are as following to filter out Nodes that we traversed by Random Walk
  # - Must NOT be the starting story
  candidates <- pr_result$vector
  story_mask <- V(graph)$type == FALSE & V(graph)$name != target_node
  final_scores <- candidates[story_mask]
  # 6. Return Top_N IDs, we can later implement to return names
  top_nodes <- head(sort(final_scores, decreasing = TRUE), top_n)
  # here we simply remove the S_ prefix to make the output clean
  clean_ids <- gsub("^S_", "", names(top_nodes))
  
  return(clean_ids)
}


# This is more advanced version of our previous random walk function. It consists of 2 functions, a Walker, and a Manager.
# The function now takes into account local Communities, Popularity of each Story, as well as, their structural similarity.
# Initially we used Louvaine community detection algorithm, and the logic behind using community parameter in Random Walker are:
# Suppose a walker hops from Story A to Story B and from Story B to Story C. Story A and B happen to be in a same community, 
# Whereas Story C is in a different community. Now in case a user would want to return back to starting node (which is dynamic),
# We would prioritize a Story which is in a last visited Community. In this case, since our walker is at Story C, and Story B
# is in the last visited community, our walker would return back to Story B. In real life, this situation can happen when a user
# wants to try out new genre but ends up reverting back to their old, more familiar genre.
simulate_sticky_walk <- function(graph, start_node_name, steps=1000, restart_prob=0.15, drift_resistance=0.7) {
  
  current_node_index <- match(start_node_name, V(graph)$name)
  history <- c()
  
  for (i in 1:steps) {
    history <- c(history, V(graph)$name[current_node_index])
    
    # RANDOM RESTART - hasn't changed since a simple random_walker algorithm
    if (runif(1) < restart_prob) {
      curr_comm <- V(graph)$community[current_node_index]
      community_peers <- which(V(graph)$community == curr_comm)
      
      if(length(community_peers) > 0) {
        # WEIGHTED RESTART: Even when restarting, prefer popular stories in the community
        peer_weights <- V(graph)$weight[community_peers]
        
        if (length(community_peers) == 1) {
          current_node_index <- community_peers
        } else {
          # This is the crux of sampling probability function
          # here a walker decides to step on an edge based on weights that incorporate all factors discussed above
          current_node_index <- sample(community_peers, 1, prob = peer_weights)
        }
      }
      next
    }
    
    # We retrieve neighbors
    neighbors_vs <- neighbors(graph, current_node_index)
    if (length(neighbors_vs) == 0) break 
    neighbor_indices <- as.numeric(neighbors_vs)
    
    # we retrieve the weights of the potential next steps
    candidate_weights <- V(graph)$weight[neighbor_indices]
    
    if (length(neighbor_indices) == 1) {
      candidate_next <- neighbor_indices
    } else {
      # The walker is more likely to choose a popular node, measured by a log(comments+kudos+1)
      candidate_next <- sample(neighbor_indices, 1, prob = candidate_weights)
    }
    
    # 4. Drift Resistance - means basically a resistance against switching from Story B to Story C, if they are in different 
    # communities
    curr_comm <- V(graph)$community[current_node_index]
    next_comm <- V(graph)$community[candidate_next]
    
    # simply checking for NA values
    if (!is.na(curr_comm) && !is.na(next_comm) && curr_comm != next_comm) {
      if (runif(1) < drift_resistance) {
        next 
      }
    }
    
    current_node_index <- candidate_next
  }
  return(history)
}


# this is the manager function that calls our sticky walk function.
# We called it sticky walk because of the resistance against switching communities, or genres of stories.
get_dynamic_recommendations <- function(graph, raw_story_id, n_walkers=100, walk_depth=6) {
  
  # We convert "154497" -> "S_154497" here
  start_node_prefixed <- paste0("S_", raw_story_id)
  

  if (!start_node_prefixed %in% V(graph)$name) {
    stop(paste("Error:", start_node_prefixed, "is not in the graph."))
  }
  
  # calls the Worker function N times
  all_visits <- replicate(n_walkers, {
    simulate_sticky_walk(graph, start_node_prefixed, steps=walk_depth)
  })
  
  # here we add all the visited nodes by the worker
  all_visited_nodes <- unlist(all_visits)
  visit_counts <- table(all_visited_nodes)
  # sorted nodes
  top_nodes <- sort(visit_counts, decreasing = TRUE)
  is_story <- grepl("^S_", names(top_nodes))
  is_not_start <- names(top_nodes) != start_node_prefixed
  final_recs <- top_nodes[is_story & is_not_start]
  clean_ids <- gsub("^S_", "", names(final_recs))
  return(head(clean_ids, 5))
}


# --- TEST BLOCK ---

# we pick a random test id
test_id <- stories_df$id[53] 

recs <- get_dynamic_recommendations(g_clean, test_id)
start_info <- stories_df[id == test_id, .(title, kudos)]
rec_info   <- stories_df[id %in% recs, .(id, title, kudos, comments)]
rec_info[, pop_score := log(kudos + comments + 1) + 1]
setorder(rec_info, -pop_score)
print(paste("START STORY:", start_info$title, "(Kudos:", start_info$kudos, ")"))
print("RECOMMENDATIONS:")
print(rec_info[, .(title, kudos, comments)]) # print specific columns only


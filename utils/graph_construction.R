library(igraph)

#' Generate a random bipartite graph with node types labeled as "story" or "tag".
#' The TRUE type corresponds to tags, whereas FALSE corresponds to stories
#'
#' This is a G(n, m) random bipartite graph, meaning there will be a constant
#' number of edges (tags associated with stories). This constant is equal to
#' story.count*m. As such, each story will have approximately m edges
#'
#' A good choice of m is 14, as this is the average number of tags associated
#' with stories in subsets we've encountered while working on the project
#'
#' While this graph is likely not realistic, it shares some structural
#' properties with the real network and may be easier to work with during
#' preliminary analysis
#'
#' @param   story.count Number of stories
#' @param   tag.count   Number of tags
#' @param   m           Approximate number of tags each story will have
sample_story_tag_graph <- function(story.count, tag.count, m=14) {
    # By using a G(n,m) graph with the edge count the product of story.count
    # and m, we get approximately m tags selected by each story
    graph <- sample_bipartite(
        n1=story.count,
        n2=tag.count,
        type='gnm',
        m=(story.count*m),
        directed=TRUE,
        mode='out'
    )
    
    # Assign the names and types
    V(graph)[1:story.count]$name <- 1:story.count
    V(graph)[1:story.count]$type <- FALSE

    V(graph)[story.count + 1:tag.count]$name <- 1:tag.count
    V(graph)[story.count + 1:tag.count]$type <- TRUE

    graph
}

#' Construct a bipartite story-tag graph
#'
#' This function takes the path of a CSV file of edges. The CSV file must have
#' the following format:
#'
#'     storyId|tagId
#'     <story ID 1>|<tag ID 1>
#'     <story ID 1>|<tag ID 2>
#'     ...
#'
#' That's to say, story ID numbers in the first column and tags associated
#' with it in the right. You'll get something like this from the `sqlite3`
#' command line utility if you run something like:
#'
#'     .output subset.csv
#'     select storyId, tagId from TagLinks where [...];
#'     .output stdout
#'
#' @param   csv.path    Path of the csv edge file
#' @param   portion     Number of rows from the CSV file to sample. This creates
#'                      a random subset of the given dataframe
#' @param   edges       If given, then use the provided edge dataframe instead
#'                      of loading it from csv.path
bipartite_story_tag_graph <- function(csv.path=NULL, portion=NULL, edges=NULL) {
    if (is.null(csv.path) && is.null(edges)) {
        stop('At least one of csv.path or edges must be provided')
    }

    if (is.null(edges)) {
        edges <- read.csv(csv.path, header=T, sep='|')
    }

    if (!is.null(portion)) {
        edges <- edges[sample(nrow(edges), round(nrow(edges) * portion)),]
    }

    stories <- unique(edges$storyId)
    tags <- unique(edges$tagId)
    
    # Create a really basic graph and then remove all edges and vertices from it.
    # We should be able to use empty_graph() for this but for some reason it seems
    # to be handled poorly in igraph, so this is easier
    story.tag.graph <- make_graph(c(1,2))
    story.tag.graph <- delete_edges(story.tag.graph, E(story.tag.graph))
    story.tag.graph <- delete_vertices(story.tag.graph, V(story.tag.graph))
    
    # Construct the bipartite graph from parts. Doing it this way helps us to
    # avoid the problem of storyIds and tagIds having some overlap
    story.tag.graph <- add_vertices(story.tag.graph, nv=length(stories), name=stories, type=FALSE)
    story.tag.graph <- add_vertices(story.tag.graph, nv=length(tags), name=tags, type=TRUE)
    
    # These will be used for adding the edges to the graph, since igraph has a
    # hard time knowing the difference between numeric names and ids
    edges$storyIndex <- match(edges$storyId, V(story.tag.graph)[which(type==FALSE)]$name)
    
    # Note that the tag vertices were added second, so by index, they're offset by
    # length(stories).
    edges$tagIndex <- match(edges$tagId, V(story.tag.graph)[which(type==TRUE)]$name) + length(stories)
    
    story.tag.graph <- add_edges(story.tag.graph,
        edges=as.matrix(edges[,3:4]) |> t() |> as.vector()
    )

    story.tag.graph
}

#' Given a story-tag bipartite graph (as generated with
#' bipartite_story_tag_graph or sample_stor_tag_graph), take a subset
#' corresponding to stories reachable by a random walk from a particular
#' starting story node
#'
#' This is a "spatial" subset because it has the property of preserving
#' something close to the local topology about the starting node. This,
#' as opposed to a random subset, which may destroy the original graph's
#' topology.
#'
#' ## Why not BFS?
#' BFS would yield a sequence of nodes corresponding to a story, all the
#' tags associated with that story, another story...
#'
#' Also, the igraph function is extremely poorly equipped to deal with
#' "limited" searches. It needs to traverse the whole network
#'
#' This function is designed to leverage the property that the selected
#' node sequence has an alternating pattern of story, tag, story, etc.
#'
#' ## Why not DFS?
#' You'd think that DFS would yield an alternating sequence of node types,
#' which I did, but it turns out the igraph implementation of DFS considers
#' nodes visited in an order identical to the one yielded by BFS.
#'
#' This probably has to do with when a vertex is considered "visited," which
#' can vary depending on the implementation of DFS
#'
#' ## Why random walk?
#' Despite its limitations (e.g. it may visit nodes more than once, making it
#' hard to estimate exactly how many stories will be included in the subset),
#' this is the best algorithm I'm aware of that'll reliably yeild this
#' alternating story/tag exploration pattern we're looking for
#'
#' ## Example
#'
#'     edges <- read.csv(csv.path, header=T, sep='|')
#'     graph <- bipartite_story_tag_graph(edges=edges)
#'     graph.subset <- story_tag_spatial_subset(graph, edges, 1, 1000)
#'
#' @param   graph       igraph bipartite story-tag graph
#' @param   edges       Dataframe (as described for bipartite_story_tag_graph)
#'                      featuring the graph's edges
#' @param   story.index Index of the starting vertex in V(graph). This must
#'                      be a story (type == FALSE) or the function will throw
#'                      an error
#' @param   steps       Number of steps the random walker will take.
story_tag_spatial_subset  <- function(graph, edges, story.index, steps) {
    # Recall: false corresponds to stories. This function only works when the
    # starting point is a story.
    if (V(graph)[story.index] == FALSE) {
        stop('The given story index corresponds to a node which is not a story')
    }

    # Find all nodes visited by a breadth-first search from start.node.index.
    # This includes both stories and tags
    node.sequence <- random_walk(graph, start=story.index, steps=steps, mode='all')

    # Take every other node in the sequence. The odd-indexed nodes in this
    # sequence are all stories
    node.sequence <- node.sequence[seq(1, steps, by=2)]

    # We only want the unique stories. Some may have been visited multiple times
    node.sequence <- unique(node.sequence)

    # Awkward value shuffling/conversion from node sequence to integer vector of
    # story IDs
    node.sequence <- as.integer(attributes(node.sequence)$names)

    # Take a subset of the edges, including only those featuring stories in the
    # node.sequence
    edge.subset <- edges[which(edges$storyId %in% node.sequence),]

    bipartite_story_tag_graph(edges=edge.subset)
}

library(igraph)

source('../../utils/random_walk_utility.R')
source('../../utils/graph_construction.R')

STARTING_STORY_ID <- 11363757
# Index: 1

# Story-Tag links
edges <- read.csv('../../subsets/2020-04--2020-07-tag-links.csv', header=T, sep='|')

# Kudos, comments, hits and bookmarks
story.metrics <- read.csv('../../subsets/2020-04--2020-07-story-metrics.csv', header=T, sep='|')

story.tag.graph <- bipartite_story_tag_graph(edges=edges)

large.bipartite.subset <- story_tag_spatial_subset(story.tag.graph, edges, story.index=1, steps=10000)
large.story.graph <- bipartite_projection(large.bipartite.subset, which=FALSE)

small.bipartite.subset <- story_tag_spatial_subset(large.bipartite.subset, edges, story.index=1, steps=1000)
small.story.graph <- bipartite_projection(small.bipartite.subset, which=FALSE)

STARTING_STORY_INDEX_LARGE <- which(V(large.story.graph)$name == STARTING_STORY_ID)
STARTING_STORY_INDEX_SMALL <- which(V(small.story.graph)$name == STARTING_STORY_ID)

kudos <- story.metrics$kudos |> sort() |> rev()

large.story.graph.metrics <- subset(story.metrics, story.metrics$storyId %in% V(large.story.graph)$name)
V(large.story.graph)$kudos <- large.story.graph.metrics$kudos

small.story.graph.metrics <- subset(story.metrics, story.metrics$storyId %in% V(small.story.graph)$name)
V(small.story.graph)$kudos <- small.story.graph.metrics$kudos

utility_means <- function(utility.matrix) {
    utility.matrix[which(utility.matrix == 0)] <- NA
    stats <- colMeans(utility.matrix, na.rm=T)
    stats[which(is.nan(stats))] <- NA
    stats
}

normalize_utility <- function(scores) {
    (scores - min(scores)) /
        (max(scores) - min(scores))
}

# For the purposes of this analysis we're just going to assume that the
# random walkers hit every node. This is fine because the utility score
# is related only to the kudos count. Using only the utility function
# is ridiculously more performant
#
# utility.synthetic <- random_walker_utility(
#     large.story.graph,
#     walker.count=10,
#     step.count=3, # Max distance between any two nodes
#     start.node=STARTING_STORY_INDEX_LARGE,
#     step.algorithm=step_algorithm_with_common_tags,
#     utility.transformer=utility_transformer_with_kudos,
#     utility.transformer.options=c(minimum=0.1)
# )

V(large.story.graph)$synthetic.utility <- utility_transformer_with_kudos(
    large.story.graph,
    Matrix(1, ncol=vcount(large.story.graph)),
    options=c(minimum=0.1, middle.spread=0.6)
) |> as.vector()

V(small.story.graph)$synthetic.utility <- utility_transformer_with_kudos(
    small.story.graph,
    Matrix(1, ncol=vcount(small.story.graph)),
    options=c(minimum=0.1, middle.spread=0.6)
) |> as.vector()

# Centrality
## Degree, Closeness, eigenvector, betweenness

V(large.story.graph)$degree.utility <- degree(large.story.graph) |> normalize_utility()
V(small.story.graph)$degree.utility <- degree(small.story.graph) |> normalize_utility()

V(large.story.graph)$closeness.utility <- degree(large.story.graph) |> normalize_utility()
V(small.story.graph)$closeness.utility <- degree(small.story.graph) |> normalize_utility()

V(large.story.graph)$betweenness.utility <- degree(large.story.graph) |> normalize_utility()
V(small.story.graph)$betweenness.utility <- degree(small.story.graph) |> normalize_utility()

V(large.story.graph)$eigen.utility <- eigen_centrality(large.story.graph)$vector |> normalize_utility()
V(small.story.graph)$eigen.utility <- eigen_centrality(small.story.graph)$vector |> normalize_utility()

# Similarity
## Cosine, jaccard

neighbourhood_subgraph  <- function(graph, node.name) {
    edgelist <- as_edgelist(small.story.graph)
    neighbours <- edgelist[which(edgelist[,1] == node.name),][,2]
    neighbours <- V(graph)[which(V(graph)$name %in% neighbours)]
    neighbourhood <- induced_subgraph(graph,
                            c(V(graph)[which(V(graph)$name == node.name)], neighbours))

    neighbourhood
}

cosine_similarity <- function(graph, node.name) {
    neighbourhood <- neighbourhood_subgraph(graph, node.name)
    adjacency.matrix <- as_adjacency_matrix(neighbourhood)

    source.node.col <- adjacency.matrix[,1]
    other.node.cols <- adjacency.matrix |> t() |> tail(-1) |> t()

    cosine.similarities <- apply(other.node.cols, 2, function(column)
            sum(source.node.col * column) /
                (sqrt(sum(source.node.col**2)) * sqrt(sum(column**2))))
    V(graph)$c__cosine.similarity <- NA
    V(graph)[which(V(graph)$name == node.name)]$c__cosine.similarity <- 1
    V(graph)[which(V(graph)$name %in% tail(V(neighbourhood)$name, -1))]$c__cosine.similarity <- cosine.similarities

    V(graph)$c__cosine.similarity
}

jaccard_similarity <- function(graph, node.name) {
    neighbourhood <- neighbourhood_subgraph(graph, node.name)
    node.index <- which(V(graph)$name == node.name)

    V(graph)$c__jaccard.similarity <- NA
    V(graph)[which(V(graph)$name %in% V(neighbourhood)$name)]$c__jaccard.similarity <- 
        similarity(neighbourhood, method='jaccard')[node.index,]

    V(graph)$c__jaccard.similarity
}

# These are already put on a scale from 0 to 1, so we don't need to bother normalizing them
V(large.story.graph)$cosine.utility <- cosine_similarity(large.story.graph, STARTING_STORY_ID)
V(small.story.graph)$cosine.utility <- cosine_similarity(small.story.graph, STARTING_STORY_ID)

V(large.story.graph)$jaccard.utility <- jaccard_similarity(large.story.graph, STARTING_STORY_ID)
V(small.story.graph)$jaccard.utility <- jaccard_similarity(small.story.graph, STARTING_STORY_ID)

large.utility.df <- data.frame(
    degree=V(large.story.graph)$degree.utility,
    closeness=V(large.story.graph)$closeness.utility,
    betweenness=V(large.story.graph)$betweenness.utility,
    eigen=V(large.story.graph)$eigen.utility,

    cosine=V(large.story.graph)$cosine.utility,
    jaccard=V(large.story.graph)$jaccard.utility
)

utility.r.squared <- apply(large.utility.df, 2, function(z)
    cor(V(large.story.graph)$synthetic.utility, z, use = "complete.obs") ^ 2
)

utility.r.squared
#>      degree   closeness betweenness       eigen      cosine     jaccard 
#> 0.036957275 0.036957275 0.036957275 0.065966774 0.007424192 0.008312686

# These are all pretty bad, although this shouldn't be surprising. The metrics
# tell us fundamentally different things about the network. Although our
# synthetic preference data is based entirely on how popular stories are
# overall, these metrics are primarily concerned with how stories relate to
# one another.

# Clearly, a story's centrality or local similarity is not proportional to how
# popular it is. The synthetic user data is most applicable in the context of
# making recommendations within a neighbourhood of a particular story,
# incorporating shared tags, as the random_walker_utility function is implemented
# to account for.

# Given that eigenvector centrality incorporates the relative importance of nodes
# that link to it, it's not surprising that eigenvector centrality is the "best
# fit," albeit still not a good fit

svg('fig01-r-squared.svg')
barplot(utility.r.squared,
    main='R² values for metrics correlated with kudos utility',
    names=c('Degree', 'Closeness', 'Betweenness', 'Eigenvector', 'Cosine', 'Jaccard'),
    xlab='Metric', ylab='R²',
    cex.names=0.8
)
dev.off()

node_palette <- hcl.colors(256)
colourizer <- function(v) {
    node_palette[round(v * 255) + 1]
}

plot_utility <- function(small.graph, large.graph, variable, name='Utility Score Distribution', normalize=FALSE, seed=1, vertex.size=7) {
    layout_matrix <- matrix(1:2, nrow=2)
    small.utilities <- vertex_attr(small.graph, variable)

    coloured.utility <- vertex_attr(small.graph, variable)
    if (normalize) {
        coloured.utility <- (coloured.utility - min(coloured.utility, na.rm=T)) /
            (max(coloured.utility, na.rm=T) - min(coloured.utility, na.rm=T))
    }

    V(small.graph)$color <- colourizer(coloured.utility)
    V(small.graph)[1]$color <- 'red' # Start node
    V(small.graph)[is.na(vertex_attr(small.graph, variable))]$color <- 'grey'
    E(small.graph)$color <- '#AAAAAA88'
    layout(layout_matrix, height=2:1)

    set.seed(seed)

    par(mar=c(0,0,5,0))
    plot(small.graph, vertex.label=NA, main=name, vertex.size=vertex.size)

    par(mar=c(5,3,0,2))
    hist(
        vertex_attr(large.graph, variable),
        main=NA,
        xlab=NA,
        ylab=NA,
        breaks=20,
    )

    set.seed(NULL)
    layout(1)
}

svg('fig02-utility-degree.svg')
plot_utility(small.story.graph, large.story.graph, name='Degree utility', 'degree.utility')
dev.off()

svg('fig03-utility-closeness.svg')
plot_utility(small.story.graph, large.story.graph, name='Closeness utility', 'closeness.utility')
dev.off()

svg('fig04-utility-betweenness.svg')
plot_utility(small.story.graph, large.story.graph, name='Betweenness utility', 'betweenness.utility')
dev.off()

svg('fig05-utility-eigenvector.svg')
plot_utility(small.story.graph, large.story.graph, name='Eigenvector centrality utility', 'eigen.utility')
dev.off()

svg('fig06-utility-cosine.svg')
plot_utility(small.story.graph, large.story.graph, name='Cosine similarity utility', 'cosine.utility')
dev.off()

svg('fig07-utility-jaccard.svg')
plot_utility(small.story.graph, large.story.graph, name='Jaccard similarity utility', 'jaccard.utility')
dev.off()

svg('fig08-utility-synthetic.svg')
plot_utility(small.story.graph, large.story.graph, name='Synthetic kudos-based utility', 'synthetic.utility')
dev.off()

make_residual <- function(graph, variable) {
    (vertex_attr(graph, 'synthetic.utility') - vertex_attr(graph, variable)) ^ 2
}

V(large.story.graph)$degree.utility.residual <- make_residual(large.story.graph, 'degree.utility')
V(large.story.graph)$closeness.utility.residual <- make_residual(large.story.graph, 'closeness.utility')
V(large.story.graph)$betweenness.utility.residual <- make_residual(large.story.graph, 'betweenness.utility')
V(large.story.graph)$eigen.utility.residual <- make_residual(large.story.graph, 'eigen.utility')

V(large.story.graph)$cosine.utility.residual <- make_residual(large.story.graph, 'cosine.utility')
V(large.story.graph)$jaccard.utility.residual <- make_residual(large.story.graph, 'jaccard.utility')

V(small.story.graph)$degree.utility.residual <- make_residual(small.story.graph, 'degree.utility')
V(small.story.graph)$closeness.utility.residual <- make_residual(small.story.graph, 'closeness.utility')
V(small.story.graph)$betweenness.utility.residual <- make_residual(small.story.graph, 'betweenness.utility')
V(small.story.graph)$eigen.utility.residual <- make_residual(small.story.graph, 'eigen.utility')

V(small.story.graph)$cosine.utility.residual <- make_residual(small.story.graph, 'cosine.utility')
V(small.story.graph)$jaccard.utility.residual <- make_residual(small.story.graph, 'jaccard.utility')

svg('fig09-utility-degree-residual.svg')
plot_utility(small.story.graph, large.story.graph, name='Residual Degree utility', 'degree.utility.residual', normalize=T)
dev.off()

svg('fig10-utility-closeness-residual.svg')
plot_utility(small.story.graph, large.story.graph, name='Residual Closeness utility', 'closeness.utility.residual', normalize=T)
dev.off()

svg('fig11-utility-betweenness-residual.svg')
plot_utility(small.story.graph, large.story.graph, name='Residual Betweenness utility', 'betweenness.utility.residual', normalize=T)
dev.off()

svg('fig12-utility-eigenvector-residual.svg')
plot_utility(small.story.graph, large.story.graph, name='Residual Eigenvector utility', 'eigen.utility.residual', normalize=T)
dev.off()

svg('fig13-utility-cosine-residual.svg')
plot_utility(small.story.graph, large.story.graph, name='Residual Cosine utility', 'cosine.utility.residual', normalize=T)
dev.off()

svg('fig14-utility-jaccard-residual.svg')
plot_utility(small.story.graph, large.story.graph, name='Residual Jaccard utility', 'jaccard.utility.residual', normalize=T)
dev.off()

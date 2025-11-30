library(igraph)

source('../../utils/random_walk_utility.R')
source('../../utils/graph_construction.R')

# Story-Tag links
edges <- read.csv('../../subsets/2020-04--2020-07-tag-links.csv', header=T, sep='|')

# Kudos, comments, hits and bookmarks
story.metrics <- read.csv('../../subsets/2020-04--2020-07-story-metrics.csv', header=T, sep='|')

story.tag.graph <- bipartite_story_tag_graph(edges=edges)

# The small story graph will be used for some demonstrative purposes and for
# figures to be used in the final report
small.bipartite.subset <- story_tag_spatial_subset(story.tag.graph, edges, story.index=1, steps=100)
small.story.graph <- bipartite_projection(small.bipartite.subset, which=FALSE)

# Goal: develop and motivate a mathematical model of A03 user behaviour that
# can be used to generate synthetic user preference data. This may be used as
# a baseline to compare methods of generating story recommendations

# The way discovery typically works on a social media website structured around
# some media object and tags is that you view some content, check its tags, and
# then explore content related by the most interesting tags. Probably the best
# tool we have to model this behaviour is a random walker algorithm.

# Pseudocode example algorithm:
#
# random_walker_utility <- function(graph, start.node, steps=1000) {
#     # Define the utility.matrix as a sparse matrix
#     utility.matrix <- sparse_matrix(ncol = story.count,
#                                     nrow = simulated.user.count)
# 
#     # Perform a random walk for each simulated user
#     for 0 to simulated.user.count {
#         # The step algorithm takes some parameters and returns a path through
#         # the network corresponding to stories visited during a simulated
#         # user's exploration. How this function is defined affects the
#         # properties of the scoring. This is key detail #1
#         path <- step_algorithm(graph, start.node, steps)
#         utility.matrix[start.node] <- tabulate(path)
#     }
# 
#     # The utility transformer is a function that takes the utility matrix and
#     # applies some transformation to it such that its values represent a
#     # meaningful preference score. How this is implemented is key detail #2
#     utility.matrix <- utility_transformer(utility.matrix)
# 
#     utility.matrix
# }

# Indeed, this is more or less how the random_walker_utility function is defined
# in utils/random_walker_utility.R. The interesting bits that deserve motivation
# are the two key details described above

# Note: there is no way to assess this model's applicability to reality because
# this network has no individual user preference data. The model uses real-world
# context in order to approximate expected user behaviour, however, it is
# impossible to demonstrate this approach's accuracy as-is. This is an inherent
# limitation of this study, however, we've decided to go forward with this
# appraoch anyway because the limitations create some interesting engineering
# problems and opportunities to explore unusual questions.

# With that in mind, we will attempt to develop a synthetic user preference data
# model within these limitations based primarily on logic and our own intuition
# as users of A03

## Key detail #1 - Step algorithm

#' Color the edges of a story projection graph based on a walk
walk_coloured_graph <- function(graph, step.count, start.node, step.algorithm, iterations=3) {
    E(graph)$visits <- 0

    for (i in 1:iterations) {
        walk <- step.algorithm(graph, step.count, start.node)
        walk.edges <- as_edgelist(induced_subgraph(graph, walk))
        walk.edges <- apply(walk.edges, 1, function(z) paste(z, collapse = "|"))

        # Increment visit count
        E(graph)[walk.edges]$visits <- E(graph)[walk.edges]$visits + 1
    }

    # Convert the visit count to a value from 00-FF
    E(graph)$visits <- E(graph)$visits / max(E(graph)$visits)
    E(graph)$visits <- sprintf('%02x', round(E(graph)$visits * 255))

    # Edges will have both their color (black to bright red) and opaqueness
    # directly proportional to the number of visits they recieved during the
    # random walk iterations
    E(graph)$color <- paste('#', E(graph)$visits, '0000', E(graph)$visits, sep='')

    V(graph)$color <- 'orange'
    V(graph)[start.node]$color <- 'red'

    graph
}

# We consider two step algorithms

### Step algorithm with equal weights

# The step algorithm with equal weights implements a pure random walker. No
# next node has any particular advantage, it's merely chosen at random from the
# possible next nodes

step_algorithm_with_equal_weights <- function(graph, step.count, start.node) {
    random_walk(
        graph=graph,
        weights=NA,
        start=start.node,
        steps=step.count,
        mode='out'
    )
}

svg('fig01-step_algo_equal_weights.svg')
set.seed(1)
par(mar=c(5,0,5,0))
plot(
    walk_coloured_graph(small.story.graph, 3, 1, step_algorithm_with_equal_weights, iterations=100),
    edge.width=E(small.story.graph)$weight,
    vertex.label=NA,
    main='Step algorithm with equal weights')
set.seed(NULL)
dev.off()

### Step algorithm with common tags

# By leveraging multiplicity within the bipartite graph we can have our random
# walker prefer to take paths higher weights, or jump to stories that share more
# tags in common

# In reality, an A03 user is likely not compiling lists of tags shared by every
# story they want to read, however, it's likely that if two stories share many
# of the same tags, they'll be more interesting to someone who enjoyed the first
# story. This strategy leverages structural similarity between stories.

step_algorithm_with_common_tags <- function(graph, step.count, start.node) {
    random_walk(
        graph=graph,
        weights=NULL,       # This takes advantage of the graph's `weight`
                            # edge attribute
        start=start.node,
        steps=step.count,
        mode='out'
    )
}

svg('fig02-step_algo_common_tags.svg')
set.seed(1)
par(mar=c(5,0,5,0))
plot(
    walk_coloured_graph(small.story.graph, 3, 1, step_algorithm_with_common_tags, iterations=100),
    edge.width=E(small.story.graph)$weight,
    vertex.label=NA,
    main='Step algorithm with common tags')
set.seed(NULL)
dev.off()

# We judge that this later step algorithm is more realistic, and so it will be
# used as a part of our synthetic user preference data generator

## Key detail #2 - Utility transformation
source('../../utils/random_walk_utility.R')

utility.means <- function(utility.matrix) {
    utility.matrix[which(utility.matrix == 0)] <- NA
    stats <- colMeans(utility.matrix, na.rm=T)
    stats[which(is.nan(stats))] <- NA
    stats
}

### Utility for easily pleased users

# The simplest utility transformation function would be one that assumes if
# a modelled user reads a story, they liked it. This is also the least
# interesting utility transformation function, but we bring it up as a sort
# of baseline for comparison.

utility_transformer_for_an_easily_pleased_walker <- function(graph, utility.matrix, options=c()) {
    utility.matrix[which(utility.matrix > 0)] <- 1
    utility.matrix
}

easily.pleased.utility <- random_walker_utility(small.story.graph,
                      walker.count=5,
                      step.count=2,
                      start.node=1,
                      step.algorithm=step_algorithm_with_common_tags,
                      utility.transformer=utility_transformer_for_an_easily_pleased_walker)

V(small.story.graph)$easily.pleased.utility <- utility.means(easily.pleased.utility)

# e.g.
V(small.story.graph)$easily.pleased.utility
#>  [1]  1 NA NA  1 NA NA  1 NA  1 NA NA NA  1 NA NA NA NA NA NA  1 NA NA  1 NA  1
#> [26] NA NA NA NA  1  1 NA NA NA NA NA NA NA

# We consider unread stories as having no score, represented as NA. Most stories
# get visited because this is a very dense graph

edge_density(small.story.graph)
#> [1] 0.7525253

### Utility based on transitivity

# This is another basic strategy for modeling story utility. The more a story
# is visited by a single random walker, the higher its visit count is. Here,
# that visit count is divided by the maximum visit count to create a normalized
# score.

# Nodes with a higher utility score are well clustered around the starting node.
# Based on the logic of A03 tags, this means they are structurally similar on
# the bipartite graph

utility_transformer_with_transitivity <- function(graph, utility.matrix, options=c()) {
    utility.matrix / options['max']
}

transitivity.utility <- random_walker_utility(small.story.graph,
                      walker.count=100,
                      step.count=10,
                      start.node=1,
                      step.algorithm=step_algorithm_with_common_tags,
                      utility.transformer=utility_transformer_with_transitivity)

V(small.story.graph)$transitivity.utility <- utility.means(transitivity.utility)

# e.g.
V(small.story.graph)$transitive.utility
#> [1] 0.3833333 0.3529412 0.3611111 0.3690476 0.3589744 0.3333333 0.3777778
#> [8] 0.3636364 0.3428571 0.3703704 0.4047619 0.3866667 0.3333333 0.3589744
#>[15] 0.3846154 0.3333333 0.4000000 0.3833333 0.3333333 0.3492063 0.3472222
#>[22] 0.3968254 0.3750000 0.3333333 0.3838384 0.3796296 0.3529412 0.3750000
#>[29] 0.4102564 0.3333333 0.4057971 0.3866667 0.3777778 0.3555556 0.3333333
#>[36] 0.3333333 0.3456790 0.3529412 0.3484848 0.3448276 0.3859649 0.3563218
#>[43] 0.3947368 0.3333333 0.3866667

# Despite revealing more information about the network, we judge that this
# utility model isn't very useful on its own. It seems most nodes have about
# the same level of reciprocity

### Utility based on story metrics

kudos <- story.metrics$kudos |> sort() |> rev()
head(kudos)
#> [1] 20718  7691  7471  6194  5966  5607

# Significant outliers in this data set make deriving an exponential fit
# awkward. However, as is, we can use the `lm` function to get a fairly strong
# linear fit for the log of story kudos

# Since you cannot take the log of 0, we exclude stories with kudos of zero.
# These stories don't tell us much anyway
kudos_geq_1 <- kudos[which(kudos > 0)]

kudos_fit <- lm(log(kudos_geq_1) ~ seq(1, length(kudos_geq_1), by=1))
summary(kudos_fit)$r.squared
#> [1] 0.9668995

# Not bad all things considered. That being said, there's a case to be made
# that stories with very low kudos have little to tell us, and the only thing
# a story with incredibly high kudos tells us is that it has broad appeal.

# We could focus only on the middle 60% of stories, where the kudos distribution
# can be very closely approximated by an exponential function:

# Take the middle 60% of the stories.
story.count <- length(kudos)
kudos_60p <- kudos[round(story.count*.2):round(story.count*.8)]
range(kudos_60p)
#> [1]  5 78

# Normalize, log and fit the data
kudos_60p <- log(kudos_60p)
kudos_60p <- kudos_60p/max(kudos_60p)

kudos_60p_fit <- lm(kudos_60p ~ seq(1, length(kudos_60p), by=1))
summary(kudos_60p_fit)$r.squared
#> [1] 0.9939823

svg('fig03-kudos-rating-curve.svg')
plot(kudos_60p, type='l', main='Normalized kudos rating curve', xlab='Rank', ylab='Kudos score')
abline(kudos_60p_fit)
text(150000, 0.8, paste('R² =', summary(kudos_60p_fit)$r.squared))
dev.off()

# This is a very strong fit

# Using this information we can normalize kudos values dependent on the
# distribution of the kudos vector. The following function is subset-independent
# and could be applied to the whole network (as we have been) or communites for
# more refined, local scores

#' Given a vector of story kudos (from the whole network or any subset),
#' construct and return a function that takes a specific kudos count and
#' computes a normalized score corresponding to how well recieved it is within
#' the context of the subset.
#'
#' @param   kd              Vector of kudoses for stories
#' @param   minimum         Lowest possible score, between 0 and 1
#' @param   middle.spread   Portion of the data to include, after removing
#'                          stories with zero kudos, measured about the median
#'                          of the kd vector. For example, if
#'                          length(kd[which(kd > 0)]) == 100, and
#'                          middle.spread == 0.6, then the middle 60 stories
#'                          will be included.
kudos_transformer_factory <- function(kd, minimum=0, middle.spread=1) {
    # Ensure that kd excludes stories with zero kudos
    # take the log
    kd <- kd[which(kd > 0)]
    outer.spread.width = (1 - middle.spread) / 2
    kd <- kd[round(length(kd) * outer.spread.width) : round(length(kd) * (1 - outer.spread.width))]
    log_kd <- log(kd)
    kd_range <- range(kd)
    function(v) {
        # If we're below the lower bound, we set the score to 0.1
        v[which(v < kd_range[1])] <- minimum

        # If we're above the upper bound, set the score to 1
        v[which(v > kd_range[2])] <- 1

        # Put the value on a scale from 0 to 1
        middle.part <- v[which(v >= kd_range[1] & v <= kd_range[2])]
        middle.part <- (log(middle.part) - range(log_kd)[1]) /
                    (range(log_kd)[2] - range(log_kd)[1])

        # Adjust the normalized score
        middle.part <- (middle.part * (1-minimum)) + minimum

        v[which(v >= kd_range[1] & v <= kd_range[2])] <- middle.part
        v
    }
}

# For example, to generate normalized, kudos-based scores described earlier,
demonstrated_kudos_transformer <-
    kudos_transformer_factory(kudos, minimum=0, middle.spread=0.6)

# This function isn't linear, although it is approximately linear as seen
# earlier. We could use a linear fit to the data although we don't really need
# to. If we chose to do so it would introduce some minor error. Ultimately we
# chose to leverage the distribution itself out of ease of implementation.

# This approach should generalize to other metrics that have a power-law
# distribution in the network, such as views, comments, and centrality.

transformed.kudos <- demonstrated_kudos_transformer(kudos)

svg('fig04-utility-kudos.svg')
plot(transformed.kudos, type='l', main='Transformed Kudos Scores', xlab='Rank', ylab='Utility')
dev.off()

small.story.graph.metrics <- subset(story.metrics, story.metrics$storyId %in% V(small.story.graph)$name)
V(small.story.graph)$kudos <- small.story.graph.metrics$kudos

kudos.utility <- random_walker_utility(
    small.story.graph,
    walker.count=100,
    step.count=10,
    start.node=1,
    step.algorithm=step_algorithm_with_common_tags,
    utility.transformer=utility_transformer_with_kudos,
    utility.transformer.options=c(minimum=0.1)
)

V(small.story.graph)$kudos.utility <- utility.means(kudos.utility)
# e.g.
#>  [1] 1.0000000 0.5341794 0.5515900 0.8076243 0.2869909 0.1000000 0.3861923
#>  [8] 0.7441434 0.7357561 0.6494913 0.4061845 0.7678458 0.1000000 0.9647377
#> [15] 0.6162468 0.2191936 0.6825659 0.8224389 0.1000000 1.0000000 0.1000000
#> [22] 0.2552165 0.4247968 0.9392687 0.7385809 0.1992014 0.1000000 0.9184534
#> [29] 0.1000000 0.2191936 0.1000000 0.1541351 0.7549357 0.6646937 0.1000000
#> [36] 0.1000000 0.6893959 0.1284232 0.1000000 0.7270996 0.1000000 0.4885675
#> [43] 0.1776082 0.6893959 0.7495935

### Illustrating the utility functions
node_palette <- hcl.colors(256)
colourizer <- function(v) {
    node_palette[round(v * 255) + 1]
}

plot_utility <- function(graph, variable, name='Utility Score Distribution', normalize=FALSE, seed=1) {
    layout_matrix <- matrix(1:2, nrow=2)
    utilities <- vertex_attr(small.story.graph, variable)
    if (normalize) {
        utilities <- (utilities - min(utilities)) /
                        (max(utilities) - min(utilities))
    }

    V(graph)$color <- colourizer(utilities)
    V(graph)[1]$color <- 'red' # Start node
    V(graph)[is.na(utilities)]$color <- 'grey'
    layout(layout_matrix, height=2:1)

    set.seed(seed)

    par(mar=c(0,0,5,0))
    plot(graph, vertex.label=NA, main=name)

    par(mar=c(5,3,0,2))
    hist(
        vertex_attr(small.story.graph, variable),
        main=NA,
        xlab=NA,
        ylab=NA,
        breaks=20
    )

    set.seed(NULL)
    layout(1)
}

svg('fig05-utility-easily-pleased.svg')
plot_utility(small.story.graph, 'easily.pleased.utility', name='Easily Pleased Utility')
dev.off()

svg('fig06-utility-transitivity.svg')
plot_utility(small.story.graph, 'transitivity.utility', name='Transitivity Utility', normalize=TRUE)
dev.off()

svg('fig07-utility-kudos.svg')
plot_utility(small.story.graph, 'kudos.utility', name='Kudos Utility')
dev.off()

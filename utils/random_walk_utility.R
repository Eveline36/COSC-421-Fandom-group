library(igraph)
library(Matrix)

#' Basic step algorith. Edges are weighted using custom weights, or the
#' weight edge attribute if NULL
#'
#' The step algorithm is used to calculate the path the random walker takes
#' through the network. For composability and ease of creating variant random
#' walkers, this functionality has been extracted into a separate function which
#' can be passed to the `random_walker_utility` using the `step.algorithm`
#' argument
#'
#' @param   graph               igraph graph object; one-mode projection on the
#'                              stories
#' @param   step.count          Number of steps to take during the walk
#' @param   start.node          Constant index of the node to start on
#' @param   options['weights']  Weights to assign to each edge. If explicitly
#'                              given NULL, the graph edges' weight attributes
#'                              are used. If not provided (NA), then each edge
#'                              has equal edges
#'
#' @return  A random walk path
step_algorithm_basic <- function(graph, step.count, start.node, options=c()) {
    weights <- options['weights']
    print(weights)

    random_walk(
        graph=graph,
        weights=options['weights'],
        start=start.node,
        steps=step.count,
        mode='out'
    )
}

#' A step algorithm that weighs each edge based on the number of common tags it
#' has with the story it's connected to
#'
#' If used with a one-mode projection of the bipartite story graph on the
#' stories, calculated with the `bipartite_projection` function, with
#' `multiplicity=TRUE`, then each edge will have a weight corresponding to the
#' number of tags the stories have in common.
#'
#' See also: <https://r.igraph.org/reference/bipartite_projection.html>
#'
#' For parameter documentation, see `step_algorithm_basic`
#'
#' @return  A random walk path
step_algorithm_with_common_tags <- function(graph, step.count, start.node, options=c()) {
    step_algorithm_basic(graph, step.count, start.node, options=c(weights = NULL))
}

#' Step algorithm where every edge has an equal chance of being selected
#'
#' For parameter documentation, see `step_algorithm_basic`
#'
#' @return  A random walk path
step_algorithm_with_equal_weights <- function(graph, step.count, start.node, options=c()) {
    step_algorithm_basic(graph, step.count, start.node, options=c(weights=NA))
}

#' Basic utility transformer. Takes the max visit count for the whole network,
#' divides each cell by it. This value indicates how clustered the node is on
#' the scale of the whole network.
#'
#' This metric may be useful for testing due to its simplicity, however, it's
#' likely less useful for analysis
#'
#' @param   utility.matrix  Sparse utility matrix object of concern
#' @param   options['max']  Max utility statistic. This is calculated
#'                          automatically in random_walker_utility but it can
#'                          be overridden manually
#'
#' @return  The transformed utility.matrix
utility_transformer_basic <- function(utility.matrix, options=c()) {
    utility.matrix / options['max']
}


#' Base random walker utility function, based on the design employed in the
#' progress report
#'
#' This implementation separates out two key components, both of which make it
#' easier to implement variations on the simulation:
#' - the step.algorithm: a function (e.g. as defined above) that calculates the
#'   random walk. This is separated out primarily to control how the edges of
#'   the graph are weighted; different functions may calculate/manipulate the
#'   weights in different ways
#' - the utility.transformer: this function takes the utility matrix, full of
#'   node visit counts from the walk, and transforms them in some way. Because
#'   the visit counts on their own aren't useful as "scores," this enables
#'   variation in how the simulation defines a particular story's "score" in
#'   relation to the number of times it was visited
#'
#' @param   graph                       igraph graph object
#' @param   walker.count                Number of random walkers to simulate
#' @param   step.count                  Constant number of steps each walker
#'                                      takes, or a vector of length
#'                                      `walker.count` corresponding to the
#'                                      length of each walker's walk
#' @param   start.node                  Index of the node each walker will start
#'                                      on, or a vector of nodes to be selected
#'                                      from randomly If NA, a node will be
#'                                      chosen entirely at random
#' @param   step.algorithm              Function used to calculate the random
#'                                      walk.
#' @param   utility.transformer         Function used to transform the utility
#'                                      matrix
#' @param   step.algorithm.options      Options passed to step.algorithm
#' @param   utility.transformer.options Options passed to utility.transformer 
#'
#' @return  A "utility matrix" representative of the simulation
random_walker_utility <- function(
    graph,
    walker.count,
    step.count,
    start.node,
    step.algorithm=step_algorithm_basic,
    utility.transformer=utility_transformer_basic,
    step.algorithm.options=c(),
    utility.transformer.options=c()
) {
    story.nodes <- V(graph)[which(V(graph)$class == 'story')]

    # Sparse utility matrix. Each row corresponds to all the stories visited
    # (v > 0) or not (v = 0). Each column corresponds to all the visits by
    # a particular walker.
    utility.matrix <- Matrix(0, nrow=walker.count,
                                ncol=length(story.nodes),
                                sparse=TRUE)

    # This will be accumulated over the course of the simulation
    utility.max <- 0

    for (walker.index in 1:walker.count) {
        selected.start.node <- start.node

        # If a vector, etc. was provided...
        if (length(start.node) > 1) {
            # Randomly choose one of them
            selected.start.node <- sample(start.node, 1)
        }

        # Take a random walk according to the selected step.algorithm
        path <- step.algorithm(graph, step.count, start.node, options=step.algorithm.options)

        # Collect visit counts
        nodes.visited <- as.vector(path)
        visit.counts <- tabulate(as.vector(path), nbins=vcount(graph))

        # Update the maximum utility
        utility.max <- max(utility.max, visit.counts)
        utility.matrix[walker.index,] <- visit.counts
    }

    # If a maximum value wasn't proved by the end-user, we provide the one
    # calculated as a part of the simulation
    if (is.null(utility.transformer.options['max'])) {
        utility.transformer.options['max'] <- utility.max
    }

    # Apply a transformation to the utility matrix so that the visit counts
    # correspond to some simulated score
    utility.matrix <- utility.transformer(utility.matrix, options=utility.transformer.options)

    utility.matrix
}


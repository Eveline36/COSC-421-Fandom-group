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

#' Deprecated; use utility_transformer_with_transitivity instead
utility_transformer_basic <- function(graph, utility.matrix, options=c()) {
    utility_transformer_with_transitivity(graph, utility.matrix, options)
}

#' Basic utility transformer. Takes the max visit count for the whole network,
#' divides each cell by it. This value indicates how clustered the node is on
#' the scale of the whole network.
#'
#' This metric may be useful for testing due to its simplicity, however, it's
#' likely less useful for analysis
#'
#' @param   graph           One-mode projection on the stories
#' @param   utility.matrix  Sparse utility matrix object of concern
#' @param   options['max']  Max utility statistic. This is calculated
#'                          automatically in random_walker_utility but it can
#'                          be overridden manually
#'
#' @return  The transformed utility.matrix
utility_transformer_with_transitivity <- function(graph, utility.matrix, options=c()) {
    utility.matrix / options['max']
}

#' Utility transformer for a walker that's easy to please. They give the maximum
#' score to every story they encounter (1)
#'
#' See utility_transformer_basic for parameters
utility_transformer_for_an_easily_pleased_walker <- function(graph, utility.matrix, options=c()) {
    utility.matrix[which(utility.matrix > 0)] <- 1
    utility.matrix
}

#' Basic kudos-based utility transformation function
#'
#' This function considers all stories visited at least once as being worth
#' consideration, and all those not visited as being worth no consideration.
#'
#' Being visited more than once doesn't affect the score, so this function
#' reveals limited information about the local topology
#'
#' This function will transform utilities such that they equal the normalized
#' kudos score, based on the options['kudos'] distribution. If the parameter
#' isn't provided, this function will
#'
#' This function assumes each story vertex has a $kudos attribute corresponding
#' to the number of kudos it's recieved
#'
#' @param   options['kudos_distribution']   Kudos distribution. If not given,
#'                                          this function will search for the
#'                                          global kudos distribution csv file
#'                                          in subsets/kudos_asc.csv and use it
#' @param   options['minimum']              Minimum score. Default: 0.1
#' @param   options['middle.spread']        Portion of the distribution to
#'                                          consider. Default: 1 (all of it)
#'
#' See utility_transformer_basic for additional parameters
utility_transformer_with_kudos <- function(utility.matrix, options=c()) {
    kudos.distribution <- options['kudos_distribution']
    if (is.na(kudos.distribution)) {
        kudos.distribution <- read.csv('../../subsets/kudos_asc.csv', header=FALSE)
        kudos.distribution <- kudos.distribution$V1
    }

    # Default: set the minimum score to 0.1. That way, all stories visited have
    # at least a little bit of preference
    minimum.score <- options['minimum']
    if (is.na(minimum.score)) { minimum.score <- 0.1 }

    # Default: use all the kudos data. This relationship will likely be less
    # linear but it will have more meaningful values for low kudos values
    middle.spread <- options['minimum']
    if (is.na(middle.spread)) { middle.spread <- 1 }

    kudos.transformer <- kudos_transformer_factory(kudos_distribution,
                                                   minimum.score,
                                                   middle.spread)

    # Construct the kudos matrix. This will have the same dimensions as the
    # utility matrix, however, each cell will be equal to the story's kudos
    # count

    kudos.matrix <- replicate(nrow(utility.matrix), V(graph)$kudos) |> t()

    # Normalize the kudos scores with the kudos transformer function
    kudos.matrix <- kudos.transformer(kudos.matrix)

    # Cap the utility at 1 initially, so it doesn't scale the result
    utility.matrix[which(utility.matrix > 0)] <- 1

    utility.matrix * kudos.matrix
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
    # Sparse utility matrix. Each row corresponds to all the stories visited
    # (v > 0) or not (v = 0). Each column corresponds to all the visits by
    # a particular walker.
    utility.matrix <- Matrix(0, nrow=walker.count,
                                ncol=vcount(graph),
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
    utility.matrix <- utility.transformer(graph, utility.matrix, options=utility.transformer.options)

    utility.matrix
}


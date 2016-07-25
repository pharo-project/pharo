I'm able to detect all the cycles in a package dependency graph.

Analysis is done in three steps: build the dependency graph, isolating the strongly connected components (SCC), and for each SCC detect all the elementary cycles.

Use the message runAlgorithm to run the algorithm to retrieve the elementary cycles in the package dependency graph.

 At the end, all the cycles are in the collection "cycles".
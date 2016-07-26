accessing
initialize 

	super initialize.
	circuits := OrderedCollection new. "use that instead of Set"
	dijkstra := MalDijkstra new.

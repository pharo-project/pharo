visiting
copyDirectory: aReference
	| directory |
	directory := dest resolve: (aReference relativeTo: source).
	directory createDirectory
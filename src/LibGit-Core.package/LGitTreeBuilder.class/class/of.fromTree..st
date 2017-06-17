instance creation
of: repository fromTree: aTree
	^ (self of: repository)
		initializeWithTree: aTree;
		yourself
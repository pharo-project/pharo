visiting
acceptVisitor: aVisitor forDefinitions: aCollection
	(MCDependencySorter sortItems: aCollection)
		do: [ :ea | ea accept: aVisitor ]
		displayingProgress: 'Writing definitions...'
selection
dependencyIndex: anInteger
	dependencyIndex := anInteger.
	dependencyIndex > 0
		ifTrue: [self repositoryIndex: 0].
	self changed: #dependencyIndex; changed: #description.
	self changedButtons.
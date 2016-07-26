selection
repositoryIndex: anInteger
	repositoryIndex := anInteger.
	repositoryIndex > 0
		ifTrue: [self dependencyIndex: 0].
	self changed: #repositoryIndex; changed: #description.
	self changedButtons.
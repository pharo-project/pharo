selection
index: anInteger
	self dependencyIndex > 0 ifTrue: [^self dependencyIndex: anInteger].
	self repositoryIndex > 0 ifTrue: [^self repositoryIndex: anInteger].
	anInteger > 0 ifTrue: [self error: 'cannot select']
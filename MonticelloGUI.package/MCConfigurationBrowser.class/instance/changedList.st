selection
changedList
	self dependencyIndex > 0 ifTrue: [ ^ self changed: #dependencyList ].
	self repositoryIndex > 0 ifTrue: [ ^ self repositoryListChanged ].
	self error: 'nothing selected'
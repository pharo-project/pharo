selection
list
	self dependencyIndex > 0 ifTrue: [^self dependencies].
	self repositoryIndex > 0 ifTrue: [^self repositories].
	^#()
replacing
replaceNode: aNode withNode: anotherNode 
	value == aNode ifTrue: [self value: anotherNode]
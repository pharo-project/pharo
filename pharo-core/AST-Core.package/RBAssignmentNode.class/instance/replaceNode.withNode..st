replacing
replaceNode: aNode withNode: anotherNode 
	value == aNode ifTrue: [self value: anotherNode].
	variable == aNode ifTrue: [self variable: anotherNode]
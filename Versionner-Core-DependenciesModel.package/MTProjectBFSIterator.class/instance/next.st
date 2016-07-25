protocol
next
	| currentElement |
	
	currentElement := queue nextOrNil.
	currentElement ifNotNil: [
		((self hasChildNodes: currentElement) and: [ self isNotVisited: currentElement ])
			ifTrue: [ queue nextPutAll: currentElement dependencies.
					 alreadyVisited add: currentElement ] ].
	^ currentElement
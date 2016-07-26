accessing
childrenFor: anItem
	
	| children |
	
	children := (childrenBlock value: anItem).
	
	^sortingBlock ifNil: [ children ] ifNotNil: [ children sort: sortingBlock ]
	
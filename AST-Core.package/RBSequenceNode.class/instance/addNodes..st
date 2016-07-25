adding nodes
addNodes: aCollection
	aCollection do: [ :each | each parent: self ].
	(statements notEmpty and: [ statements last isReturn ])
		ifTrue: [ self error: 'Cannot add statement after return node' ].
	statements := statements asOrderedCollection
		addAll: aCollection;
		yourself.
	^ aCollection
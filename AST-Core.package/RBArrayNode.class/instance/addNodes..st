adding nodes
addNodes: aCollection
	statements := statements asOrderedCollection
		addAll: aCollection;
		yourself.
	aCollection do: [ :each | each parent: self ].
	^ aCollection
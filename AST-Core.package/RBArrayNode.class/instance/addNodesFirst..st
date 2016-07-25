adding nodes
addNodesFirst: aCollection
	statements := statements asOrderedCollection
		addAllFirst: aCollection;
		yourself.
	aCollection do: [ :each | each parent: self ].
	^ aCollection
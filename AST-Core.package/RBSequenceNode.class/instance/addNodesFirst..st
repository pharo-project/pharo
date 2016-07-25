adding nodes
addNodesFirst: aCollection
	aCollection do: [ :each | each parent: self ].
	statements := statements asOrderedCollection
		addAllFirst: aCollection;
		yourself.
	^ aCollection
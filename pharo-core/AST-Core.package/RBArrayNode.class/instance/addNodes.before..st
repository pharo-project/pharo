adding nodes
addNodes: aCollection before: anotherNode 
	aCollection do: [ :each | self addNode: each before: anotherNode ].
	^ aCollection
running
popStackAndUpdateCurrentScc: aComponent until: aTarjanNode
	| currentNode |
	[ currentNode := stack pop.
	currentNode popped.
	aComponent add: currentNode ] 
		doWhileFalse: [ currentNode = aTarjanNode ].
	sccs add: aComponent.
	aComponent do: [ :each | each cycleNodes: aComponent ]
private
popStackAndUpdateCurrentSCC: aRelation until: aPDPackage
	| currentNode |	
	[ currentNode := stack pop.
	currentNode
		inStack: false;
		beIncluded.
	aRelation addPackage: currentNode ] doWhileFalse: [ currentNode = aPDPackage ].
	stronglyConnectedComponents add: aRelation
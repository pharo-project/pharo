running
iteratedAsArray
	| result |
	result := OrderedCollection new.	
		
	HiNodesAndLinksIterator new 
		linkBuilders: { parentsLinkBuilder. originLinkBuilder };
		values: reversedNodes reversed;
		doWithNodes: [ :item | result add: item ]
		doWithLinks: [ :link | result add: link ].
	
	^ result asArray
initialization
refreshWithLogSelected: item

	item ifNil: [ ^self ].

	item log refresh.

	logModel 
		log: item log; 
		refresh.
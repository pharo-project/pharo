refreshing
refreshWithLogSelected: item

	| freshLog |
	freshLog := item
		ifNil: [ EpLog newNull ]
		ifNotNil: [ item log ].
	
	logModel 
		log: freshLog; 
		refresh;
		cleanEntryContentModel.

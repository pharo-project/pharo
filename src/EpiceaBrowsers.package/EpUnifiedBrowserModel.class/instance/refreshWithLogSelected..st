refreshing
refreshWithLogSelected: item

	| freshLog |
	freshLog := item
		ifNil: [ EpLog newNull ]
		ifNotNil: [ item log refresh; yourself].
	
	logModel 
		log: freshLog; 
		refresh;
		cleanEntryContentModel.

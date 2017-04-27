refreshing
refreshWithLogSelected: item

	| freshLog |
	freshLog := item
		ifNil: [ EpLog newNull ]
		ifNotNil: [ item log ].
	
	logModel
		theLog: freshLog;
		removeAllFilters;
		refresh;
		cleanEntryContentModel.

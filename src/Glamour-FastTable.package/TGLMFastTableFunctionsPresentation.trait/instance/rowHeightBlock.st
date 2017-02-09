public api
rowHeightBlock
	^ self parameters 
		at: 'rowHeight'
		ifAbsent: [ self class defaultRowHeight ]
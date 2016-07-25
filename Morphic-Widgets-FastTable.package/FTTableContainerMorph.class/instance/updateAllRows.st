updating
updateAllRows
	self owner isShowColumnHeaders 
		ifTrue: [ self updateHeaderRow ].
	self updateExposedRows.
	
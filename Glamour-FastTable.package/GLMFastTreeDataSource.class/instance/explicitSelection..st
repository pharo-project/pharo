selecting
explicitSelection: anObject
	self table selectRowIndexes: (self searchText: anObject asString)
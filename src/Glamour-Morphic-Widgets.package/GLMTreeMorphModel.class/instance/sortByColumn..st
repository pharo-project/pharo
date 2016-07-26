callbacks
sortByColumn: aGlamourColumn

	columnToSortBy := (columnToSortBy == aGlamourColumn 
		ifTrue: [ nil ]
		ifFalse: [ aGlamourColumn ]).
	self updateRoots
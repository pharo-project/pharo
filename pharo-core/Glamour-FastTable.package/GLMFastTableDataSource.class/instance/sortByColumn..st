accessing
sortByColumn: aGlamourColumn
	columnToSortBy := (columnToSortBy == aGlamourColumn 
		ifTrue: [ nil ]
		ifFalse: [ aGlamourColumn ]).
	elements := self elementsSorted.
	self glamourPresentation changed.
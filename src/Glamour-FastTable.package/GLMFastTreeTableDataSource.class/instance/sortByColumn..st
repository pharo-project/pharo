sorting
sortByColumn: aGlamourColumn
	columnToSortBy := (columnToSortBy == aGlamourColumn 
		ifTrue: [ nil ]
		ifFalse: [ aGlamourColumn ]).
		
	columnToSortBy 
		ifNotNil: [ self rootItem children: (self rootItem children sorted: self sortBlock) ]
		ifNil: [ self rootItem data: self basicRoots ].

	self glamourPresentation changed.
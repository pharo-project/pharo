accessing
realSearch
	| founds |
	founds := self table dataSource searchText: pattern.
	self table
		selectRowIndex: (founds ifEmpty: [ 1 ] ifNotEmpty: [ founds first ]);
		highlightRowIndexes: founds.
	^ founds notEmpty
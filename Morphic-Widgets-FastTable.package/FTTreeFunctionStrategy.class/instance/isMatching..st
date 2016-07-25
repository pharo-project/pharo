accessing
isMatching: anItem
	^ (dataSource toString: anItem) beginsWith: pattern
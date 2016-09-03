accessing
newDataSourceMatching: aFTFilter
	| newItems |
	newItems := items class withAll: (items collect: [ :coll | coll select: [ :each | aFTFilter matches: (self toString: each) ] ]).
	^ self class new items: newItems
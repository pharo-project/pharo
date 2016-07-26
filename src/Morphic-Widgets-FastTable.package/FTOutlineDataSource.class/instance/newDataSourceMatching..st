accessing
newDataSourceMatching: aRegex
	| newItems |
	newItems := items class withAll: (items collect: [ :coll | coll select: [ :each | (aRegex matchesIn: (self toString: each)) notEmpty ] ]).
	^ self class new items: newItems
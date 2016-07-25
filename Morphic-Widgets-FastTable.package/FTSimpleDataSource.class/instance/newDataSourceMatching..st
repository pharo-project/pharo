accessing
newDataSourceMatching: aRegex
	| newElements |
	newElements := Array withAll: (elements select: [ :each | (aRegex matchesIn: (self toString: each)) notEmpty ]).
	^ self class new elements: newElements
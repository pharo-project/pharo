tests
testMergingOverlappingRects

	| coll merge |
	coll := OrderedCollection new.
	coll add: (Rectangle left: 5 right: 10 top: 0 bottom: 15).
	coll add: (Rectangle left: 0 right: 15 top: 5 bottom: 10).
	merge := Rectangle merging: coll.
	self assert: merge = (Rectangle left: 0 right: 15 top: 0 bottom: 15).
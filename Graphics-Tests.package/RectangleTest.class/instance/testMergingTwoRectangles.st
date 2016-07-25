tests
testMergingTwoRectangles

	| coll merge |
	coll := OrderedCollection new.
	coll add: (Rectangle left: 1 right: 1 top: 1 bottom: 1).
	coll add: (Rectangle left: 10 right: 10 top: 10 bottom: 10).

	merge := Rectangle merging: coll.
	self assert: merge = (Rectangle left: 1 right: 10 top: 1 bottom: 10).
tests
testMergingDisjointRectangles

	| aCollection merge |
	aCollection := OrderedCollection new.
	aCollection add: (Rectangle left: -10 right: 0 top: -10 bottom: 0).
	aCollection add: (Rectangle left: 0 right: 10 top: 0 bottom: 10).
	merge := Rectangle merging: aCollection.
	self assert: merge = (Rectangle left: -10 right: 10 top: -10 bottom: 10).
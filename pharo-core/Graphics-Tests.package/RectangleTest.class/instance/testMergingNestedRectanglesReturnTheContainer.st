tests
testMergingNestedRectanglesReturnTheContainer

	| aCollection merge |
	aCollection := OrderedCollection new.
	aCollection add: (Rectangle left: 1 right: 10 top: 1 bottom: 10).
	aCollection add: (Rectangle left: 4 right: 5 top: 4 bottom: 5).
	merge := Rectangle merging: aCollection.
	self assert: merge = aCollection first.
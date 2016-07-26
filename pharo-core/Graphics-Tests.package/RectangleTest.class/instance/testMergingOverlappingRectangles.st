tests
testMergingOverlappingRectangles
	| aCollection merge |
	aCollection := OrderedCollection new.
	aCollection
		add: (Rectangle
				left: 5
				right: 10
				top: 0
				bottom: 15);
		
		add: (Rectangle
				left: 0
				right: 15
				top: 5
				bottom: 10).
	merge := Rectangle merging: aCollection.
	self assert: merge
			= (Rectangle
					left: 0
					right: 15
					top: 0
					bottom: 15)
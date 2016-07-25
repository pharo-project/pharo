tests
testMergingOneRectangleReturnsThisRectangle
	| aCollection mergingRectangle |
	aCollection := OrderedCollection new.
	aCollection add: rectangle1.
	mergingRectangle := Rectangle merging: aCollection.
	self assert: mergingRectangle = aCollection first
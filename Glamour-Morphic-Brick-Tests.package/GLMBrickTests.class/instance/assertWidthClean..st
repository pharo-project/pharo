asserting
assertWidthClean: aBrick

	self assert: aBrick wrappedBounds widthState equals: #clean.
	self assert: aBrick wrappedBounds isWidthDirty not.
	self assert: aBrick wrappedBounds isWidthClean
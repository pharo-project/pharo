asserting
assertWidthDirty: aBrick

	self assert: aBrick wrappedBounds widthState equals: #dirty.
	self assert: aBrick wrappedBounds isWidthDirty.
	self assert: aBrick wrappedBounds isWidthClean not
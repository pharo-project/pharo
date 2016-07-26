tests-marker
testMarkerOneStaticInParentHeight
	|root brick|
	
	root := self newBrickWithFullFill.
	brick := self newBrickWithStaticExtent.
	root addBrickBack: brick.
	self markFullyClean: root.
	
	brick wrappedBounds markHeightDirty.
	GLMBrickLayouter uniqueInstance markDependent: brick brickApi.
	
	self assert: brick wrappedBounds isWidthClean.
	self assert: brick wrappedBounds isHeightDirty.
	
	self assert: root wrappedBounds isWidthClean.
	self assert: root wrappedBounds isHeightClean.
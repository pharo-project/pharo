tests-marker
testMarkerOneParentHeight
	|brick|
	
	brick := self newBrickWithFullFill.
	self markFullyClean: brick.
	
	brick wrappedBounds markHeightDirty.
	GLMBrickLayouter uniqueInstance markDependent: brick brickApi.
	
	self assert: brick wrappedBounds isWidthClean.
	self assert: brick wrappedBounds isHeightDirty.
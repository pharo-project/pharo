tests-marker
testMarkerOneChildrenWidth
	|brick|
	
	brick := self newBrickWithFullDependsOnChildren.
	self markFullyClean: brick.
	
	brick wrappedBounds markWidthDirty.
	GLMBrickLayouter uniqueInstance markDependent: brick brickApi.
	
	self assert: brick wrappedBounds isWidthDirty.
	self assert: brick wrappedBounds isHeightClean.
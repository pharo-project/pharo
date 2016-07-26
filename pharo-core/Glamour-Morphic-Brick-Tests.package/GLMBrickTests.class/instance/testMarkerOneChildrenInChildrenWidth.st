tests-marker
testMarkerOneChildrenInChildrenWidth
	|root brick|
	
	root := self newBrickWithFullDependsOnChildren.
	brick := self newBrickWithFullDependsOnChildren.
	root addBrickBack: brick.
	self markFullyClean: root.
	
	brick wrappedBounds markWidthDirty.
	GLMBrickLayouter uniqueInstance markDependent: brick brickApi.
	
	self assert: brick wrappedBounds isWidthDirty.
	self assert: brick wrappedBounds isHeightClean.
	
	self assert: root wrappedBounds isWidthDirty.
	self assert: root wrappedBounds isHeightClean.
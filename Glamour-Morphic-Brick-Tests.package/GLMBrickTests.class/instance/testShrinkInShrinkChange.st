tests-layouter
testShrinkInShrinkChange
	|root brick1 brick2|
	
	root := self newBrickWithFullDependsOnChildren.
	brick1 := self newBrickWithFullDependsOnChildren.
	brick2 := self newBrickWithSmallStaticExtent.
	
	root addBrickBack: brick1.
	brick1 addBrickBack: brick2.
	
	self assertAllSubbricksAreClean: root.
	self assert: root brickBounds equals: (0@0 extent: self staticSmallExtent).
	
	brick2
		width: self staticWidth;
		height: self staticHeight.
	
	self assert: root brickBounds equals: (0@0 extent: self staticExtent).
	
tests-layouter
testTwoShrinkInShrinkChange
	|root brick1 brick2 brick3|
	
	root := self newBrickWithFullDependsOnChildren.
	brick1 := self newBrickWithFullDependsOnChildren.
	brick2 := self newBrickWithSmallStaticExtent.
	brick3 := self newBrickWithSmallStaticExtent.
	brick1 useVerticalLinearLayout.
	
	root addBrickBack: brick1.
	brick1 addBrickBack: brick2.
	brick1 addBrickBack: brick3.
	
	self assertAllSubbricksAreClean: root.
	self assert: root brickBounds equals: (0@0 extent: (self staticSmallWidth @ (2* self staticSmallHeight))).
	self assert: brick1 brickBounds equals: (0@0 extent: (self staticSmallWidth @ (2* self staticSmallHeight))).
	self assert: brick2 brickBounds equals: (0@0 extent: self staticSmallExtent).
	self assert: brick3 brickBounds equals: (0@self staticSmallHeight extent: self staticSmallExtent).
	
	brick2
		width: self staticWidth;
		height: self staticHeight.
	
	self assert: root brickBounds equals: (0@0 extent: (self staticWidth @ (self staticHeight + self staticSmallHeight))).
	self assert: brick1 brickBounds equals: (0@0 extent: (self staticWidth @ (self staticHeight + self staticSmallHeight))).
	self assert: brick2 brickBounds equals: (0@0 extent: self staticExtent).
	self assert: brick3 brickBounds equals: (0@self staticHeight extent: self staticSmallExtent).
	
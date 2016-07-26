tests-geometry
testMaxHeightTwoParentDependencies
	|root brick1 brick2|
	
	root := self newBrickWithStaticExtent.
	root useVerticalLinearLayout.
	brick1 := self newBrickWithFullFill.
	brick1 maxHeight: self staticSmallHeight.
	brick2 := self newBrickWithFullFill.
	
	root addAllBricks:  { brick1 . brick2 }.
	self assertAllSubbricksAreClean: root.
	
	self assert: root extent equals: self staticExtent.
	self assert: brick1 extent equals: (self staticWidth @ self staticSmallHeight).
	self assert: brick2 extent equals: (self staticWidth @ (self staticHeight - self staticSmallHeight))
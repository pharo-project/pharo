tests-geometry
testMinHeightTwoParentDependencies
	|root brick1 brick2 height|
	height := 80.
	
	root := self newBrickWithStaticExtent.
	root useVerticalLinearLayout.
	brick1 := self newBrickWithFullFill.
	brick1 vSpaceFill: 50. "50% = 50px"
	brick1 minHeight: height. "min is 80px > 50%"
	brick2 := self newBrickWithFullFill.
	
	root addAllBricks:  { brick1 . brick2 }.
	self assertAllSubbricksAreClean: root.
	self assert: root extent equals: self staticExtent.
	self assert: brick1 extent equals: (self staticWidth @ height).
	self assert: brick2 extent equals: (self staticWidth @ (self staticHeight - height))
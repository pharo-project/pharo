tests-geometry
testMinAndMaxHeightTwoParentDependencies
	|root brick1 brick2 minHeight maxHeight|
	minHeight := 60.
	maxHeight := 20.
	
	root := self newBrickWithStaticExtent.
	root useVerticalLinearLayout.
	brick1 := self newBrickWithFullFill.
	brick1 vSpaceFill: 50. "50% = 50px"
	brick1 minHeight: minHeight. "min is 60px > 50%"
	brick2 := self newBrickWithFullFill.
	brick2 maxHeight: maxHeight. "max is 20 < 100%"
	
	root addAllBricks:  { brick1 . brick2 }.
	self assertAllSubbricksAreClean: root.
	self assert: root extent equals: self staticExtent.
	self assert: brick1 extent equals: (self staticWidth @ minHeight).
	self assert: brick2 extent equals: (self staticWidth @ maxHeight)
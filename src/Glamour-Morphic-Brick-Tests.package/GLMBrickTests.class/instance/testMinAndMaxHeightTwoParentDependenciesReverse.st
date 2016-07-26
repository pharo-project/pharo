tests-geometry
testMinAndMaxHeightTwoParentDependenciesReverse
	|root brick1 brick2 minHeight maxHeight|
	minHeight := 20.
	maxHeight := 60.
	
	root := self newBrickWithStaticExtent.
	root useVerticalLinearLayout.
	brick1 := self newBrickWithFullFill.
	brick1 vSpaceFill: 0. "0% = 0px"
	brick1 minHeight: minHeight. "min is 20px > 0%"
	brick2 := self newBrickWithFullFill.
	brick2 maxHeight: maxHeight. "max is 60 < 100%"
	
	root addAllBricks:  { brick1 . brick2 }.
	self assertAllSubbricksAreClean: root.
	self assert: root extent equals: self staticExtent.
	self assert: brick1 extent equals: (self staticWidth @ minHeight).
	self assert: brick2 extent equals: (self staticWidth @ maxHeight)
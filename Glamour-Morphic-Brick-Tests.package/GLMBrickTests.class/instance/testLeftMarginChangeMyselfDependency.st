tests-geometry
testLeftMarginChangeMyselfDependency
	|root margin newMargin brick|
	margin := 10.
	newMargin := 20.
	
	root := self newBrickWithStaticExtent.
	brick := self newBrickWithSmallStaticExtent.
	brick marginLeft: margin.
	root addBrickBack: brick.

	self assertAllSubbricksAreClean: root.
	self assert: root extent equals: self staticExtent.
	self assert: brick extent equals: self staticSmallExtent.
	self assert: brick brickBounds equals: (margin@0 extent: self staticSmallExtent).
	
	brick marginLeft: newMargin.
	brick doLayoutForce.
	
	self assertAllSubbricksAreClean: root.
	self assert: root extent equals: self staticExtent.
	self assert: brick extent equals: self staticSmallExtent.
	self assert: brick brickBounds equals: (newMargin@0 extent: self staticSmallExtent).
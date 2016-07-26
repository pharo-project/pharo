tests-floating
testRightFloatingMySelfDependencyWithMargin
	|root margin floating brick|
	margin := 10.
	floating := #right.
	
	root := self newBrickWithStaticExtent.
	brick := self newBrickWithSmallStaticExtent.
	brick layout floating: floating.
	brick marginRight: margin.
	root addBrickBack: brick.

	self assertAllSubbricksAreClean: root.
	self assert: root extent equals: self staticExtent.
	self assert: brick extent equals: self staticSmallExtent.
	self assert: brick brickBounds equals: ((self staticWidth - margin - self staticSmallWidth)@0 extent: self staticSmallExtent)
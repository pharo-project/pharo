tests-floating
testLeftFloatingMySelfDependencyWithMargin
	|root floating margin brick|
	margin := 10.
	floating := #left.
	
	root := self newBrickWithStaticExtent.
	brick := self newBrickWithSmallStaticExtent.
	brick marginLeft: margin.
	brick layout floating: floating.
	root addBrickBack: brick.

	self assertAllSubbricksAreClean: root.
	self assert: root extent equals: self staticExtent.
	self assert: brick extent equals: self staticSmallExtent.
	self assert: brick brickBounds equals: (margin@0 extent: self staticSmallExtent)
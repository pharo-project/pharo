tests-geometry
testBottomMarginMyselfDependency
	|root margin brick|
	margin := 10.
	
	root := self newBrickWithStaticExtent.
	brick := self newBrickWithSmallStaticExtent.
	brick marginBottom: margin.
	root addBrickBack: brick.

	self assertAllSubbricksAreClean: root.
	self assert: root extent equals: self staticExtent.
	self assert: brick extent equals: self staticSmallExtent.
	self assert: brick brickBounds equals: (0@0 extent: self staticSmallExtent)
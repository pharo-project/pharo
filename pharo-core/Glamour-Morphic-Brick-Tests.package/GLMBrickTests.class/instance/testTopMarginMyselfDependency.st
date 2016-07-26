tests-geometry
testTopMarginMyselfDependency
	|root margin brick|
	margin := 10.
	
	root := self newBrickWithStaticExtent.
	brick := self newBrickWithSmallStaticExtent.
	brick marginTop: margin.
	root addBrickBack: brick.

	self assertAllSubbricksAreClean: root.
	self assert: root extent equals: self staticExtent.
	self assert: brick extent equals: self staticSmallExtent.
	self assert: brick brickBounds equals: (0@margin extent: self staticSmallExtent)
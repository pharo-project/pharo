tests-floating
testRightFloatingMySelfDependency
	|root floating brick|
	floating := #right.
	
	root := self newBrickWithStaticExtent.
	brick := self newBrickWithSmallStaticExtent.
	brick layout floating: floating.
	root addBrickBack: brick.

	self assertAllSubbricksAreClean: root.
	self assert: root extent equals: self staticExtent.
	self assert: brick extent equals: self staticSmallExtent.
	self assert: brick brickBounds equals: ((self staticWidth - self staticSmallWidth)@0 extent: self staticSmallExtent)
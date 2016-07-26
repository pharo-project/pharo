tests-floating
testLeftFloatingMySelfDependency
	|root floating brick|
	floating := #left.
	
	root := self newBrickWithStaticExtent.
	brick := self newBrickWithSmallStaticExtent.
	brick layout floating: floating.
	root addBrickBack: brick.

	self assertAllSubbricksAreClean: root.
	self assert: root extent equals: self staticExtent.
	self assert: brick extent equals: self staticSmallExtent.
	self assert: brick brickBounds equals: (0@0 extent: self staticSmallExtent)
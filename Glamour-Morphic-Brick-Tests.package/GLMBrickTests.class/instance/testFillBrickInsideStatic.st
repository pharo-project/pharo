tests-layouter
testFillBrickInsideStatic
	|root brick|
	
	root := self newBrickWithStaticExtent.
	brick := self newBrickWithFullFill.
	brick brickApi layout hState isOnParent.
	brick brickApi layout vState isOnParent.
	root addBrickBack: brick.
	
	self assertAllSubbricksAreClean: root.
	self assert: brick extent equals: root extent
	
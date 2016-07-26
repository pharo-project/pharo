tests-layouter
testBrickDependsOnChildrenOneStatic
	|root brick|
	
	root := self newBrickWithFullDependsOnChildren.
	brick := self newBrickWithStaticExtent.
	
	root addBrickBack: brick.
	
	self assert: root brickApi layout hState isOnChildren.
	self assert: root brickApi layout vState isOnChildren.
	
	self assertAllSubbricksAreClean: root.
	
	self assert: root extent equals: brick extent
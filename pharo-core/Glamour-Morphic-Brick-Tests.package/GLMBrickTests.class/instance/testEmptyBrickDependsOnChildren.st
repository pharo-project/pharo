tests-layouter
testEmptyBrickDependsOnChildren
	|brick|
	
	brick := self newBrickWithFullDependsOnChildren.
	
	self assert: brick brickApi layout hState isOnChildren.
	self assert: brick brickApi layout vState isOnChildren.
	
	self assertAllSubbricksAreDirty: brick.
	brick doLayout.
	self assertAllSubbricksAreClean: brick.
	
	self assert: brick extent equals: 0@0
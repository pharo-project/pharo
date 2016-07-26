tests-layouter
testOneStaticBrick
	|brick|
	
	brick := self newBrickWithStaticExtent.
	
	self assert: brick brickApi layout hState isOnMyself.
	self assert: brick brickApi layout vState isOnMyself.
	self assertBrickClean: brick.
	self assert: brick extent equals: self staticExtent
	
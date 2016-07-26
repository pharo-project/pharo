tests
testClassRemoval

	aClass := classFactory newClass.

	self assert: (self countLogEventsWith: EpClassRemoval) equals: 0.

	aClass removeFromSystem.
	
	self assert: (self countLogEventsWith: EpClassRemoval) equals: 1.

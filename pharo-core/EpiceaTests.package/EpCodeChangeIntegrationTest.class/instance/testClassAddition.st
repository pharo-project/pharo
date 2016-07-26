tests
testClassAddition

	self assert: (self countLogEventsWith: EpClassAddition) equals: 0.

	classFactory newClass.
	
	self assert: (self countLogEventsWith: EpClassAddition) equals: 1.

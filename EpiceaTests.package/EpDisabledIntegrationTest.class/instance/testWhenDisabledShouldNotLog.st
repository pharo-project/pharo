tests
testWhenDisabledShouldNotLog
	classFactory newClass.

	self assert: (self countLogEventsWith: EpClassAddition) equals: 0
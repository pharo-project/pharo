tests
testWhenEnabledShouldLog
	monitor enable.

	classFactory newClass.

	self assert: (self countLogEventsWith: EpClassAddition) equals: 1
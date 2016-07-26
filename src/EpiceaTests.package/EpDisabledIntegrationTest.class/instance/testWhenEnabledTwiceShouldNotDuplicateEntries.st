tests
testWhenEnabledTwiceShouldNotDuplicateEntries
	2 timesRepeat: [ monitor enable ].

	classFactory newClass.

	self assert: (self countLogEventsWith: EpClassAddition) equals: 1
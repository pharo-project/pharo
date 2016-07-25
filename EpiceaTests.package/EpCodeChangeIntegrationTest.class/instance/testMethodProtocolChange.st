tests
testMethodProtocolChange

	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: #accessing.
	aClass compile: 'fortyTwo ^42' classified: #testing.

	self assert: (self countLogEventsWith: EpMethodModification) equals: 2
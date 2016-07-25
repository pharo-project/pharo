tests
testMethodModification

	aClass := classFactory newClass.
	aClass compile: 'fortyTwo 42'.
	aClass compile: 'fortyTwo ^42'.

	self assert: (self countLogEventsWith: EpMethodModification) equals: 1
tests
testMethodAddition

	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42'.

	self assert: (self countLogEventsWith: EpMethodAddition) equals: 1
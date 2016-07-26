tests
testMethodModificationWithWideString

	aClass := classFactory newClass.
	aClass compile: self wideStringSourceCode.
	aClass compile: self wideStringSourceCode, ' '.

	self assert: (self countLogEventsWith: EpMethodModification) equals: 1
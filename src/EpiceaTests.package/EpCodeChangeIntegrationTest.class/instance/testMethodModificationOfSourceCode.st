tests
testMethodModificationOfSourceCode

	| events event |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42'.
	aClass compile: 'fortyTwo ^nil'.

	events := self allLogEventsWith: EpMethodModification.
	self assert: events size equals: 1.
	event := events anyOne.
	self assert: event oldMethod sourceCode equals: 'fortyTwo ^42'.
	self assert: event newMethod sourceCode equals: 'fortyTwo ^nil'.

tests
testMethodModificationOfProtocol

	| events event |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'old'.
	(aClass>>#fortyTwo) protocol: 'new'.

	events := self allLogEventsWith: EpMethodModification.
	self assert: events size equals: 1.
	event := events anyOne.
	self assert: event oldMethod protocol equals: 'old'.
	self assert: event newMethod protocol equals: 'new'.

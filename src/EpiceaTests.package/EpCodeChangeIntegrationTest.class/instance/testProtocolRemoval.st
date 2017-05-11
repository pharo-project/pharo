tests
testProtocolRemoval

	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: #testing42.
	aClass removeProtocol: #testing42.

	self assert: (self countLogEventsWith: EpProtocolRemoval) equals: 1
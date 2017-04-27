tests
testProtocolRemoval

	| aClass |
	aClass := classFactory newClass.
	aClass organization addCategory: 'protocol'.
	aClass removeProtocol: 'protocol'.
	self setHeadAsInputEntry.
	aClass organization addCategory: 'protocol'.

	self assert: inputEntry content class equals: EpProtocolRemoval.
	self assert: (aClass organization protocolOrganizer hasProtocolNamed: 'protocol').
	self applyInputEntry.
	self deny: (aClass organization protocolOrganizer hasProtocolNamed: 'protocol').

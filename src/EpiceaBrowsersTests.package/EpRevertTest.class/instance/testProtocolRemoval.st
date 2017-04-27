tests
testProtocolRemoval

	| aClass |
	aClass := classFactory newClass.
	aClass organization addCategory: 'protocol'.
	aClass removeProtocol: 'protocol'.
	self setHeadAsInputEntry.

	self assert: inputEntry content class equals: EpProtocolRemoval.
	self deny: (aClass organization protocolOrganizer hasProtocolNamed: 'protocol').
	self revertInputEntry.
	self assert: (aClass organization protocolOrganizer hasProtocolNamed: 'protocol').

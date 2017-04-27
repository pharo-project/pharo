tests
testProtocolAddition

	| aClass |
	aClass := classFactory newClass.
	aClass organization addCategory: 'protocol'.
	self setHeadAsInputEntry.
	aClass removeProtocol: 'protocol'.

	self assert: inputEntry content class equals: EpProtocolAddition.
	self deny: (aClass organization protocolOrganizer hasProtocolNamed: 'protocol').
	self applyInputEntry.
	self assert: (aClass organization protocolOrganizer hasProtocolNamed: 'protocol').

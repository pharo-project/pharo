tests
testProtocolAddition

	| aClass |
	aClass := classFactory newClass.
	aClass organization addCategory: 'protocol'.
	self setHeadAsInputEntry.

	self assert: inputEntry content class equals: EpProtocolAddition.
	self assert: (aClass organization protocolOrganizer hasProtocolNamed: 'protocol').
	self revertInputEntry.
	self deny: (aClass organization protocolOrganizer hasProtocolNamed: 'protocol').

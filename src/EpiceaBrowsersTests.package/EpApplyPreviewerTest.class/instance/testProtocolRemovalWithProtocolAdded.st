tests
testProtocolRemovalWithProtocolAdded

	| aClass |
	aClass := classFactory newClass.
	aClass organization addCategory: 'protocol'.
	aClass removeProtocol: 'protocol'.
	self setHeadAsInputEntry.
	aClass organization addCategory: 'protocol'.

	self assertOutputsAnEventWith: [:output |
		self assert: output class equals: EpProtocolRemoval.
		self assert: output protocol equals: 'protocol'.
		]

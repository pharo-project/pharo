tests
testProtocolAdditionWithProtocolRemoved

	| aClass |
	aClass := classFactory newClass.
	aClass organization addCategory: 'protocol'.
	self setHeadAsInputEntry.
	aClass removeProtocol: 'protocol'.

	self assertOutputsAnEventWith: [:output |
		self assert: output class equals: EpProtocolAddition.
		self assert: output protocol equals: 'protocol'.
		]

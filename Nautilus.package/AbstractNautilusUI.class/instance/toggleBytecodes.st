menus behavior
toggleBytecodes
	self selectedMethod ifNil: [ ^ self ].
	self
		updateSourceCodeAreaWith:
			(self sourceTextModel getString startsWithDigit
				ifTrue: [ self sourceTextModel getString ]
				ifFalse: [ self selectedMethod symbolic ])
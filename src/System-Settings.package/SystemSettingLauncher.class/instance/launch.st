accessing
launch
	self script 
			ifNotNil: [:s | s isSymbol 
				ifTrue: [s numArgs = 0 
					ifTrue: [self settingReceiver perform: s]
					ifFalse: [self settingReceiver perform: s with: self]]
				ifFalse: [s numArgs = 0 
					ifTrue: [s value]
					ifFalse: [s value: self]]]
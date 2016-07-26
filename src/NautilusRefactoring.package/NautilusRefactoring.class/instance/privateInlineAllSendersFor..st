private-method
privateInlineAllSendersFor: aMethod
	^ RBInlineAllSendersRefactoring
		model: environment
		sendersOf: aMethod selector
		in: aMethod methodClass
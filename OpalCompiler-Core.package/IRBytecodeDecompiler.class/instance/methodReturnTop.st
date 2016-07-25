instruction decoding
methodReturnTop
	irBuilder isLastClosureInstruction
		ifTrue: [
			self popScope.
			irBuilder fixPushNilsForTemps.
			irBuilder returnTop.
			irBuilder popScope. ]
		ifFalse: [ irBuilder returnTop ]
private-source
privateInlineMethodBetween: anInterval from: aMethod
	^ RBInlineMethodRefactoring
		model: environment
		inline: anInterval
		inMethod: aMethod selector
		forClass: aMethod methodClass
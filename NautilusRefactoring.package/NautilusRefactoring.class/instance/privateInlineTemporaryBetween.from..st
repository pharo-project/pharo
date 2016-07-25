private-source
privateInlineTemporaryBetween: anInterval from: aMethod
	^ RBInlineTemporaryRefactoring
		model: environment
		inline: (aMethod ast bestNodeFor: anInterval) sourceInterval
		from: aMethod selector
		in: aMethod methodClass
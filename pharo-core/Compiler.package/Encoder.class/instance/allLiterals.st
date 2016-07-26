results
allLiterals
	addedSelectorAndMethodClassLiterals ifFalse:
		[addedSelectorAndMethodClassLiterals := true.
		"Put the optimized selectors in literals so as to browse senders more easily"
		optimizedSelectors := optimizedSelectors reject: [:e| literalStream originalContents hasLiteral: e].
		optimizedSelectors isEmpty ifFalse: [
			"Use one entry per literal if enough room, else make anArray"
			literalStream position + optimizedSelectors size + 2 > 255
				ifTrue: [self litIndex: optimizedSelectors asArray]
				ifFalse: [optimizedSelectors do: [:e | self litIndex: e]]].
		"Add a slot for selector or MethodProperties"
		self litIndex: nil.
		self litIndex: self associationForClass].
	^literalStream contents
private
headingAndAutoselectForLiteral: aLiteral do: binaryBlock
	"Evaluate aBlock with either Users of ... or Senders of ... plus the auto-select string
	 for the given literal.  aLiteral can be a Symbol, a VariableBinding or an arbitrary object."

	| autoSelect |
	^aLiteral isSymbol
		ifTrue: [binaryBlock value: 'Senders of ', aLiteral value: aLiteral ]
		ifFalse:
			[autoSelect := aLiteral isVariableBinding
							ifTrue: [aLiteral key]
							ifFalse: [aLiteral printString].
			binaryBlock value: 'Users of ', autoSelect value: autoSelect]
initialize-release
initializePatternVariables
	| myname |
	myname := self name.
	isAnything := isList := isLiteral := isStatement := recurseInto := false.
	2 to: myname size
		do: 
			[:i | 
			| character |
			character := myname at: i.
			character == self listCharacter 
				ifTrue: [isAnything := isList := true]
				ifFalse: 
					[character == self literalCharacter 
						ifTrue: [isLiteral := true]
						ifFalse: 
							[character == self statementCharacter 
								ifTrue: [isStatement := true]
								ifFalse: 
									[character == self recurseIntoCharacter 
										ifTrue: [recurseInto := true]
										ifFalse: [^self]]]]]
separator
separatorAfterARow: aRow

	
	
	aRow ifNil: [ ^ false ].

	self separatorBlockOrSelector
		ifNotNil: [:blockOrSelector || anItem |
			anItem := getListElementSelector
						ifNil: [ list at: aRow ifAbsent: [ ^ false ]]
						ifNotNil: [ model perform: getListElementSelector with: aRow ].
						
			^ blockOrSelector isBlock
				ifTrue: [ blockOrSelector cull: anItem cull: aRow ]
				ifFalse: [
					blockOrSelector isSymbol
						ifTrue: [ blockOrSelector numArgs == 0
									ifTrue: [ anItem perform: blockOrSelector ]
									ifFalse: [ self model perform: blockOrSelector withEnoughArguments: { anItem. aRow} ]]
						ifFalse: [ false ]]].
	
	^ false
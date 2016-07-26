background coloring
backgroundColorFor: aRow

	| return |
	aRow ifNil: [ ^ nil ].
	self enabled
		ifFalse: [ return := Color white darker darker ].
	self backgroundColoringBlockOrSelector
		ifNotNil: [:blockOrSelector || anItem |
			anItem := getListElementSelector
						ifNil: [ list at: aRow ifAbsent: [ ^ nil ]]
						ifNotNil: [ model perform: getListElementSelector with: aRow ].

			return := blockOrSelector isBlock
				ifTrue: [ blockOrSelector cull: anItem cull: aRow ]
				ifFalse: [
					blockOrSelector isSymbol
						ifTrue: [ blockOrSelector numArgs == 0
									ifTrue: [ anItem perform: blockOrSelector ]
									ifFalse: [ self model perform: blockOrSelector withEnoughArguments: { anItem. aRow} ]]
						ifFalse: [ nil ]]].
		
		^ return isColor
			ifTrue: [ return ]
			ifFalse: [ nil ]
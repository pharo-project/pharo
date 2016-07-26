wrapping
wrapItem: anItem 
	"Here I return the display of my item, wrapped or not,
	using a block or a selector which will be performed
	directly on the item" 
	^ self wrapBlockOrSelector isBlock
		ifTrue: [wrapBlockOrSelector cull: anItem]
		ifFalse: [wrapBlockOrSelector isSymbol
				ifTrue: [| numArgs |
					numArgs := wrapBlockOrSelector numArgs.
					numArgs isZero
						ifTrue: [anItem perform: wrapBlockOrSelector]
						ifFalse: [numArgs = 1
								ifTrue: [anItem perform: wrapBlockOrSelector with: anItem]
								ifFalse: [self error: 'Wrong number of arguments']]]
				ifFalse: [anItem]]
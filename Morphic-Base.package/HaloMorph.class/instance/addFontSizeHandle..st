handles
addFontSizeHandle: haloSpec 
	(innerTarget isTextMorph) 
		ifTrue: 
			[self 
				addHandle: haloSpec
				on: #mouseDown
				send: #chooseFont
				to: innerTarget]
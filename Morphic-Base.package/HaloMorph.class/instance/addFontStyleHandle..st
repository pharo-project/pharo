handles
addFontStyleHandle: haloSpec 
	(innerTarget isTextMorph) 
		ifTrue: 
			[self 
				addHandle: haloSpec
				on: #mouseDown
				send: #chooseStyle
				to: innerTarget]
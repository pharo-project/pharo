handles
addFontEmphHandle: haloSpec 
	(innerTarget isTextMorph) 
		ifTrue: 
			[self 
				addHandle: haloSpec
				on: #mouseDown
				send: #chooseEmphasisOrAlignment
				to: innerTarget]
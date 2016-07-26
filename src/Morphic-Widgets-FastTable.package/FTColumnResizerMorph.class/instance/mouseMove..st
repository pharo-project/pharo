event handling
mouseMove: anEvent 
	self canResizeColumn ifFalse: [^ self].
	anEvent hand temporaryCursor ifNil: [^ self].
	traceMorph ifNil: [
		traceMorph := Morph newBounds: (self bounds withHeight: container height).
		traceMorph color: Smalltalk ui theme fastTableColumnResizingColor.
		traceMorph borderWidth: 0.
		container addMorph: traceMorph].
	traceMorph position: (anEvent cursorPoint x - lastMouse second x) @ traceMorph position y
event handling
mouseMove: anEvent 
	self canResizeColumn ifFalse: [^ self].
	anEvent hand temporaryCursor
		ifNil: [^ self].
	self class fastSplitterResize
		ifTrue: [traceMorph
				ifNil: [traceMorph := Morph newBounds: self bounds.
					traceMorph borderColor: Color lightGray.
					traceMorph borderWidth: 1.
					self owner addMorph: traceMorph].
			traceMorph position: (anEvent cursorPoint x - lastMouse second x) @ traceMorph position y]
		ifFalse: [self updateFromEvent: anEvent]
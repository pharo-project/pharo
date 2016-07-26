actions api
shiftPressed: aBoolean

	self isShiftPressed = aBoolean ifTrue: [ ^ self ].
	
	isShiftPressed := aBoolean.
	
	SmalltalkImage current vm canHandleShiftPressed
		ifTrue: [ ^ isShiftPressed := false ].

	self isShiftPressed
		ifTrue: [ self notifyRevealHints ]
		ifFalse: [ self notifyHideHints ]

	
scrollpane-testing
isKeyboardScrollEvent: anEvent

	(anEvent controlKeyPressed | anEvent commandKeyPressed) ifFalse: [^ false ].
	anEvent keyValue = 30 ifTrue: [ ^ true ].
	anEvent keyValue = 31 ifTrue: [ ^ true ].
	^ false
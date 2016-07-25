typing support
dispatchEnterOn: aKeyboardEvent return: aBlock
	aKeyboardEvent keyValue = Character cr asciiValue
		ifFalse: [ ^ false ].
	aKeyboardEvent controlKeyPressed
		ifTrue: [ aBlock value: (self normalCharacter: aKeyboardEvent) ].
	aKeyboardEvent shiftPressed
		ifTrue: [ aBlock value: (self lf: aKeyboardEvent) ].
	aKeyboardEvent commandKeyPressed
		ifTrue: [ aBlock value: (self crlf: aKeyboardEvent) ].
	aBlock value: (self crWithIndent: aKeyboardEvent)
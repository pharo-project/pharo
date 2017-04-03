primitives
indexOfAscii: anInteger inString: aString startingAt: start

	| stringSize |
	<primitive: 'primitiveIndexOfAsciiInString' module: 'MiscPrimitivePlugin'>
	<var: #aCharacter declareC: 'int anInteger'>
	<var: #aString declareC: 'unsigned char *aString'>

	stringSize := aString size.
	start to: stringSize do: [:pos |
		(aString basicAt: pos) = anInteger ifTrue: [^ pos]].

	^ 0
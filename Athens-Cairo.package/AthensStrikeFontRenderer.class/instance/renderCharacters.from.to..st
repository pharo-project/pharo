rendering
renderCharacters: aString from: start to: end
	| cairoText |
	aString isEmpty
		ifTrue: [ ^ self ].
	cairoText := self createCairoText: (aString copyFrom: start to: end).
	canvas paint loadOnCairoCanvas: canvas.
	canvas newPath.
	cairoText drawOn: canvas
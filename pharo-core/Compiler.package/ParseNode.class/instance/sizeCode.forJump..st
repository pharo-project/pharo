code generation
sizeCode: encoder forJump: dist

	^dist = 0 ifTrue: [0] ifFalse: [encoder sizeJump: dist]
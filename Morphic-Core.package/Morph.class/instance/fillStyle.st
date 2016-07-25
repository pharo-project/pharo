visual properties
fillStyle
	"Return the current fillStyle of the receiver."
	
	^extension
		ifNil: [^color]
		ifNotNil: [extension fillStyle ifNil: [color]]
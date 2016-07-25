accessing
typeSize
	"Answer a number of bytes, which receiver type takes in memory"
	self pointerArity > 0 ifTrue: [ ^ self pointerSize ].
	^ self externalTypeSize
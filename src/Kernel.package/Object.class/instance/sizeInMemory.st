memory usage
sizeInMemory
	"Answer the number of bytes consumed by this instance including object header."

	| contentBytes |
	self class isImmediateClass
		ifTrue: [ ^ 0 ].
	contentBytes := Smalltalk wordSize.	"base header"
	contentBytes := contentBytes + (self class instSize * Smalltalk wordSize).	"instance vars"
	self class isVariable
		ifTrue: [ | bytesPerElement |
			"indexed elements"
			bytesPerElement := self class isBytes
				ifTrue: [ 1 ]
				ifFalse: [ 4 ].
			contentBytes := contentBytes + (self basicSize * bytesPerElement).
			"If we are not filling an ammount of bytes multiple of the wordSize, we do it"
			contentBytes \\ Smalltalk wordSize = 0
				ifFalse: [ | extraBytesToFillAWord |
					extraBytesToFillAWord := Smalltalk wordSize - (contentBytes \\ Smalltalk wordSize).
					contentBytes := contentBytes + extraBytesToFillAWord ] ].
	contentBytes > 255
		ifTrue: [ contentBytes := contentBytes + (2 * Smalltalk wordSize) ]
		ifFalse: [ contentBytes := contentBytes + Smalltalk wordSize ].
	^ contentBytes
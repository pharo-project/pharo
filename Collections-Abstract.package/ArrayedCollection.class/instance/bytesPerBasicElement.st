objects from disk
bytesPerBasicElement
	"Answer the number of bytes that each of my basic elements requires.
	In other words:
		self basicSize * self bytesPerBasicElement
	should equal the space required on disk by my variable sized representation."
	^self class isBytes ifTrue: [ 1 ] ifFalse: [ 4 ]
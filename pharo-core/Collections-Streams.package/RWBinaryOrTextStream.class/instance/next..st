accessing
next: anInteger 
	"Answer the next anInteger elements of my collection. Must override to get class right."

	| newArray |
	newArray := (isBinary ifTrue: [ByteArray] ifFalse: [ByteString]) new: anInteger.
	^ self nextInto: newArray
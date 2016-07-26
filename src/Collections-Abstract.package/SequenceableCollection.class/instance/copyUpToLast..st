copying
copyUpToLast: anElement
	"Answer a copy of the receiver from index 1 to the last occurrence of 
	anElement, not including anElement."

	^ self first: (self lastIndexOf: anElement ifAbsent: [^ self copy]) - 1
testing
isPointerType
	"Return true if the receiver represents a pointer type"
	^self isStructureType not and:[self headerWord anyMask: FFIFlagPointer]
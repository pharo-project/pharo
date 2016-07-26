testing
isStructureType
	"Return true if the receiver represents a structure type"
	^self headerWord anyMask: FFIFlagStructure
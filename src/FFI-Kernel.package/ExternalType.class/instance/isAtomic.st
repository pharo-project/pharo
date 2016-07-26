testing
isAtomic
	"Return true if the receiver describes a built-in type"
	^self headerWord anyMask: FFIFlagAtomic
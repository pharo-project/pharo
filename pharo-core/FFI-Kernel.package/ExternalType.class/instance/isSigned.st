testing
isSigned
	"Return true if the receiver is a signed type.
	Note: Only useful for integer types."
	^self atomicType anyMask: 1
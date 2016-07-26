testing
isVoid
	"Return true if the receiver describes a plain 'void' type"
	^self isAtomic and:[self atomicType = 0]
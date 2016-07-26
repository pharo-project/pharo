testing
isIntegerType
	"Return true if the receiver is a built-in integer type"
	| type |
	type := self atomicType.
	^type > FFITypeBool and:[type <= FFITypeUnsignedLongLong]
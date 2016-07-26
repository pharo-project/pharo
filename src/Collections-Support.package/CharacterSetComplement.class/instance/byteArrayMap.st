private
byteArrayMap
	"return a ByteArray mapping each ascii value to a 1 if that ascii value is in the set, and a 0 if it isn't.  Intended for use by primitives only"

	^byteArrayMapCache ifNil: [byteArrayMapCache := absent byteArrayMap collect: [:i | 1 - i]]
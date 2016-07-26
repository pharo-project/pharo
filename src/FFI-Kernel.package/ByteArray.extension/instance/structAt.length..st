*FFI-Kernel
structAt: byteOffset length: length
	"Return a structure of the given length starting at the indicated byte offset."
	| value |
	value := ByteArray new: length.
	1 to: length do:[:i|
		value unsignedByteAt: i put: (self unsignedByteAt: byteOffset+i-1)].
	^value
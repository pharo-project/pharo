*FFI-Kernel
structAt: byteOffset put: value length: length
	"Store a structure of the given length starting at the indicated byte offset."
	1 to: length do:[:i|
		self unsignedByteAt: byteOffset+i-1 put: (value unsignedByteAt: i)].
	^value
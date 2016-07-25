*FFI-Kernel
unsignedLongLongAt: byteOffset put: value
	"I store 64-bit integers in Smalltalk (little-endian) order."
	self unsignedLongAt: byteOffset put: (value bitAnd: 16rFFFFFFFF).
	self unsignedLongAt: byteOffset+4 put: (value bitShift: -32).
	^value
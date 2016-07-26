*FFI-Kernel
longPointerAt: byteOffset
	"Answer an 8-byte pointer object stored at the given byte address"
	| addr |
	addr := ExternalAddress basicNew: 8.
	1 to: 8 do:
		[:i|
		addr basicAt: i put: (self unsignedByteAt: byteOffset+i-1)].
	^addr
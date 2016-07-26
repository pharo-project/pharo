*FFI-Kernel
shortPointerAt: byteOffset
	"Answer a 4-byte pointer object stored at the given byte address"
	| addr |
	addr := ExternalAddress basicNew: 4.
	1 to: 4 do:
		[:i|
		addr basicAt: i put: (self unsignedByteAt: byteOffset+i-1)].
	^addr
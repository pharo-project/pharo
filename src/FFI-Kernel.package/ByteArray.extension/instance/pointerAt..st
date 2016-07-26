*FFI-Kernel
pointerAt: byteOffset
	"Answer a pointer object stored at the given byte address"
	| addr |
	addr := ExternalAddress new.
	1 to: ExternalAddress wordSize do:
		[:i|
		addr basicAt: i put: (self unsignedByteAt: byteOffset+i-1)].
	^addr
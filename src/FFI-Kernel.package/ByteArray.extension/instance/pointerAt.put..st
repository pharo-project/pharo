*FFI-Kernel
pointerAt: byteOffset put: value
	"Store a pointer object at the given byte address"
	value isExternalAddress ifFalse:
		[^self error:'Only external addresses can be stored'].
	1 to: ExternalAddress wordSize do:
		[:i|
		self unsignedByteAt: byteOffset+i-1 put: (value basicAt: i)].
	^value
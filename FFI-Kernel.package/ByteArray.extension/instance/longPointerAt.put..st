*FFI-Kernel
longPointerAt: byteOffset put: value
	"Store an 8-byte pointer object at the given byte address"
	value isExternalAddress ifFalse:
		[^self error:'Only external addresses can be stored'].
	1 to: 8 do:
		[:i|
		self unsignedByteAt: byteOffset+i-1 put: (value basicAt: i)].
	^value
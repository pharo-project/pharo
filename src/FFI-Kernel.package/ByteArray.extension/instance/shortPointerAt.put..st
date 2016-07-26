*FFI-Kernel
shortPointerAt: byteOffset put: value
	"Store a 4-byte pointer object at the given byte address"
	value isExternalAddress ifFalse:
		[^self error:'Only external addresses can be stored'].
	1 to: 4 do:
		[:i|
		self unsignedByteAt: byteOffset+i-1 put: (value basicAt: i)].
	^value
*FFI-Kernel
signedLongLongAt: byteOffset put: value
	self unsignedLongLongAt: byteOffset put: (value < 0
		ifTrue: [ value + 16r10000000000000000 ]
		ifFalse: [ value ])
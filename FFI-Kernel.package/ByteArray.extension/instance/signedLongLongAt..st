*FFI-Kernel
signedLongLongAt: byteOffset
	| int |
	int := self unsignedLongLongAt: byteOffset.
	int > 16r7FFFFFFFFFFFFFFF ifTrue: [^int - 16r10000000000000000].
	^int
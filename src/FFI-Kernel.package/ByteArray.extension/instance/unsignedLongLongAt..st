*FFI-Kernel
unsignedLongLongAt: byteOffset
	"Answer a 64-bit integer in Smalltalk order (little-endian)."
	| n1 n2 |
	n1 := self unsignedLongAt: byteOffset.
	n2 := self unsignedLongAt: byteOffset+4.
	^n2 = 0 ifTrue: [n1] ifFalse: [(n2 bitShift: 32) + n1]
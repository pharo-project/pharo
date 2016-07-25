*FFI-Kernel
booleanAt: byteOffset put: value
	"Booleans are just integers in C word"
	^self integerAt: byteOffset put: (value ifTrue:[1] ifFalse:[0]) size: 1 signed: false
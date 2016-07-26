*FFI-Kernel
signedCharAt: byteOffset put: aCharacter
	^self unsignedByteAt: byteOffset put: aCharacter asciiValue
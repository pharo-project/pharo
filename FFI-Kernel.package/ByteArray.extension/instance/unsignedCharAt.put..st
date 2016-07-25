*FFI-Kernel
unsignedCharAt: byteOffset put: aCharacter
	^self unsignedByteAt: byteOffset put: aCharacter asciiValue
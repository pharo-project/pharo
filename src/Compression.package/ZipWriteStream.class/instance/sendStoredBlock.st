stored blocks
sendStoredBlock
	"Send an uncompressed block"
	| inBytes |
	inBytes := blockPosition - blockStart.
	encoder flushBits. "Skip to byte boundary"
	encoder nextBits: 16 put: inBytes.
	encoder nextBits: 16 put: (inBytes bitXor: 16rFFFF).
	encoder flushBits.
	1 to: inBytes do:[:i|
		encoder nextBytePut: (collection byteAt: blockStart+i)].
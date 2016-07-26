bit manipulation
bitInvert32
	"Answer the 32-bit complement of the receiver."

	^ self bitXor: 16rFFFFFFFF
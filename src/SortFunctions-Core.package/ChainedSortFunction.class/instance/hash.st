comparing
hash
	"Answer an integer value that is related to the identity of the receiver."

	^ direction hash
		bitXor: (startFunction hash bitXor: nextFunction hash)
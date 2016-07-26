comparing
hash
	"Answer an integer value that is related to the identity of the receiver."

	^ username hash bitXor: (password hash bitXor: enabled hash)
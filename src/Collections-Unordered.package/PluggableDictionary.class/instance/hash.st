comparing
hash
	"hashBlock is used to hash keys for lookup, not the dictionary itself, but its hash is still
	considered."
	^ (super hash bitXor: self equalBlock hash) bitXor: self hashBlock hash
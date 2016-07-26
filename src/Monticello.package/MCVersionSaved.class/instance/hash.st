comparing
hash

	^ (self class hash
		bitXor: self version hash)
		bitXor: self repository hash
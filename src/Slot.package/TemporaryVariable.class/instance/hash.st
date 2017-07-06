comparing
hash

	^ (name hash
		bitXor: method hash)
		bitXor: (startpc ifNil: [0])
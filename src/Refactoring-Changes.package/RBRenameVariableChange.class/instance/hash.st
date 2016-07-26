comparing
hash
	^ (self class hash bitXor: self oldName hash) bitXor: self newName hash
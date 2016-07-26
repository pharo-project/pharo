comparing
hash
	^ (self name hash bitXor: self path hash) bitXor: self domain hash
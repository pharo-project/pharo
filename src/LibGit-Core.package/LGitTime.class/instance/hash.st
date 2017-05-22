comparing
hash
	^ (super hash bitXor: self time) bitXor: self offset
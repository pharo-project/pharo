comparing
hash
	^ (self receiver hash bitXor: self selector hash) bitXor: (self hashForCollection: self arguments)
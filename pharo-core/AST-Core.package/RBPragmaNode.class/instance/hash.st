comparing
hash
	^ self selector hash bitXor: (self hashForCollection: self arguments)
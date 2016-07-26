comparing
hash
	^ ((self selector hash bitXor: (self hashForCollection: self arguments)) bitXor: (self hashForCollection: self pragmas)) bitXor: self body hash
comparing
hash
	^ (self hashForCollection: self temporaries) bitXor: (self hashForCollection: self statements)
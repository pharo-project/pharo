comparing
hash
	^ ((self name hash bitXor: self email hash) bitXor: self email hash) bitXor: self dateAndTime hash
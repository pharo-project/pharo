comparing
hash
	^ (self method hash bitXor: self uri hash) bitXor: self version hash
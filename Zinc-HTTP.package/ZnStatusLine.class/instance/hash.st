comparing
hash
	^ (self code hash bitXor: self version hash) bitXor: self reason hash
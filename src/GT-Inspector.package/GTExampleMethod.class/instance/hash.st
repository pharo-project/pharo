public
hash
	^ (self class hash 
		bitXor: self provider hash)
			bitXor: self selector hash
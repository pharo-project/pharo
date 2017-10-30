comparing
hash
	^ (self species hash 
		bitXor: super hash)
		bitXor: self categories hash
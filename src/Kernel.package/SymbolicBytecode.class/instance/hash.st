comparing
hash

	^ ((self bytes hash bitXor: self description hash) bitXor: 
			self method hash) bitXor: self offset hash 
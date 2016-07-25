comparing
hash
	^ (self species hash bitXor: self localName hash) bitXor: self globalName hash
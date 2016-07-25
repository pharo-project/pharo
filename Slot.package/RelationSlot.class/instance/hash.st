comparing
hash
	^ ((self species hash bitXor: self name hash) 
			bitXor: (self index ifNil: [ 0 ])) 
				bitXor: self targetClassName hash
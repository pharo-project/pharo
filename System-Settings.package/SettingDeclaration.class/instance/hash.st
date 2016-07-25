comparing
hash
	"hash is re-implemented because #= is re-implemented"
	^ self target hash
		bitXor: (self targetSelector hash
				bitXor: (self getSelector hash bitXor: self setSelector hash)) 
comparing
hash
	"Hash is re-implemented because #= is re-implemented"
	
	^self class hash bitXor: (self name hash)
comparing
hash
	"Hash is implemented because #= is implemented."
	
	^self morph hash
		bitXor: (self state hash
		bitXor: (self icon hash
		bitXor: self label hash))
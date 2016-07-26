comparing
hash
	"Hash is implemented because #= is implemented."
	
	^self species hash
		bitXor: (self origin hash
		bitXor: (self direction hash
		bitXor: (self normal hash)))
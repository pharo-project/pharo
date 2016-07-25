comparing
hash
	"hash is implemented because #= is implemented"
	^self species hash bitXor: (self width hash bitXor: self color hash)
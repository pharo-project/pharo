comparing
hash
	"Hash is re-implemented because #= is re-implemented"
	^colors hash bitXor: (shifts hash bitXor: masks hash)
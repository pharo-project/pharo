comparing
hash
	| hash |
	hash := self class identityHash.
	1 to: self size do:
		[:i | hash := String stringHash: (self at: i) initialHash: hash].
	^ hash
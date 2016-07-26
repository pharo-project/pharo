comparing
hash
	| hash |
	hash := String stringHash: name initialHash: 0.
	hash := String stringHash: self traitCompositionString initialHash: hash.
	hash := String stringHash: (category ifNil: ['']) initialHash: hash.
	^ hash

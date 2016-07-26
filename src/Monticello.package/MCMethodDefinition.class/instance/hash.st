comparing
hash
	| hash |
	hash := String stringHash: classIsMeta asString initialHash: 0.
	hash := String stringHash: source initialHash: hash.
	hash := String stringHash: category initialHash: hash.
	hash := String stringHash: className initialHash: hash.
	^ hash
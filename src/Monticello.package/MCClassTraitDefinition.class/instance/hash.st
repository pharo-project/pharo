accessing
hash
	| hash |
	hash := String stringHash: baseTrait initialHash: 0.
	hash := String stringHash: self classTraitCompositionString initialHash: hash.
	^hash

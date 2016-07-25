comparing
hash
	| hash |
	hash := String stringHash: name initialHash: 0.
	hash := String stringHash: superclassName initialHash: hash.
	hash := String stringHash: self traitCompositionString initialHash: hash.
	hash := String stringHash: self classTraitComposition asString initialHash: hash.
	hash := String stringHash: (category ifNil: ['']) initialHash: hash.
	hash := String stringHash: type initialHash: hash.
	variables do: [
		:v |
		hash := String stringHash: v name initialHash: hash.
	].
	^ hash
testing
isNilIf

	^(special between: 3 and: 4)
	   and: [(arguments first returns or: [arguments first isJust: NodeNil])
	   and: [(arguments last returns or: [arguments last isJust: NodeNil])]]
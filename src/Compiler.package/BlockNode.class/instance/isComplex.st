testing
isComplex

	^statements size > 1 or: [statements size = 1 and: [statements first isComplex]]
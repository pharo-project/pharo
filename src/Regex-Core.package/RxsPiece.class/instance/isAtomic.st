testing
isAtomic
	"A piece is atomic if only it contains exactly one atom
	which is atomic (sic)."

	^self isSingular and: [atom isAtomic]
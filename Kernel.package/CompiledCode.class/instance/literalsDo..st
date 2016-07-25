literals
literalsDo: aBlock
	"Evaluate aBlock for each of the literals referenced by the receiver."
	1 to: self numLiterals do:
		[:index |
		aBlock value: (self objectAt: index + 1)]
comparing
= anOrientedFillStyle
	"Answer whether equal."

	^self species = anOrientedFillStyle species
		and: [self origin = anOrientedFillStyle origin
		and: [self direction = anOrientedFillStyle direction
		and: [self normal = anOrientedFillStyle normal]]]
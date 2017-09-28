initialization
initializeBuilder: aBuilder
	super initializeBuilder: aBuilder.
	aBuilder propertyAt: #traitComposition put: nil.
	aBuilder propertyAt: #classTraitComposition put: nil	
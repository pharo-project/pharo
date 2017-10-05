initialization
initializeBuilder: aBuilder
	super initializeBuilder: aBuilder.
	aBuilder propertyAt: #traitComposition put: nil.
	aBuilder propertyAt: #classTraitComposition put: nil	.
	
	aBuilder addChangeComparer: (ShTraitCompositionChanged new enhancer: self; yourself).
	aBuilder addChangeComparer: (ShClassTraitCompositionChanged new enhancer: self; yourself).	
class modifications
afterMethodsCompiled: aBuilder
	super afterMethodsCompiled: aBuilder.
	
	(aBuilder propertyAt: #traitComposition) ifNotNil:[ 
		self
			installTraitComposition: (self traitCompositionOf: aBuilder)
			on: aBuilder newClass
			from: aBuilder oldClass].

	(aBuilder propertyAt: #classTraitComposition) ifNotNil:[ 
		self
			installTraitComposition: (self classTraitCompositionOf: aBuilder)
			on: aBuilder newMetaclass
			from: aBuilder oldMetaclass].
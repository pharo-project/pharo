class modifications
afterMethodsCompiled: aBuilder
	super afterMethodsCompiled: aBuilder.
	
	self
			installTraitComposition: (self traitCompositionOf: aBuilder)
			on: aBuilder newClass
			from: aBuilder oldClass.

	self
			installTraitComposition: (self classTraitCompositionOf: aBuilder)
			on: aBuilder newMetaclass
			from: aBuilder oldMetaclass.
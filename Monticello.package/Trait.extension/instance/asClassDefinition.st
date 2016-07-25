*Monticello
asClassDefinition
	^ MCTraitDefinition
		name: self name
		traitComposition: self traitCompositionString
		category: self category 
		comment: self organization classComment asString
		commentStamp: self organization commentStamp.
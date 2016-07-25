comparing
= aDefinition
	^(super = aDefinition)
		and: [superclassName = aDefinition superclassName
		and: [self traitCompositionString = aDefinition traitCompositionString
		and: [self classTraitCompositionString = aDefinition classTraitCompositionString
		and: [category = aDefinition category
		and: [type = aDefinition type
		and: [self sortedVariables = aDefinition sortedVariables
		and: [comment = aDefinition comment]]]]]]]
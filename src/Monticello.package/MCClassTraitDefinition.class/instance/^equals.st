comparing
= aDefinition
	^ (super = aDefinition)
		and: [baseTrait = aDefinition baseTrait
		and: [self classTraitCompositionString = aDefinition classTraitCompositionString]]


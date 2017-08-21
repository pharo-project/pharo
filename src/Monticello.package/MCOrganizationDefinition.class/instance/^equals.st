comparing
= aDefinition

	^ (super = aDefinition)
		and: [ self categories size = aDefinition categories size and: [ self categories includesAll: aDefinition categories ] ].
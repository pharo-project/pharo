*Reflectivity
variableNodes
		^self allChildren select: [:each | 
			each isVariable and: [each isDefinition not]].
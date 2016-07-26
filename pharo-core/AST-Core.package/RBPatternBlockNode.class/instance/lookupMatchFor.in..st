matching
lookupMatchFor: aString in: aDictionary 
	^aDictionary at: aString
		ifAbsent: 
			[| variableNode |
			variableNode := RBPatternVariableNode named: aString.
			aDictionary at: variableNode ifAbsent: [nil]]
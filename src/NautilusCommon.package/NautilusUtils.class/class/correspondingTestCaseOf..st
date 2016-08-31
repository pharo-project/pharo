utilities
correspondingTestCaseOf: aClassOrClassDefinition
	^ self environment
		at:
			(aClassOrClassDefinition theNonMetaClass name asString , 'Test') asSymbol
		ifAbsent: [ nil ]
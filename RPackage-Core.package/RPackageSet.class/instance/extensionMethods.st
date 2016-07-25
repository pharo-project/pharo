accessing
extensionMethods
	^extensionMethods ifNil: [
		extensionMethods := ((self collectFromAllPackages: #extensionMethods) collect: #asRingDefinition) asSet asArray ]
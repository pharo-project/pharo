system compatibility
systemCategories
	^systemCategories ifNil: [ systemCategories := (self collectFromAllPackages: #systemCategories) asSet asArray].
	
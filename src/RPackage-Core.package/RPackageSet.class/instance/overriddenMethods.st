system compatibility
overriddenMethods
	^overriddenMethods ifNil: [ overriddenMethods := self collectFromAllPackages: #overriddenMethods ]
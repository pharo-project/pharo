accessing
definedClasses
	^definedClasses ifNil: [ definedClasses := self collectFromAllPackages: #definedClasses ]
*Fuel
fuelAccept: aGeneralMapper

	^ self shouldBeSubstitutedByCleanCopy
		"The 'onRecursionDo:' is just to avoid an infinitive loop for the substitution. The cleanCopy MUST be a clean copy so it can be serialized normally"
		ifTrue: [ aGeneralMapper visitSubstitution: self by: self cleanCopy onRecursionDo: [ aGeneralMapper visitVariableObject: self ]  ]
		ifFalse: [ aGeneralMapper visitVariableObject: self ]
accessing
newMethods
	^newMethods isNil 
		ifTrue: [newMethods := IdentityDictionary new]
		ifFalse: [newMethods]
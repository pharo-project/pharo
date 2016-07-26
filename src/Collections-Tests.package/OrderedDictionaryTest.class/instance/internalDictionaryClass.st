accessing
internalDictionaryClass
	self isTestingIdentityDictionary
		ifTrue: [^ IdentityDictionary]
		ifFalse: [^ Dictionary]
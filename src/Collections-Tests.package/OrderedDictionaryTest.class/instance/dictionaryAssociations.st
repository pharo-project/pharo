accessing
dictionaryAssociations
	self isTestingIdentityDictionary
		ifTrue: [^ self identityDictionaryAssociations]
		ifFalse: [^ self nonIdentityDictionaryAssociations]
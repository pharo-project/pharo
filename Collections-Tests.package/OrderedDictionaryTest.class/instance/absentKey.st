accessing
absentKey
	self isTestingIdentityDictionary
		ifTrue: [^ self identityAbsentKey]
		ifFalse: [^ self nonIdentityAbsentKey]
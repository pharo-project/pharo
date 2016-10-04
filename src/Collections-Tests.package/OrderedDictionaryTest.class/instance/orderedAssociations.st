accessing
orderedAssociations
	self isTestingIdentityDictionary
		ifTrue: [^ self identityOrderedAssociations]
		ifFalse: [^ self nonIdentityOrderedAssociations]
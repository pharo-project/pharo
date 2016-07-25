public
propertyAt: aKey put: aValue
	^ (properties ifNil: [ properties := IdentityDictionary new ])
		at: aKey put: aValue
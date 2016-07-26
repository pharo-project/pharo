public
propertyAt: aKey ifAbsentPut: aBlock
	^ (properties ifNil: [ properties := IdentityDictionary new ])
		at: aKey ifAbsentPut: aBlock
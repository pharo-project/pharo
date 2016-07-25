initialization
initialize

	TraitProperties
		ifNil: [ TraitProperties := WeakIdentityKeyDictionary new ]
		ifNotNil: [ | newDict | 
			newDict := WeakIdentityKeyDictionary newFrom: TraitProperties.
			newDict rehash.
			TraitProperties := newDict ]
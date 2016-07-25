class initialization
initialize
	Properties
		ifNil: [ self initializeProperties ]
		ifNotNil: [ | newDict | 
			newDict := WeakIdentityKeyDictionary newFrom: Properties.
			newDict rehash.
			Properties := newDict ].
	PersistentTemps ifNil: [ PersistentTemps := Set new]
		
initialization
initialize

	super initialize.
	MethodsIconsCache ifNil: [ MethodsIconsCache := WeakIdentityKeyDictionary new ].
	methodsSelection := OrderedCollection new.
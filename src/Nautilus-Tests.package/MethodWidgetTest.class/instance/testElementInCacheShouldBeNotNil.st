tests
testElementInCacheShouldBeNotNil

	MethodWidget resetMethodIconCache.
	self assert: (MethodWidget new methodIconFor: (self class >> #foo)) target notNil.
	Smalltalk garbageCollect.
	self assert: (MethodWidget new methodIconFor: (self class >> #foo)) target notNil.
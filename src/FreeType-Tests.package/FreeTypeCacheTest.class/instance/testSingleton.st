tests
testSingleton
	self assert: FreeTypeCache current class = FreeTypeCache.
	self assert: FreeTypeCache current ==  FreeTypeCache current.
	
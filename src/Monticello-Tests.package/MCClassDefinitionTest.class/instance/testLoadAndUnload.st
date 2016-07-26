tests
testLoadAndUnload
	| d c |
	d := self mockClass: 'MCMockClassC' super: 'Object'.
	d load.
	self assert: (Smalltalk hasClassNamed: 'MCMockClassC').
	c := Smalltalk globals classNamed: 'MCMockClassC'.
	self assert: (c isKindOf: Class).
	self assert: c superclass = Object.
	self assert: c instVarNames isEmpty.
	self assert: c classVarNames isEmpty.
	self assert: c sharedPools isEmpty.
	self assert: c category = self mockCategoryName.
	self assert: c organization classComment = (self commentForClass: 'MCMockClassC').
	self assert: c organization commentStamp = (self commentStampForClass: 'MCMockClassC').
	d unload.
	self deny: (Smalltalk hasClassNamed: 'MCMockClassC')
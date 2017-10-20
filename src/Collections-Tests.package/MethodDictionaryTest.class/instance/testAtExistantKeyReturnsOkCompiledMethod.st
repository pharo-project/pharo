tests - others
testAtExistantKeyReturnsOkCompiledMethod
	| methodSelector method |
	methodSelector := #testAssociationAtExistantKeyReturnsOkAssociation.
	method := self class methodDict at: methodSelector ifAbsent: [self error].
	
	self assert: method class = CompiledMethod.
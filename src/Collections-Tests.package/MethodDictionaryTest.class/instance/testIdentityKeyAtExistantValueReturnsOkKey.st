tests - others
testIdentityKeyAtExistantValueReturnsOkKey
	| methodSelector result aMethod |
	methodSelector := #testAssociationAtExistantKeyReturnsOkAssociation.
	
	aMethod := self class >> methodSelector.
	result := self class methodDict keyAtIdentityValue: aMethod ifAbsent: [self error].
	
	self assert: result = methodSelector.
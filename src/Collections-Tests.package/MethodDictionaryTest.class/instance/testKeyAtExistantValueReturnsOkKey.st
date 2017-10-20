tests - others
testKeyAtExistantValueReturnsOkKey
	| methodSelector result aMethod |
	methodSelector := #testAssociationAtExistantKeyReturnsOkAssociation.
	aMethod := self class >> methodSelector.
	result := self class methodDict keyAtValue: aMethod ifAbsent: [self error].
	
	self assert: result = methodSelector.
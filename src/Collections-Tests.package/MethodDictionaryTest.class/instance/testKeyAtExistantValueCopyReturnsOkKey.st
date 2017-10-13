tests - others
testKeyAtExistantValueCopyReturnsOkKey
	| methodSelector result aMethod |
	methodSelector := #testAssociationAtExistantKeyReturnsOkAssociation.
	aMethod := (self class >> methodSelector) copy.
	result := self class methodDict keyAtValue: aMethod ifAbsent: [self error].
	
	self assert: result = methodSelector.
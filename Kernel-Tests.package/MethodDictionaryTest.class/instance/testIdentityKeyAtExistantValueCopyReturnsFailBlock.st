tests - others
testIdentityKeyAtExistantValueCopyReturnsFailBlock
	| methodSelector result aMethod error |
	methodSelector := #testAssociationAtExistantKeyReturnsOkAssociation.
	error := #error.
	
	aMethod := (self class >> methodSelector) copy.
	result := self class methodDict keyAtIdentityValue: aMethod ifAbsent: [error].
	
	self assert: result = error.
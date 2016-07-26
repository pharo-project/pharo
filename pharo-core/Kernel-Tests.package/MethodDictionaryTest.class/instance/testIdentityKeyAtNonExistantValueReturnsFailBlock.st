tests - others
testIdentityKeyAtNonExistantValueReturnsFailBlock
	| methodSelector result aMethod error |
	methodSelector := #testAssociationAtExistantKeyReturnsOkAssociation.
	error := #error.
	
	result := self class methodDict keyAtIdentityValue: self ifAbsent: [error].
	
	self assert: result = error.
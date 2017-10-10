tests - others
testAssociationAtExistantKeyReturnsOkAssociation
	| methodSelector association |
	methodSelector := #testAssociationAtExistantKeyReturnsOkAssociation.
	association := self class methodDict associationAt: methodSelector ifAbsent: [self error].
	
	self assert: association key = methodSelector.
	self assert: association value = (self class >> methodSelector).
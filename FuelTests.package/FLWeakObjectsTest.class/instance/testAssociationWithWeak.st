tests
testAssociationWithWeak
	"This tests an association with weak values. InstVar1 is a fixed instance variable, hence they are strong and hence should not be replaed by nil. Instead, 'weak' is stored in the variable part of FLWeakClassMock, so it is weak and so it should have been replaced by nil."
	| weak association materializedAssociation |
	
	weak := FLWeakClassMock new: 1.
	weak instVar1: 'nonWeak'.
	weak basicAt: 1 put: 'weak'. 
	association := Association key: #foo value: weak.
	
	materializedAssociation := self resultOfSerializeAndMaterialize: association.
	self assert: materializedAssociation value instVar1 isNil not.
	self assert: (materializedAssociation value basicAt: 1) isNil.
	self assert: materializedAssociation key = #foo.
	
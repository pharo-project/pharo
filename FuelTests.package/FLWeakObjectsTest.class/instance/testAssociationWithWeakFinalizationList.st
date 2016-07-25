tests
testAssociationWithWeakFinalizationList
	"This tests an association with WeakFInalizationList as its value."
	| weak association materializedAssociation object list |
	
	object := Object new.
	list := WeakFinalizationList new.
	weak := WeakFinalizerItem new list: list object: object executor: nil.
	association := Association key: #foo value: weak.
	
	materializedAssociation := self resultOfSerializeAndMaterialize: association.
	"Both, 'executor' and 'list' are fixed instance variables, hence they are strong and hence should not be replaced by nil. Instead, 'object' is stored in the variable part of WeakFinalizationItem, so it is weak and so it should have been replaced by nil."
	self assert: materializedAssociation value executor isNil.
	self assert: materializedAssociation value list isNil not.
	self assert: materializedAssociation value object isNil.
	self assert: materializedAssociation key = #foo.
	